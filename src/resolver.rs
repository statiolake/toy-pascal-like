use crate::hir::*;
use crate::hir_visit;
use crate::hir_visit::VisitMut;
use crate::span::Span;
use std::collections::HashMap;

#[derive(thiserror::Error, Debug)]
#[error("{span}: {kind}")]
pub struct ResolverError {
    span: Span,
    kind: ResolverErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum ResolverErrorKind {
    #[error("undeclared function: {ident}")]
    UndeclaredFunction { ident: String },

    #[error("undeclared variable: {ident}")]
    UndeclaredVariable { ident: String },

    #[error("unknown type: {ident}")]
    UnknownTy { ident: String },

    #[error("could not determine the type")]
    InferFailure,

    #[error("type mismatch: expected {expected} but found {found}")]
    TyMismatch { expected: TyKind, found: TyKind },
}

impl ResolverErrorKind {
    pub fn summary(&self) -> String {
        match self {
            ResolverErrorKind::UndeclaredFunction { .. } => "unknown function name".to_string(),
            ResolverErrorKind::UndeclaredVariable { .. } => "unknown variable name".to_string(),
            ResolverErrorKind::UnknownTy { .. } => "unknown type".to_string(),
            ResolverErrorKind::InferFailure => "could not infer the type of this".to_string(),
            ResolverErrorKind::TyMismatch { expected, found } => {
                format!("expected: {}, found: {}", expected, found)
            }
        }
    }
}

pub type Result<T, E = ResolverError> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct Resolver {
    prog: Program,
}

impl Resolver {
    pub fn from_program(prog: Program) -> Self {
        Self { prog }
    }

    pub fn resolve(self) -> Result<Program> {
        self.resolve_all()?;
        self.validate();

        Ok(self.prog)
    }
}

macro_rules! unwrap_resolved {
    ($res:expr) => {
        match $res {
            ResolveStatus::Resolved(v) => v,
            ResolveStatus::Unresolved(ident) => {
                panic!("internal error: type {} is not resolved yet?")
            }
            ResolveStatus::Unknown => panic!("internal error: type is not inferred yet?"),
        }
    };
}

macro_rules! stop_if_err {
    ($err:expr) => {
        if $err.is_some() {
            return;
        }
    };
}

macro_rules! return_if_err {
    ($self:expr, $res:expr) => {
        if let Err(err) = $res {
            $self.err = Some(err);
            return;
        }
    };
}

macro_rules! return_err {
    ($self:expr, $err:expr) => {{
        $self.err = Some($err);
        return;
    }};
}

impl Resolver {
    fn resolve_all(&mut self) -> Result<()> {
        let mut visitor = ResolverVisitor {
            prog: &self.prog,
            locals: HashMap::new(),
            err: None,
        };
        visitor.visit_all(&mut self.prog);
        return match visitor.err {
            None => Ok(()),
            Some(err) => Err(err),
        };

        struct ResolverVisitor<'hir> {
            prog: &'hir Program,
            locals: HashMap<String, (Span, TyKind)>,
            err: Option<ResolverError>,
        }

        fn resolve_primitive(ty: &mut Ty) -> Result<()> {
            if let ResolveStatus::Unresolved(name) = ty.res {
                match &*name.ident {
                    "int" => ty.res = ResolveStatus::Resolved(TyKind::Int),
                    "float" => ty.res = ResolveStatus::Resolved(TyKind::Float),
                    _ => {
                        return Err(ResolverError {
                            span: ty.span,
                            kind: ResolverErrorKind::UnknownTy {
                                ident: name.ident.clone(),
                            },
                        })
                    }
                }
            }

            Ok(())
        }

        impl VisitMut for ResolverVisitor<'_> {
            fn visit_fndecl(&mut self, fndecl: &mut FnDecl) {
                stop_if_err!(self.err);
                hir_visit::visit_fndecl(self, fndecl);
                stop_if_err!(self.err);
                return_if_err!(self, resolve_primitive(&mut fndecl.ret_ty));
            }

            fn visit_fnbody(&mut self, fnbody: &mut FnBody) {
                stop_if_err!(self.err);
                // set up new local variable tables
                self.locals = HashMap::new();
                hir_visit::visit_fnbody(self, fnbody);
                stop_if_err!(self.err);
            }

            fn visit_param(&mut self, param: &mut Param) {
                stop_if_err!(self.err);
                hir_visit::visit_param(self, param);
                stop_if_err!(self.err);
                return_if_err!(self, resolve_primitive(&mut param.ty));
            }

            fn visit_assg_stmt(&mut self, stmt: &mut AssgStmt) {
                stop_if_err!(self.err);
                // You can't use hir_visit::visit_assg_stmt(), because you need to declare the
                // variable if it's not declared. But to infer the type of declared variable, you
                // need to first check type of the assigning expr.
                self.visit_arith_expr(&mut stmt.expr);
                stop_if_err!(self.err);

                let ty_expr = unwrap_resolved!(stmt.expr.ty.res);
                let (span, ty_var) = self
                    .locals
                    .entry(stmt.var.name.ident.clone())
                    .or_insert_with(|| (stmt.var.span, ty_expr));
                let ty_var = ty_var.clone();

                if ty_var != ty_expr {
                    return_err!(
                        self,
                        ResolverError {
                            span: stmt.expr.span,
                            kind: ResolverErrorKind::TyMismatch {
                                expected: ty_var,
                                found: ty_expr,
                            }
                        }
                    );
                }
            }

            fn visit_arith_expr(&mut self, expr: &mut ArithExpr) {
                stop_if_err!(self.err);
                hir_visit::visit_arith_expr(self, expr);
                stop_if_err!(self.err);
                match &expr.kind {
                    ArithExprKind::Primary(e) => {
                        expr.ty.res = ResolveStatus::Resolved(unwrap_resolved!(e.ty.res));
                    }
                    ArithExprKind::UnaryOp(_, e) => {
                        expr.ty.res = ResolveStatus::Resolved(unwrap_resolved!(e.ty.res));
                    }
                    ArithExprKind::BinOp(_, l, r) => {
                        let lt = unwrap_resolved!(l.ty.res);
                        let rt = unwrap_resolved!(r.ty.res);

                        if lt != rt {
                            return_err!(
                                self,
                                ResolverError {
                                    span: r.span,
                                    kind: ResolverErrorKind::TyMismatch {
                                        expected: lt,
                                        found: rt,
                                    },
                                }
                            );
                        }
                    }
                }
            }

            fn visit_primary_expr(&mut self, expr: &mut PrimaryExpr) {
                stop_if_err!(self.err);
                hir_visit::visit_primary_expr(self, expr);
                stop_if_err!(self.err);

                match expr.kind {
                    PrimaryExprKind::Var(v) => {
                        expr.ty.res = ResolveStatus::Resolved(unwrap_resolved!(v.ty.res));
                    }
                    PrimaryExprKind::Const(c) => {
                        expr.ty.res = ResolveStatus::Resolved(unwrap_resolved!(c.ty.res));
                    }
                    PrimaryExprKind::FnCall(fc) => {
                        let id = unwrap_resolved!(fc.res);
                        let fndecl = self.prog.fndecl(id);
                        expr.ty.res = ResolveStatus::Resolved(unwrap_resolved!(fndecl.ret_ty.res));
                    }
                    PrimaryExprKind::Paren(e) => {
                        expr.ty.res = ResolveStatus::Resolved(unwrap_resolved!(e.ty.res));
                    }
                }
            }

            fn visit_var(&mut self, var: &mut Var) {
                stop_if_err!(self.err);
                hir_visit::visit_var(self, var);
                stop_if_err!(self.err);

                match self.locals.get(&var.name.ident) {
                    Some((_, res)) => {
                        var.ty.res = ResolveStatus::Resolved(res.clone());
                    }
                    None => {
                        return_err!(
                            self,
                            ResolverError {
                                span: var.span,
                                kind: ResolverErrorKind::UndeclaredVariable {
                                    ident: var.name.ident.clone()
                                }
                            }
                        );
                    }
                }
            }

            fn visit_fncall(&mut self, fncall: &mut FnCall) {
                stop_if_err!(self.err);
                hir_visit::visit_fncall(self, fncall);
                stop_if_err!(self.err);

                if let ResolveStatus::Unresolved(name) = fncall.res {
                    let fndecl = self
                        .prog
                        .fndecls
                        .values()
                        .find(|fndecl| fndecl.name.ident == name.ident);
                    let fndecl = match fndecl {
                        Some(fndecl) => fndecl,
                        None => {
                            return_err!(
                                self,
                                ResolverError {
                                    span: fncall.span_name,
                                    kind: ResolverErrorKind::UndeclaredFunction {
                                        ident: name.ident.clone(),
                                    }
                                }
                            );
                        }
                    };
                    fncall.res = ResolveStatus::Resolved(fndecl.id);
                }
            }
        }
    }

    fn validate(&mut self) {
        let mut visitor = ValidatorVisitor;
        visitor.visit_all(&mut self.prog);

        struct ValidatorVisitor;

        // FIXME: It doesn't have to be mut but use it until we prepare non-mut version of Visitor.
        impl VisitMut for ValidatorVisitor {}
    }
}
