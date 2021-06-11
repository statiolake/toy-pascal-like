use crate::hir::*;
use crate::hir_visit;
use crate::hir_visit::Visit;
use crate::span::Span;
use std::collections::BTreeMap;

#[derive(thiserror::Error, Debug)]
#[error("{span}: {kind}")]
pub struct TypeckError {
    pub span: Span,
    pub kind: TypeckErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum TypeckErrorKind {
    #[error("could not determine the type")]
    InferFailure,

    #[error("type mismatch: expected `{expected}` but found `{found}`")]
    TyMismatch { expected: TyKind, found: TyKind },
}

impl TypeckErrorKind {
    pub fn summary(&self) -> String {
        match self {
            TypeckErrorKind::InferFailure => "could not infer the type of this".to_string(),
            TypeckErrorKind::TyMismatch { expected, found } => {
                format!("expected `{}`, found `{}`", expected, found)
            }
        }
    }
}

pub type Result<T, E = TypeckError> = std::result::Result<T, E>;

pub fn check_program(prog: Program) -> Result<Program> {
    Typeck::from_program(prog).check()
}

#[derive(Debug)]
pub struct Typeck {
    prog: Program,
}

impl Typeck {
    pub fn from_program(prog: Program) -> Self {
        Self { prog }
    }

    pub fn check(self) -> Result<Program> {
        self.check_all()?;

        Ok(self.prog)
    }
}

macro_rules! unwrap_checkd {
    ($res:expr) => {
        match &*$res {
            ResolveStatus::Resolved(v) => v,
            ResolveStatus::Unresolved(ident) => {
                panic!("internal error: type {} is not checkd yet?", ident.ident)
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

impl Typeck {
    fn check_all(&self) -> Result<()> {
        let mut visitor = TypeckVisitor {
            prog: &self.prog,
            locals: BTreeMap::new(),
            err: None,
        };
        visitor.visit_all(&self.prog);
        return match visitor.err {
            None => Ok(()),
            Some(err) => Err(err),
        };

        struct TypeckVisitor<'hir> {
            prog: &'hir Program,
            locals: BTreeMap<String, (Span, TyKind)>,
            err: Option<TypeckError>,
        }

        fn check_primitive(ty: &Ty) -> Result<()> {
            let cloned = ty.res.borrow().clone();
            if let CheckStatus::Unchecked(name) = cloned {
                match &*name.ident {
                    "int" => *ty.res.borrow_mut() = CheckStatus::Checkd(TyKind::Int),
                    "float" => *ty.res.borrow_mut() = CheckStatus::Checkd(TyKind::Float),
                    _ => {
                        return Err(TypeckError {
                            span: ty.span,
                            kind: TypeckErrorKind::UnknownTy {
                                ident: name.ident.clone(),
                            },
                        })
                    }
                }
            }

            Ok(())
        }

        impl Visit for TypeckVisitor<'_> {
            fn visit_fndecl(&mut self, fndecl: &FnDecl) {
                stop_if_err!(self.err);
                hir_visit::visit_fndecl(self, fndecl);
                stop_if_err!(self.err);
                return_if_err!(self, check_primitive(&fndecl.ret_ty));
            }

            fn visit_fnbody(&mut self, fnbody: &FnBody) {
                stop_if_err!(self.err);

                // set up new local variable tables
                self.locals = BTreeMap::new();

                // args and return values should be registered to the local table.
                let fndecl = self.prog.fndecl(fnbody.id);
                let local = (
                    fndecl.name.span,
                    unwrap_checkd!(fndecl.ret_ty.res.borrow()).clone(),
                );
                self.locals.insert(fndecl.name.ident.clone(), local);
                for param in &fndecl.params {
                    let local = (
                        param.name.span,
                        unwrap_checkd!(param.ty.res.borrow()).clone(),
                    );
                    let old = self.locals.insert(param.name.ident.clone(), local);
                    if old.is_some() {
                        return_err!(
                            self,
                            TypeckError {
                                span: param.name.span,
                                kind: TypeckErrorKind::AlreadyDefinedVariable {
                                    ident: param.name.ident.clone(),
                                }
                            }
                        );
                    }
                }

                hir_visit::visit_fnbody(self, fnbody);
                stop_if_err!(self.err);
            }

            fn visit_param(&mut self, param: &Param) {
                stop_if_err!(self.err);
                hir_visit::visit_param(self, param);
                stop_if_err!(self.err);
                return_if_err!(self, check_primitive(&param.ty));
            }

            fn visit_assg_stmt(&mut self, stmt: &AssgStmt) {
                stop_if_err!(self.err);
                // You can't use hir_visit::visit_assg_stmt(), because you need to declare the
                // variable if it's not declared. But to infer the type of declared variable, you
                // need to first check type of the assigning expr.
                self.visit_arith_expr(&stmt.expr);
                stop_if_err!(self.err);

                let ty_expr = unwrap_checkd!(stmt.expr.ty.res.borrow()).clone();
                let (_, ty_var) = self
                    .locals
                    .entry(stmt.var.name.ident.clone())
                    .or_insert_with(|| (stmt.var.span, ty_expr.clone()));
                let ty_var = ty_var.clone();

                if ty_var == ty_expr {
                    *stmt.var.ty.res.borrow_mut() = CheckStatus::Checkd(ty_var);
                } else {
                    return_err!(
                        self,
                        TypeckError {
                            span: stmt.expr.span,
                            kind: TypeckErrorKind::TyMismatch {
                                expected: ty_var,
                                found: ty_expr,
                            }
                        }
                    );
                }
            }

            fn visit_arith_expr(&mut self, expr: &ArithExpr) {
                stop_if_err!(self.err);
                hir_visit::visit_arith_expr(self, expr);
                stop_if_err!(self.err);
                match &expr.kind {
                    ArithExprKind::Primary(e) => {
                        *expr.ty.res.borrow_mut() =
                            CheckStatus::Checkd(unwrap_checkd!(e.ty.res.borrow()).clone());
                    }
                    ArithExprKind::UnaryOp(_, e) => {
                        *expr.ty.res.borrow_mut() =
                            CheckStatus::Checkd(unwrap_checkd!(e.ty.res.borrow()).clone());
                    }
                    ArithExprKind::BinOp(_, l, r) => {
                        let lt = unwrap_checkd!(l.ty.res.borrow()).clone();
                        let rt = unwrap_checkd!(r.ty.res.borrow()).clone();

                        if lt == rt {
                            *expr.ty.res.borrow_mut() = CheckStatus::Checkd(lt);
                        } else {
                            return_err!(
                                self,
                                TypeckError {
                                    span: r.span,
                                    kind: TypeckErrorKind::TyMismatch {
                                        expected: lt,
                                        found: rt,
                                    },
                                }
                            );
                        }
                    }
                }
            }

            fn visit_primary_expr(&mut self, expr: &PrimaryExpr) {
                stop_if_err!(self.err);
                hir_visit::visit_primary_expr(self, expr);
                stop_if_err!(self.err);

                match &expr.kind {
                    PrimaryExprKind::Var(v) => {
                        *expr.ty.res.borrow_mut() =
                            CheckStatus::Checkd(unwrap_checkd!(v.ty.res.borrow()).clone());
                    }
                    PrimaryExprKind::Const(c) => {
                        *expr.ty.res.borrow_mut() =
                            CheckStatus::Checkd(unwrap_checkd!(c.ty.res.borrow()).clone());
                    }
                    PrimaryExprKind::FnCall(fc) => {
                        let id = *unwrap_checkd!(fc.res.borrow());
                        let fndecl = self.prog.fndecl(id);
                        *expr.ty.res.borrow_mut() =
                            CheckStatus::Checkd(unwrap_checkd!(fndecl.ret_ty.res.borrow()).clone());
                    }
                    PrimaryExprKind::Paren(e) => {
                        *expr.ty.res.borrow_mut() =
                            CheckStatus::Checkd(unwrap_checkd!(e.ty.res.borrow()).clone());
                    }
                }
            }

            fn visit_var(&mut self, var: &Var) {
                stop_if_err!(self.err);
                hir_visit::visit_var(self, var);
                stop_if_err!(self.err);

                match self.locals.get(&var.name.ident) {
                    Some((_, ty)) => {
                        *var.ty.res.borrow_mut() = CheckStatus::Checkd(ty.clone());
                    }
                    None => {
                        return_err!(
                            self,
                            TypeckError {
                                span: var.span,
                                kind: TypeckErrorKind::UndeclaredVariable {
                                    ident: var.name.ident.clone()
                                }
                            }
                        );
                    }
                }
            }

            fn visit_fncall(&mut self, fncall: &FnCall) {
                stop_if_err!(self.err);
                hir_visit::visit_fncall(self, fncall);
                stop_if_err!(self.err);

                let cloned = fncall.res.borrow().clone();
                if let CheckStatus::Unchecked(name) = cloned {
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
                                TypeckError {
                                    span: fncall.span_name,
                                    kind: TypeckErrorKind::UndeclaredFunction {
                                        ident: name.ident.clone(),
                                    }
                                }
                            );
                        }
                    };
                    *fncall.res.borrow_mut() = CheckStatus::Checkd(fndecl.id);
                }
            }
        }
    }

    fn validate(&self) {
        let mut visitor = ValidatorVisitor;
        visitor.visit_all(&self.prog);
        struct ValidatorVisitor;
        impl Visit for ValidatorVisitor {}
    }
}
