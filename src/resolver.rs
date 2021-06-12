use crate::hir::*;
use crate::hir_visit;
use crate::hir_visit::Visit;
use crate::rhir;
use crate::span::Span;
use itertools::Itertools as _;
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};

#[derive(thiserror::Error, Debug)]
#[error("{span:?}: {kind}")]
pub struct ResolverError {
    pub span: Span,
    pub kind: ResolverErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum ResolverErrorKind {
    #[error("undeclared function: `{ident}`")]
    UndeclaredFunction { ident: String },

    #[error("undeclared variable: `{ident}`")]
    UndeclaredVariable { ident: String },

    #[error("variable `{ident}` is read before initialization")]
    UseBeforeInit { ident: String },

    #[error("unknown type: `{ident}`")]
    UnknownTy { ident: String },

    // When two parameters are the same name, etc.
    #[error("already defined variable: `{ident}`")]
    AlreadyDefinedVariable { ident: String },
}

impl ResolverErrorKind {
    pub fn summary(&self) -> String {
        match self {
            ResolverErrorKind::UndeclaredFunction { .. } => "unknown function name".to_string(),
            ResolverErrorKind::UndeclaredVariable { .. } => "unknown variable name".to_string(),
            ResolverErrorKind::UseBeforeInit { .. } => "used but not initialized".to_string(),
            ResolverErrorKind::UnknownTy { .. } => "unknown type".to_string(),
            ResolverErrorKind::AlreadyDefinedVariable { .. } => "already defined".to_string(),
        }
    }
}

pub type Result<T, E = ResolverError> = std::result::Result<T, E>;

pub fn resolve_hir(prog: Program) -> Result<rhir::Program, Vec<ResolverError>> {
    Resolver::from_program(prog).resolve()
}

#[derive(Debug)]
pub struct Resolver {
    prog: Program,
}

impl Resolver {
    pub fn from_program(prog: Program) -> Self {
        Self { prog }
    }

    pub fn resolve(self) -> Result<rhir::Program, Vec<ResolverError>> {
        self.resolve_all()?;
        self.validate();

        Ok(self.into_rhir())
    }
}

impl Resolver {
    fn resolve_all(&self) -> Result<(), Vec<ResolverError>> {
        let scope_id = self.prog.fndecl(self.prog.start_fn_id).scope_id;
        let mut visitor = VisitorContext {
            prog: &self.prog,
            scope_id,
            visible_vars: BTreeSet::new(),
            errors: vec![],
        };
        visitor.visit_all(&self.prog);
        return if visitor.errors.is_empty() {
            Ok(())
        } else {
            Err(visitor.errors)
        };

        struct VisitorContext<'hir> {
            prog: &'hir Program,
            scope_id: ScopeId,
            visible_vars: BTreeSet<VarId>,
            errors: Vec<ResolverError>,
        }

        fn resolve_primitive_ty(ty: &Ty) -> Result<()> {
            let cloned = ty.res.borrow().clone();
            if let ResolveStatus::Unresolved(name) = cloned {
                match &*name.ident {
                    "int" => {
                        *ty.res.borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Int))
                    }
                    "float" => {
                        *ty.res.borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Float))
                    }
                    _ => {
                        *ty.res.borrow_mut() = ResolveStatus::Err(name.clone());
                        return Err(ResolverError {
                            span: ty.span,
                            kind: ResolverErrorKind::UnknownTy { ident: name.ident },
                        });
                    }
                }
            }

            Ok(())
        }

        fn find_var(scope: &Scope, name: &Ident) -> Result<VarId> {
            if let Some(var) = scope.vars.values().find(|v| v.name.ident == name.ident) {
                Ok(var.id)
            } else {
                Err(ResolverError {
                    span: name.span,
                    kind: ResolverErrorKind::UndeclaredVariable {
                        ident: name.ident.clone(),
                    },
                })
            }
        }

        impl VisitorContext<'_> {
            fn resolve_var_ref(&mut self, var_ref: &VarRef, is_assign: bool) -> Result<()> {
                let scope = self.prog.scope(self.scope_id);

                let cloned = var_ref.res.borrow().clone();
                if let ResolveStatus::Unresolved(name) = cloned {
                    let var_id = match find_var(scope, &name) {
                        Ok(id) => id,
                        Err(e) => {
                            // set the status to error
                            *var_ref.res.borrow_mut() = ResolveStatus::Err(name);
                            return Err(e);
                        }
                    };

                    // if the variable actually found but it's not yet visible, then register it as
                    // a visible variable if this is assign statement, and otherwise it's
                    // use-before-init error.
                    if !self.visible_vars.contains(&var_id) {
                        if is_assign {
                            self.visible_vars.insert(var_id);
                        } else {
                            let ident = name.ident.clone();
                            *var_ref.res.borrow_mut() = ResolveStatus::Err(name);
                            return Err(ResolverError {
                                span: var_ref.span,
                                kind: ResolverErrorKind::UseBeforeInit { ident },
                            });
                        }
                    }

                    // Successfully resolved now.
                    *var_ref.res.borrow_mut() = ResolveStatus::Resolved(var_id);
                }

                Ok(())
            }

            fn resolve_fncall(&mut self, fncall: &FnCall) -> Result<()> {
                let cloned = fncall.res.borrow().clone();
                if let ResolveStatus::Unresolved(name) = cloned {
                    let fndecl = self
                        .prog
                        .fndecls
                        .values()
                        .find(|fndecl| fndecl.name.ident == name.ident);
                    let fndecl = match fndecl {
                        Some(fndecl) => fndecl,
                        None => {
                            *fncall.res.borrow_mut() = ResolveStatus::Err(name.clone());
                            return Err(ResolverError {
                                span: fncall.span_name,
                                kind: ResolverErrorKind::UndeclaredFunction { ident: name.ident },
                            });
                        }
                    };
                    *fncall.res.borrow_mut() = ResolveStatus::Resolved(fndecl.id);
                }

                Ok(())
            }
        }

        impl Visit for VisitorContext<'_> {
            fn visit_fnbody(&mut self, fnbody: &FnBody) {
                // set up new local variable tables
                self.visible_vars = BTreeSet::new();
                self.scope_id = fnbody.inner_scope_id;
                let scope = self.prog.scope(self.scope_id);

                // params and return variable should be registered to the local vars table.
                let fndecl = self.prog.fndecl(fnbody.id);

                // return variable: it is added only when the return type is not void
                let ret_var_id = match find_var(scope, &fndecl.name) {
                    Ok(id) => id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                if !matches!(
                    &*scope.var(ret_var_id).ty.res.borrow(),
                    ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Void))
                ) {
                    // only add when the return value is not void
                    self.visible_vars.insert(ret_var_id);
                }

                // find params and make it visible
                for param in &fndecl.params {
                    let param_var_id = match find_var(scope, &param.name) {
                        Ok(id) => id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if !self.visible_vars.insert(param_var_id) {
                        // if the specified var_id is already visible: more than one parameters have
                        // the same name in this fndecl.
                        self.errors.push(ResolverError {
                            span: param.span,
                            kind: ResolverErrorKind::AlreadyDefinedVariable {
                                ident: param.name.ident.clone(),
                            },
                        });
                        return;
                    }
                }

                hir_visit::visit_fnbody(self, fnbody);
            }

            fn visit_ty(&mut self, ty: &Ty) {
                if let Err(err) = resolve_primitive_ty(ty) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_assg_stmt(&mut self, stmt: &AssgStmt) {
                // We can't use hir_visit::visit_assg_stmt(), since that will try to resolve var_ref
                // of asignee.
                self.visit_arith_expr(&stmt.expr);
                if let Err(err) = self.resolve_var_ref(&*stmt.var, true) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_var_ref(&mut self, var_ref: &VarRef) {
                hir_visit::visit_var_ref(self, var_ref);
                if let Err(err) = self.resolve_var_ref(&*var_ref, false) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_fncall(&mut self, fncall: &FnCall) {
                hir_visit::visit_fncall(self, fncall);
                if let Err(err) = self.resolve_fncall(fncall) {
                    self.errors.push(err);
                    return;
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

impl Resolver {
    fn into_rhir(self) -> rhir::Program {
        let Program {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
        } = self.prog;

        let scopes = convert_scopes(scopes);
        let fndecls = convert_fndecls(fndecls);
        let fnbodies = convert_fnbodies(fnbodies);

        return rhir::Program {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
        };

        fn convert_scopes(scopes: BTreeMap<ScopeId, Scope>) -> BTreeMap<ScopeId, rhir::Scope> {
            scopes
                .into_iter()
                .map(|(id, scope)| (id, convert_scope(scope)))
                .collect()
        }

        fn convert_fndecls(fndecls: BTreeMap<FnId, FnDecl>) -> BTreeMap<FnId, rhir::FnDecl> {
            fndecls
                .into_iter()
                .map(|(id, fndecl)| (id, convert_fndecl(fndecl)))
                .collect()
        }

        fn convert_fnbodies(fnbodies: BTreeMap<FnId, FnBody>) -> BTreeMap<FnId, rhir::FnBody> {
            fnbodies
                .into_iter()
                .map(|(id, fnbody)| (id, convert_fnbody(fnbody)))
                .collect()
        }

        fn convert_scope(scope: Scope) -> rhir::Scope {
            let Scope {
                id,
                parent_id,
                fn_ids,
                vars,
            } = scope;
            let vars = convert_vars(vars);

            rhir::Scope {
                id,
                parent_id,
                fn_ids,
                vars,
            }
        }

        fn convert_vars(vars: BTreeMap<VarId, Var>) -> BTreeMap<VarId, rhir::Var> {
            vars.into_iter()
                .map(|(id, var)| (id, convert_var(var)))
                .collect()
        }

        fn convert_var(var: Var) -> rhir::Var {
            let Var { id, name, ty } = var;
            let ty = convert_ty(ty);

            rhir::Var { id, name, ty }
        }

        fn convert_fndecl(fndecl: FnDecl) -> rhir::FnDecl {
            let FnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_ty,
            } = fndecl;

            let params = params.into_iter().map(convert_param).collect_vec();
            let ret_ty = convert_ty(ret_ty);

            rhir::FnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_ty,
            }
        }

        fn convert_param(param: Param) -> rhir::Param {
            let Param { span, name, ty } = param;
            let ty = convert_ty(ty);

            rhir::Param { span, name, ty }
        }

        fn convert_ty(ty: Ty) -> rhir::Ty {
            let Ty { span, res } = ty;
            let res = RefCell::new(convert_res(res));

            rhir::Ty { span, res }
        }

        fn convert_res<T: Clone>(res: RefCell<ResolveStatus<T>>) -> T {
            match res.into_inner() {
                ResolveStatus::Resolved(v) => v,
                ResolveStatus::Unresolved(ident) => panic!("item not resolved: {:?}", ident),
                ResolveStatus::Err(ident) => panic!("resolve error not reported: {:?}", ident),
            }
        }

        fn convert_fnbody(fnbody: FnBody) -> rhir::FnBody {
            let FnBody {
                id,
                inner_scope_id,
                kind,
            } = fnbody;
            let kind = match kind {
                FnBodyKind::Stmt(stmt) => {
                    rhir::FnBodyKind::Stmt(Box::new(convert_begin_stmt(*stmt)))
                }
                FnBodyKind::Builtin(dynfn) => rhir::FnBodyKind::Builtin(dynfn),
            };

            rhir::FnBody {
                id,
                inner_scope_id,
                kind,
            }
        }

        fn convert_stmt(stmt: Stmt) -> rhir::Stmt {
            let Stmt { span, kind } = stmt;
            let kind = match kind {
                StmtKind::FnDef(id) => rhir::StmtKind::FnDef(id),
                StmtKind::If(stmt) => rhir::StmtKind::If(convert_if_stmt(stmt)),
                StmtKind::While(stmt) => rhir::StmtKind::While(convert_while_stmt(stmt)),
                StmtKind::Begin(stmt) => rhir::StmtKind::Begin(convert_begin_stmt(stmt)),
                StmtKind::Assg(stmt) => rhir::StmtKind::Assg(convert_assg_stmt(stmt)),
                StmtKind::Dump(stmt) => rhir::StmtKind::Dump(convert_dump_stmt(stmt)),
            };

            rhir::Stmt { span, kind }
        }

        fn convert_if_stmt(stmt: IfStmt) -> rhir::IfStmt {
            let IfStmt {
                span,
                cond,
                then,
                otherwise,
            } = stmt;

            let cond = Box::new(convert_bool_expr(*cond));
            let then = Box::new(convert_stmt(*then));
            let otherwise = Box::new(convert_stmt(*otherwise));

            rhir::IfStmt {
                span,
                cond,
                then,
                otherwise,
            }
        }

        fn convert_begin_stmt(stmt: BeginStmt) -> rhir::BeginStmt {
            let BeginStmt { span, stmts } = stmt;
            let stmts = stmts.into_iter().map(convert_stmt).collect_vec();

            rhir::BeginStmt { span, stmts }
        }

        fn convert_while_stmt(stmt: WhileStmt) -> rhir::WhileStmt {
            let WhileStmt { span, cond, body } = stmt;
            let cond = Box::new(convert_bool_expr(*cond));
            let body = Box::new(convert_stmt(*body));

            rhir::WhileStmt { span, cond, body }
        }

        fn convert_assg_stmt(stmt: AssgStmt) -> rhir::AssgStmt {
            let AssgStmt { span, var, expr } = stmt;
            let var = Box::new(convert_var_ref(*var));
            let expr = Box::new(convert_arith_expr(*expr));

            rhir::AssgStmt { span, var, expr }
        }

        fn convert_dump_stmt(stmt: DumpStmt) -> rhir::DumpStmt {
            let DumpStmt { span, var } = stmt;
            let var = Box::new(convert_var_ref(*var));

            rhir::DumpStmt { span, var }
        }

        fn convert_bool_expr(expr: BoolExpr) -> rhir::BoolExpr {
            let BoolExpr { span, op, lhs, rhs } = expr;
            let lhs = Box::new(convert_arith_expr(*lhs));
            let rhs = Box::new(convert_arith_expr(*rhs));

            rhir::BoolExpr { span, op, lhs, rhs }
        }

        fn convert_arith_expr(expr: ArithExpr) -> rhir::ArithExpr {
            let ArithExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                ArithExprKind::Primary(e) => {
                    rhir::ArithExprKind::Primary(Box::new(convert_primary_expr(*e)))
                }
                ArithExprKind::UnaryOp(op, e) => {
                    rhir::ArithExprKind::UnaryOp(op, Box::new(convert_arith_expr(*e)))
                }
                ArithExprKind::BinOp(op, lhs, rhs) => rhir::ArithExprKind::BinOp(
                    op,
                    Box::new(convert_arith_expr(*lhs)),
                    Box::new(convert_arith_expr(*rhs)),
                ),
            };

            rhir::ArithExpr { span, ty, kind }
        }

        fn convert_primary_expr(expr: PrimaryExpr) -> rhir::PrimaryExpr {
            let PrimaryExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                PrimaryExprKind::Var(var) => {
                    rhir::PrimaryExprKind::Var(Box::new(convert_var_ref(*var)))
                }
                PrimaryExprKind::Const(cst) => {
                    rhir::PrimaryExprKind::Const(Box::new(convert_const(*cst)))
                }
                PrimaryExprKind::FnCall(fncall) => {
                    rhir::PrimaryExprKind::FnCall(Box::new(convert_fncall(*fncall)))
                }
                PrimaryExprKind::Paren(expr) => {
                    rhir::PrimaryExprKind::Paren(Box::new(convert_arith_expr(*expr)))
                }
            };

            rhir::PrimaryExpr { span, ty, kind }
        }

        fn convert_fncall(fncall: FnCall) -> rhir::FnCall {
            let FnCall {
                span,
                span_name,
                res,
                args,
            } = fncall;
            let res = convert_res(res);
            let args = args.into_iter().map(convert_arith_expr).collect_vec();

            rhir::FnCall {
                span,
                span_name,
                res,
                args,
            }
        }

        fn convert_var_ref(var: VarRef) -> rhir::VarRef {
            let VarRef { span, res } = var;
            let res = convert_res(res);

            rhir::VarRef { span, res }
        }

        fn convert_const(cst: Const) -> rhir::Const {
            let Const { span, ty, value } = cst;
            let ty = convert_ty(ty);

            rhir::Const { span, ty, value }
        }
    }
}
