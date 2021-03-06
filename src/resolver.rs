use crate::hir::*;
use crate::hir_visit;
use crate::hir_visit::Visit;
use crate::rhir::*;
use crate::span::Span;
use itertools::Itertools as _;
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::mem::replace;

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
    ReadBeforeInit { ident: String },

    #[error("variable `{ident}` is not always initialized")]
    PossiblyReadBeforeInit { ident: String },

    #[error("return value is not specified")]
    RetVarUninit,

    #[error("return value is not always specified")]
    PossiblyRetVarUninit,

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
            ResolverErrorKind::ReadBeforeInit { .. } => "used but not initialized".to_string(),
            ResolverErrorKind::PossiblyReadBeforeInit { .. } => "not always assigned".to_string(),
            ResolverErrorKind::RetVarUninit => "return value not specified".to_string(),
            ResolverErrorKind::PossiblyRetVarUninit => {
                "return value not always specified".to_string()
            }
            ResolverErrorKind::UnknownTy { .. } => "unknown type".to_string(),
            ResolverErrorKind::AlreadyDefinedVariable { .. } => "already defined".to_string(),
        }
    }
}

pub type Result<T, E = ResolverError> = std::result::Result<T, E>;

pub fn resolve_hir(ctx: HirContext) -> Result<RhirContext, Vec<ResolverError>> {
    Resolver::from_hir_ctx(ctx).resolve()
}

#[derive(Debug)]
pub struct Resolver {
    ctx: HirContext,
}

impl Resolver {
    pub fn from_hir_ctx(ctx: HirContext) -> Self {
        Self { ctx }
    }

    pub fn resolve(self) -> Result<RhirContext, Vec<ResolverError>> {
        self.resolve_all()?;
        self.validate();

        Ok(self.into_rhir())
    }
}

impl Resolver {
    fn resolve_all(&self) -> Result<(), Vec<ResolverError>> {
        let mut visitor = ProgVisitorContext { errors: vec![] };
        visitor.visit_all(&self.ctx);
        return if visitor.errors.is_empty() {
            Ok(())
        } else {
            Err(visitor.errors)
        };

        struct ProgVisitorContext {
            errors: Vec<ResolverError>,
        }

        struct FnBodyVisitorContext {
            scope_id: ScopeId,

            /// Variables once visible in the scope. This is for example variables only declared at
            /// then statement of if. Only used for providing clearer diagnosis.
            once_init_vars: BTreeSet<VarId>,

            /// Variables always visible in the scope. This is for example variables declared in
            /// this scope, or variables declared both then and otherwise statements.
            init_vars: BTreeSet<VarId>,

            errors: Vec<ResolverError>,
        }

        fn resolve_primitive_ty(ctx: &HirContext, ty: &HirTy) -> Result<()> {
            let cloned = ctx.res_ty_kind(ty.res_id).borrow().clone();
            if let ResolveStatus::Unresolved(name) = cloned {
                match &*name.ident {
                    "int" => {
                        *ctx.res_ty_kind(ty.res_id).borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Int))
                    }
                    "float" => {
                        *ctx.res_ty_kind(ty.res_id).borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Float))
                    }
                    "bool" => {
                        *ctx.res_ty_kind(ty.res_id).borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Bool))
                    }
                    _ => {
                        *ctx.res_ty_kind(ty.res_id).borrow_mut() = ResolveStatus::Err(name.clone());
                        return Err(ResolverError {
                            span: ty.span,
                            kind: ResolverErrorKind::UnknownTy { ident: name.ident },
                        });
                    }
                }
            }

            Ok(())
        }

        fn find_var(scope: &HirScope, name: &Ident) -> Result<VarId> {
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

        impl FnBodyVisitorContext {
            fn resolve_var_ref(
                &mut self,
                ctx: &HirContext,
                var_ref: &HirVarRef,
                is_assign: bool,
            ) -> Result<()> {
                let scope = ctx.scope(self.scope_id);

                let cloned = ctx.res_var_id(var_ref.res_id).borrow().clone();
                if let ResolveStatus::Unresolved(name) = cloned {
                    let var_id = match find_var(scope, &name) {
                        Ok(id) => id,
                        Err(e) => {
                            // set the status to error
                            *ctx.res_var_id(var_ref.res_id).borrow_mut() = ResolveStatus::Err(name);
                            return Err(e);
                        }
                    };

                    // if the variable actually found but it's not yet visible, then register it as
                    // a visible variable if this is assign statement, and otherwise it's
                    // use-before-init error.
                    if !self.init_vars.contains(&var_id) {
                        if is_assign {
                            self.once_init_vars.insert(var_id);
                            self.init_vars.insert(var_id);
                        } else {
                            let ident = name.ident.clone();
                            *ctx.res_var_id(var_ref.res_id).borrow_mut() = ResolveStatus::Err(name);
                            return Err(ResolverError {
                                span: var_ref.span,
                                kind: if self.once_init_vars.contains(&var_id) {
                                    ResolverErrorKind::PossiblyReadBeforeInit { ident }
                                } else {
                                    ResolverErrorKind::ReadBeforeInit { ident }
                                },
                            });
                        }
                    }

                    // Successfully resolved now.
                    *ctx.res_var_id(var_ref.res_id).borrow_mut() = ResolveStatus::Resolved(var_id);
                }

                Ok(())
            }

            fn resolve_fncall(&mut self, ctx: &HirContext, fncall: &HirFnCall) -> Result<()> {
                let cloned = ctx.res_fn_id(fncall.res_id).borrow().clone();
                if let ResolveStatus::Unresolved(name) = cloned {
                    let fndecl = ctx
                        .fndecls
                        .values()
                        .find(|fndecl| fndecl.name.ident == name.ident);
                    let fndecl = match fndecl {
                        Some(fndecl) => fndecl,
                        None => {
                            *ctx.res_fn_id(fncall.res_id).borrow_mut() =
                                ResolveStatus::Err(name.clone());
                            return Err(ResolverError {
                                span: fncall.span_name,
                                kind: ResolverErrorKind::UndeclaredFunction { ident: name.ident },
                            });
                        }
                    };
                    *ctx.res_fn_id(fncall.res_id).borrow_mut() = ResolveStatus::Resolved(fndecl.id);
                }

                Ok(())
            }
        }

        impl Visit for ProgVisitorContext {
            fn visit_ty(&mut self, ctx: &HirContext, ty: &HirTy) {
                if let Err(err) = resolve_primitive_ty(ctx, ty) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_fnbody(&mut self, ctx: &HirContext, fnbody: &HirFnBody) {
                // set up new local variable tables
                let mut init_vars = BTreeSet::new();
                let scope_id = fnbody.inner_scope_id;
                let scope = ctx.scope(scope_id);

                // params should be registered to the local vars table.
                let fndecl = ctx.fndecl(fnbody.id);

                // return variable: it is added only when the return type is not void
                let ret_var_id = match find_var(scope, &fndecl.name) {
                    Ok(id) => id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                *ctx.res_var_id(fndecl.ret_var).borrow_mut() = ResolveStatus::Resolved(ret_var_id);

                // Add return value only when the return type is void: void function can omit the
                // assignment to the return variable. Otherwise, you must explicitly specify the
                // return value.
                if let ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Void)) =
                    &*ctx.res_ty_kind(fndecl.ret_ty.res_id).borrow()
                {
                    init_vars.insert(ret_var_id);
                }

                // find params and make it visible ...
                for param in &fndecl.params {
                    // ... only when this parameter has a name.
                    if let Some(name) = &param.res_id {
                        let ident = match ctx.res_var_id(*name).borrow().clone() {
                            ResolveStatus::Unresolved(ident) => ident,
                            ResolveStatus::Resolved(_) => panic!(
                                "internal error: parameter should not be resolved at this stage"
                            ),
                            ResolveStatus::Err(_) => panic!(
                                "internal error: parameter should not be error at this stage"
                            ),
                        };

                        let param_var_id = match find_var(scope, &ident) {
                            Ok(id) => id,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        if !init_vars.insert(param_var_id) {
                            // if the specified var_id is already visible: more than one parameters have
                            // the same name in this fndecl.
                            self.errors.push(ResolverError {
                                span: param.span,
                                kind: ResolverErrorKind::AlreadyDefinedVariable {
                                    ident: ident.ident,
                                },
                            });
                            return;
                        }

                        // resolve this parameter as well
                        *ctx.res_var_id(*name).borrow_mut() = ResolveStatus::Resolved(param_var_id);
                    }
                }

                let once_init_vars = init_vars.clone();
                let mut fnbody_visitor = FnBodyVisitorContext {
                    scope_id,
                    init_vars,
                    once_init_vars,
                    errors: Vec::new(),
                };
                fnbody_visitor.visit_fnbody(ctx, &fnbody);

                // check return value is initialized (only when the body kind is Stmt, Builtins are
                // completely different).
                match &fnbody.kind {
                    HirFnBodyKind::Stmt(_) => {
                        if !fnbody_visitor.init_vars.contains(&ret_var_id) {
                            self.errors.push(ResolverError {
                                span: fndecl.name.span,
                                kind: if fnbody_visitor.once_init_vars.contains(&ret_var_id) {
                                    ResolverErrorKind::PossiblyRetVarUninit
                                } else {
                                    ResolverErrorKind::RetVarUninit
                                },
                            });
                        }
                    }
                    HirFnBodyKind::Builtin(_) => {
                        // No need to check. Builtins must work collectly.
                    }
                }

                self.errors.extend(fnbody_visitor.errors);
            }
        }

        impl Visit for FnBodyVisitorContext {
            fn visit_ty(&mut self, ctx: &HirContext, ty: &HirTy) {
                if let Err(err) = resolve_primitive_ty(ctx, ty) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_if_stmt(&mut self, ctx: &HirContext, stmt: &HirIfStmt) {
                let cond = ctx.expr(stmt.cond_id);
                self.visit_expr(ctx, cond);

                // if stmt may create "possibly uninitialized variables".
                let init_vars = self.init_vars.clone();
                let then = ctx.stmt(stmt.then_id);
                self.visit_stmt(ctx, then);
                // restore the original init_vars for else part
                let then_init_vars = replace(&mut self.init_vars, init_vars);
                if let Some(otherwise_id) = stmt.otherwise_id {
                    let otherwise = ctx.stmt(otherwise_id);
                    self.visit_stmt(ctx, otherwise);
                }
                let otherwise_init_vars = self.init_vars.clone();

                // init_vars are initialized variables on both branch.
                self.init_vars = BTreeSet::intersection(&then_init_vars, &otherwise_init_vars)
                    .copied()
                    .collect();
            }

            fn visit_while_stmt(&mut self, ctx: &HirContext, stmt: &HirWhileStmt) {
                // any variables declared inside the while loop is "possibly uninitialized", because
                // the loop body is not necessarily run.
                let init_vars = self.init_vars.clone();
                let cond = ctx.expr(stmt.cond_id);
                self.visit_expr(ctx, cond);
                let body = ctx.stmt(stmt.body_id);
                self.visit_stmt(ctx, body);
                self.init_vars = init_vars;
            }

            fn visit_assg_stmt(&mut self, ctx: &HirContext, stmt: &HirAssgStmt) {
                // We can't use hir_visit::visit_assg_stmt(), since that will try to resolve var_ref
                // of asignee.
                let expr = ctx.expr(stmt.expr_id);
                self.visit_expr(ctx, expr);
                if let Err(err) = self.resolve_var_ref(ctx, &stmt.var, true) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_var_ref(&mut self, ctx: &HirContext, var_ref: &HirVarRef) {
                hir_visit::visit_var_ref(self, ctx, var_ref);
                if let Err(err) = self.resolve_var_ref(ctx, &*var_ref, false) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_fncall(&mut self, ctx: &HirContext, fncall: &HirFnCall) {
                hir_visit::visit_fncall(self, ctx, fncall);
                if let Err(err) = self.resolve_fncall(ctx, fncall) {
                    self.errors.push(err);
                    return;
                }
            }
        }
    }

    fn validate(&self) {
        let mut visitor = ValidatorVisitor;
        visitor.visit_all(&self.ctx);
        struct ValidatorVisitor;
        impl Visit for ValidatorVisitor {}
    }
}

impl Resolver {
    fn into_rhir(self) -> RhirContext {
        let HirContext {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
            stmts,
            exprs,
            res_ty_kinds,
            res_fn_ids,
            res_var_ids,
        } = self.ctx;

        let scopes = convert_scopes(scopes);
        let fndecls = convert_fndecls(fndecls);
        let fnbodies = convert_fnbodies(fnbodies);
        let stmts = convert_stmts(stmts);
        let exprs = convert_exprs(exprs);
        let res_ty_kinds = convert_res_ty_kinds(res_ty_kinds);
        let res_fn_ids = convert_res_fn_ids(res_fn_ids);
        let res_var_ids = convert_res_var_ids(res_var_ids);

        return RhirContext {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
            stmts,
            exprs,
            res_ty_kinds,
            res_fn_ids,
            res_var_ids,
        };

        fn convert_scopes(scopes: BTreeMap<ScopeId, HirScope>) -> BTreeMap<ScopeId, RhirScope> {
            scopes
                .into_iter()
                .map(|(id, scope)| (id, convert_scope(scope)))
                .collect()
        }

        fn convert_fndecls(fndecls: BTreeMap<FnId, HirFnDecl>) -> BTreeMap<FnId, RhirFnDecl> {
            fndecls
                .into_iter()
                .map(|(id, fndecl)| (id, convert_fndecl(fndecl)))
                .collect()
        }

        fn convert_fnbodies(fnbodies: BTreeMap<FnId, HirFnBody>) -> BTreeMap<FnId, RhirFnBody> {
            fnbodies
                .into_iter()
                .map(|(id, fnbody)| (id, convert_fnbody(fnbody)))
                .collect()
        }

        fn convert_stmts(stmts: BTreeMap<StmtId, HirStmt>) -> BTreeMap<StmtId, RhirStmt> {
            stmts
                .into_iter()
                .map(|(id, stmt)| (id, convert_stmt(stmt)))
                .collect()
        }

        fn convert_exprs(exprs: BTreeMap<ExprId, HirExpr>) -> BTreeMap<ExprId, RhirExpr> {
            exprs
                .into_iter()
                .map(|(id, expr)| (id, convert_expr(expr)))
                .collect()
        }

        fn convert_res_ty_kinds(
            res_ty_kinds: BTreeMap<ResTyKindId, RefCell<ResolveStatus<TypeckStatus>>>,
        ) -> BTreeMap<ResTyKindId, RefCell<TypeckStatus>> {
            res_ty_kinds
                .into_iter()
                .map(|(id, res_ty_kind)| (id, RefCell::new(convert_res(res_ty_kind))))
                .collect()
        }

        fn convert_res_fn_ids(
            res_fn_ids: BTreeMap<ResFnIdId, RefCell<ResolveStatus<FnId>>>,
        ) -> BTreeMap<ResFnIdId, FnId> {
            res_fn_ids
                .into_iter()
                .map(|(id, res_fn_id)| (id, convert_res(res_fn_id)))
                .collect()
        }

        fn convert_res_var_ids(
            res_var_ids: BTreeMap<ResVarIdId, RefCell<ResolveStatus<VarId>>>,
        ) -> BTreeMap<ResVarIdId, VarId> {
            res_var_ids
                .into_iter()
                .map(|(id, res_var_id)| (id, convert_res(res_var_id)))
                .collect()
        }

        fn convert_scope(scope: HirScope) -> RhirScope {
            let HirScope {
                id,
                parent_id,
                fn_ids,
                vars,
            } = scope;
            let vars = convert_vars(vars);

            RhirScope {
                id,
                parent_id,
                fn_ids,
                vars,
            }
        }

        fn convert_vars(vars: BTreeMap<VarId, HirVar>) -> BTreeMap<VarId, RhirVar> {
            vars.into_iter()
                .map(|(id, var)| (id, convert_var(var)))
                .collect()
        }

        fn convert_var(var: HirVar) -> RhirVar {
            let HirVar { id, name, ty } = var;
            let ty = convert_ty(ty);

            RhirVar { id, name, ty }
        }

        fn convert_fndecl(fndecl: HirFnDecl) -> RhirFnDecl {
            let HirFnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_var,
                ret_ty,
            } = fndecl;
            let params = params.into_iter().map(convert_param).collect_vec();
            let ret_ty = convert_ty(ret_ty);

            RhirFnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_var,
                ret_ty,
            }
        }

        fn convert_param(param: HirParam) -> RhirParam {
            let HirParam { span, res_id, ty } = param;
            let ty = convert_ty(ty);
            RhirParam { span, res_id, ty }
        }

        fn convert_ty(ty: HirTy) -> RhirTy {
            let HirTy { span, res_id } = ty;
            RhirTy { span, res_id }
        }

        fn convert_res<T: Clone>(res: RefCell<ResolveStatus<T>>) -> T {
            match res.into_inner() {
                ResolveStatus::Resolved(v) => v,
                ResolveStatus::Unresolved(ident) => panic!("item not resolved: {:?}", ident),
                ResolveStatus::Err(ident) => panic!("resolve error not reported: {:?}", ident),
            }
        }

        fn convert_fnbody(fnbody: HirFnBody) -> RhirFnBody {
            let HirFnBody {
                id,
                inner_scope_id,
                kind,
            } = fnbody;
            let kind = match kind {
                HirFnBodyKind::Stmt(stmt_id) => RhirFnBodyKind::Stmt(stmt_id),
                HirFnBodyKind::Builtin(dynfn) => RhirFnBodyKind::Builtin(dynfn),
            };

            RhirFnBody {
                id,
                inner_scope_id,
                kind,
            }
        }

        fn convert_stmt(stmt: HirStmt) -> RhirStmt {
            let HirStmt { span, kind } = stmt;
            let kind = match kind {
                HirStmtKind::FnDef(id) => RhirStmtKind::FnDef(id),
                HirStmtKind::If(stmt) => RhirStmtKind::If(convert_if_stmt(stmt)),
                HirStmtKind::While(stmt) => RhirStmtKind::While(convert_while_stmt(stmt)),
                HirStmtKind::Begin(stmt) => RhirStmtKind::Begin(convert_begin_stmt(stmt)),
                HirStmtKind::Assg(stmt) => RhirStmtKind::Assg(convert_assg_stmt(stmt)),
                HirStmtKind::Dump(stmt) => RhirStmtKind::Dump(convert_dump_stmt(stmt)),
            };

            RhirStmt { span, kind }
        }

        fn convert_if_stmt(stmt: HirIfStmt) -> RhirIfStmt {
            let HirIfStmt {
                span,
                cond_id,
                then_id,
                otherwise_id,
            } = stmt;

            RhirIfStmt {
                span,
                cond_id,
                then_id,
                otherwise_id,
            }
        }

        fn convert_begin_stmt(stmt: HirBeginStmt) -> RhirBeginStmt {
            let HirBeginStmt { span, stmt_ids } = stmt;
            RhirBeginStmt { span, stmt_ids }
        }

        fn convert_while_stmt(stmt: HirWhileStmt) -> RhirWhileStmt {
            let HirWhileStmt {
                span,
                cond_id,
                body_id,
            } = stmt;

            RhirWhileStmt {
                span,
                cond_id,
                body_id,
            }
        }

        fn convert_assg_stmt(stmt: HirAssgStmt) -> RhirAssgStmt {
            let HirAssgStmt { span, var, expr_id } = stmt;
            let var = convert_var_ref(var);
            RhirAssgStmt { span, var, expr_id }
        }

        fn convert_dump_stmt(stmt: HirDumpStmt) -> RhirDumpStmt {
            let HirDumpStmt { span, var } = stmt;
            let var = convert_var_ref(var);
            RhirDumpStmt { span, var }
        }

        fn convert_expr(expr: HirExpr) -> RhirExpr {
            let HirExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                HirExprKind::Var(var) => RhirExprKind::Var(convert_var_ref(var)),
                HirExprKind::Const(cst) => RhirExprKind::Const(convert_const(cst)),
                HirExprKind::FnCall(fncall) => RhirExprKind::FnCall(convert_fncall(fncall)),
                HirExprKind::Paren(expr) => RhirExprKind::Paren(expr),
                HirExprKind::UnaryOp(op, e) => RhirExprKind::UnaryOp(op, e),
                HirExprKind::BinOp(op, lhs, rhs) => RhirExprKind::BinOp(op, lhs, rhs),
            };

            RhirExpr { span, ty, kind }
        }

        fn convert_fncall(fncall: HirFnCall) -> RhirFnCall {
            let HirFnCall {
                span,
                span_name,
                res_id,
                args,
            } = fncall;

            RhirFnCall {
                span,
                span_name,
                res_id,
                arg_ids: args,
            }
        }

        fn convert_var_ref(var: HirVarRef) -> RhirVarRef {
            let HirVarRef { span, res_id } = var;
            RhirVarRef { span, res_id }
        }

        fn convert_const(cst: HirConst) -> RhirConst {
            let HirConst { span, ty, value } = cst;
            let ty = convert_ty(ty);

            RhirConst { span, ty, value }
        }
    }
}
