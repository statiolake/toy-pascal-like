use crate::hir::*;
use crate::hir_visit;
use crate::hir_visit::Visit;
use crate::rhir;
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

pub fn resolve_hir(prog: HirProgram) -> Result<rhir::Program, Vec<ResolverError>> {
    Resolver::from_program(prog).resolve()
}

#[derive(Debug)]
pub struct Resolver {
    prog: HirProgram,
}

impl Resolver {
    pub fn from_program(prog: HirProgram) -> Self {
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
        let mut visitor = ProgVisitorContext {
            prog: &self.prog,
            errors: vec![],
        };
        visitor.visit_all(&self.prog);
        return if visitor.errors.is_empty() {
            Ok(())
        } else {
            Err(visitor.errors)
        };

        struct ProgVisitorContext<'hir> {
            prog: &'hir HirProgram,
            errors: Vec<ResolverError>,
        }

        struct FnBodyVisitorContext<'hir> {
            prog: &'hir HirProgram,
            scope_id: ScopeId,

            /// Variables once visible in the scope. This is for example variables only declared at
            /// then statement of if. Only used for providing clearer diagnosis.
            once_init_vars: BTreeSet<VarId>,

            /// Variables always visible in the scope. This is for example variables declared in
            /// this scope, or variables declared both then and otherwise statements.
            init_vars: BTreeSet<VarId>,

            errors: Vec<ResolverError>,
        }

        fn resolve_primitive_ty(ty: &HirTy) -> Result<()> {
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

        impl FnBodyVisitorContext<'_> {
            fn resolve_var_ref(&mut self, var_ref: &HirVarRef, is_assign: bool) -> Result<()> {
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
                    if !self.init_vars.contains(&var_id) {
                        if is_assign {
                            self.once_init_vars.insert(var_id);
                            self.init_vars.insert(var_id);
                        } else {
                            let ident = name.ident.clone();
                            *var_ref.res.borrow_mut() = ResolveStatus::Err(name);
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
                    *var_ref.res.borrow_mut() = ResolveStatus::Resolved(var_id);
                }

                Ok(())
            }

            fn resolve_fncall(&mut self, fncall: &HirFnCall) -> Result<()> {
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

        impl Visit for ProgVisitorContext<'_> {
            fn visit_ty(&mut self, ty: &HirTy) {
                if let Err(err) = resolve_primitive_ty(ty) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_fnbody(&mut self, fnbody: &HirFnBody) {
                // set up new local variable tables
                let mut init_vars = BTreeSet::new();
                let scope_id = fnbody.inner_scope_id;
                let scope = self.prog.scope(scope_id);

                // params should be registered to the local vars table.
                let fndecl = self.prog.fndecl(fnbody.id);

                // return variable: it is added only when the return type is not void
                let ret_var_id = match find_var(scope, &fndecl.name) {
                    Ok(id) => id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                *fndecl.ret_var.borrow_mut() = ResolveStatus::Resolved(ret_var_id);

                // Add return value only when the return type is void: void function can omit the
                // assignment to the return variable. Otherwise, you must explicitly specify the
                // return value.
                if let ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Void)) =
                    &*fndecl.ret_ty.res.borrow()
                {
                    init_vars.insert(ret_var_id);
                }

                // find params and make it visible ...
                for param in &fndecl.params {
                    // ... only when this parameter has a name.
                    if let Some(name) = &param.res {
                        let ident = match name.borrow().clone() {
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
                        *name.borrow_mut() = ResolveStatus::Resolved(param_var_id);
                    }
                }

                let once_init_vars = init_vars.clone();
                let mut fnbody_visitor = FnBodyVisitorContext {
                    prog: self.prog,
                    scope_id,
                    init_vars,
                    once_init_vars,
                    errors: Vec::new(),
                };
                fnbody_visitor.visit_fnbody(&fnbody);

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

        impl Visit for FnBodyVisitorContext<'_> {
            fn visit_ty(&mut self, ty: &HirTy) {
                if let Err(err) = resolve_primitive_ty(ty) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_if_stmt(&mut self, stmt: &HirIfStmt) {
                self.visit_bool_expr(&stmt.cond);

                // if stmt may create "possibly uninitialized variables".
                let init_vars = self.init_vars.clone();
                self.visit_stmt(&stmt.then);
                // restore the original init_vars for else part
                let then_init_vars = replace(&mut self.init_vars, init_vars);
                self.visit_stmt(&stmt.otherwise);
                let otherwise_init_vars = self.init_vars.clone();

                // init_vars are initialized variables on both branch.
                self.init_vars = BTreeSet::intersection(&then_init_vars, &otherwise_init_vars)
                    .copied()
                    .collect();
            }

            fn visit_while_stmt(&mut self, stmt: &HirWhileStmt) {
                // any variables declared inside the while loop is "possibly uninitialized", because
                // the loop body is not necessarily run.
                let init_vars = self.init_vars.clone();
                self.visit_bool_expr(&stmt.cond);
                self.visit_stmt(&stmt.body);
                self.init_vars = init_vars;
            }

            fn visit_assg_stmt(&mut self, stmt: &HirAssgStmt) {
                // We can't use hir_visit::visit_assg_stmt(), since that will try to resolve var_ref
                // of asignee.
                self.visit_arith_expr(&stmt.expr);
                if let Err(err) = self.resolve_var_ref(&*stmt.var, true) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_var_ref(&mut self, var_ref: &HirVarRef) {
                hir_visit::visit_var_ref(self, var_ref);
                if let Err(err) = self.resolve_var_ref(&*var_ref, false) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_fncall(&mut self, fncall: &HirFnCall) {
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
        let HirProgram {
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

        fn convert_scopes(scopes: BTreeMap<ScopeId, HirScope>) -> BTreeMap<ScopeId, rhir::Scope> {
            scopes
                .into_iter()
                .map(|(id, scope)| (id, convert_scope(scope)))
                .collect()
        }

        fn convert_fndecls(fndecls: BTreeMap<FnId, HirFnDecl>) -> BTreeMap<FnId, rhir::FnDecl> {
            fndecls
                .into_iter()
                .map(|(id, fndecl)| (id, convert_fndecl(fndecl)))
                .collect()
        }

        fn convert_fnbodies(fnbodies: BTreeMap<FnId, HirFnBody>) -> BTreeMap<FnId, rhir::FnBody> {
            fnbodies
                .into_iter()
                .map(|(id, fnbody)| (id, convert_fnbody(fnbody)))
                .collect()
        }

        fn convert_scope(scope: HirScope) -> rhir::Scope {
            let HirScope {
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

        fn convert_vars(vars: BTreeMap<VarId, HirVar>) -> BTreeMap<VarId, rhir::Var> {
            vars.into_iter()
                .map(|(id, var)| (id, convert_var(var)))
                .collect()
        }

        fn convert_var(var: HirVar) -> rhir::Var {
            let HirVar { id, name, ty } = var;
            let ty = convert_ty(ty);

            rhir::Var { id, name, ty }
        }

        fn convert_fndecl(fndecl: HirFnDecl) -> rhir::FnDecl {
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
            let ret_var = convert_res(ret_var);
            let ret_ty = convert_ty(ret_ty);

            rhir::FnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_var,
                ret_ty,
            }
        }

        fn convert_param(param: HirParam) -> rhir::Param {
            let HirParam { span, res, ty } = param;
            let ty = convert_ty(ty);

            rhir::Param {
                span,
                res: res.map(convert_res),
                ty,
            }
        }

        fn convert_ty(ty: HirTy) -> rhir::Ty {
            let HirTy { span, res } = ty;
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

        fn convert_fnbody(fnbody: HirFnBody) -> rhir::FnBody {
            let HirFnBody {
                id,
                inner_scope_id,
                kind,
            } = fnbody;
            let kind = match kind {
                HirFnBodyKind::Stmt(stmt) => {
                    rhir::FnBodyKind::Stmt(Box::new(convert_begin_stmt(*stmt)))
                }
                HirFnBodyKind::Builtin(dynfn) => rhir::FnBodyKind::Builtin(dynfn),
            };

            rhir::FnBody {
                id,
                inner_scope_id,
                kind,
            }
        }

        fn convert_stmt(stmt: HirStmt) -> rhir::Stmt {
            let HirStmt { span, kind } = stmt;
            let kind = match kind {
                HirStmtKind::FnDef(id) => rhir::StmtKind::FnDef(id),
                HirStmtKind::If(stmt) => rhir::StmtKind::If(convert_if_stmt(stmt)),
                HirStmtKind::While(stmt) => rhir::StmtKind::While(convert_while_stmt(stmt)),
                HirStmtKind::Begin(stmt) => rhir::StmtKind::Begin(convert_begin_stmt(stmt)),
                HirStmtKind::Assg(stmt) => rhir::StmtKind::Assg(convert_assg_stmt(stmt)),
                HirStmtKind::Dump(stmt) => rhir::StmtKind::Dump(convert_dump_stmt(stmt)),
            };

            rhir::Stmt { span, kind }
        }

        fn convert_if_stmt(stmt: HirIfStmt) -> rhir::IfStmt {
            let HirIfStmt {
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

        fn convert_begin_stmt(stmt: HirBeginStmt) -> rhir::BeginStmt {
            let HirBeginStmt { span, stmts } = stmt;
            let stmts = stmts.into_iter().map(convert_stmt).collect_vec();

            rhir::BeginStmt { span, stmts }
        }

        fn convert_while_stmt(stmt: HirWhileStmt) -> rhir::WhileStmt {
            let HirWhileStmt { span, cond, body } = stmt;
            let cond = Box::new(convert_bool_expr(*cond));
            let body = Box::new(convert_stmt(*body));

            rhir::WhileStmt { span, cond, body }
        }

        fn convert_assg_stmt(stmt: HirAssgStmt) -> rhir::AssgStmt {
            let HirAssgStmt { span, var, expr } = stmt;
            let var = Box::new(convert_var_ref(*var));
            let expr = Box::new(convert_arith_expr(*expr));

            rhir::AssgStmt { span, var, expr }
        }

        fn convert_dump_stmt(stmt: HirDumpStmt) -> rhir::DumpStmt {
            let HirDumpStmt { span, var } = stmt;
            let var = Box::new(convert_var_ref(*var));

            rhir::DumpStmt { span, var }
        }

        fn convert_bool_expr(expr: HirBoolExpr) -> rhir::BoolExpr {
            let HirBoolExpr { span, op, lhs, rhs } = expr;
            let lhs = Box::new(convert_arith_expr(*lhs));
            let rhs = Box::new(convert_arith_expr(*rhs));

            rhir::BoolExpr { span, op, lhs, rhs }
        }

        fn convert_arith_expr(expr: HirArithExpr) -> rhir::ArithExpr {
            let HirArithExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                HirArithExprKind::Primary(e) => {
                    rhir::ArithExprKind::Primary(Box::new(convert_primary_expr(*e)))
                }
                HirArithExprKind::UnaryOp(op, e) => {
                    rhir::ArithExprKind::UnaryOp(op, Box::new(convert_arith_expr(*e)))
                }
                HirArithExprKind::BinOp(op, lhs, rhs) => rhir::ArithExprKind::BinOp(
                    op,
                    Box::new(convert_arith_expr(*lhs)),
                    Box::new(convert_arith_expr(*rhs)),
                ),
            };

            rhir::ArithExpr { span, ty, kind }
        }

        fn convert_primary_expr(expr: HirPrimaryExpr) -> rhir::PrimaryExpr {
            let HirPrimaryExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                HirPrimaryExprKind::Var(var) => {
                    rhir::PrimaryExprKind::Var(Box::new(convert_var_ref(*var)))
                }
                HirPrimaryExprKind::Const(cst) => {
                    rhir::PrimaryExprKind::Const(Box::new(convert_const(*cst)))
                }
                HirPrimaryExprKind::FnCall(fncall) => {
                    rhir::PrimaryExprKind::FnCall(Box::new(convert_fncall(*fncall)))
                }
                HirPrimaryExprKind::Paren(expr) => {
                    rhir::PrimaryExprKind::Paren(Box::new(convert_arith_expr(*expr)))
                }
            };

            rhir::PrimaryExpr { span, ty, kind }
        }

        fn convert_fncall(fncall: HirFnCall) -> rhir::FnCall {
            let HirFnCall {
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

        fn convert_var_ref(var: HirVarRef) -> rhir::VarRef {
            let HirVarRef { span, res } = var;
            let res = convert_res(res);

            rhir::VarRef { span, res }
        }

        fn convert_const(cst: HirConst) -> rhir::Const {
            let HirConst { span, ty, value } = cst;
            let ty = convert_ty(ty);

            rhir::Const { span, ty, value }
        }
    }
}
