use crate::hir::{FnId, ScopeId, TyKind, TypeckStatus, VarId};
use crate::rhir::*;
use crate::rhir_visit;
use crate::rhir_visit::Visit;
use crate::span::Span;
use crate::thir;
use itertools::izip;
use itertools::Itertools as _;
use std::cell::RefCell;
use std::collections::BTreeMap;

#[derive(thiserror::Error, Debug)]
#[error("{span:?}: {kind}")]
pub struct TypeckError {
    pub span: Span,
    pub kind: TypeckErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum TypeckErrorKind {
    // It never occurs because now type is uniquely determined easily.
    #[error("could not determine the type")]
    InferFailure,

    #[error("arity mismatch: expected {expected} arguments but found {found}")]
    ArityMismatch { expected: usize, found: usize },

    #[error("type mismatch: expected `{expected}` but found `{found}`")]
    TyMismatch { expected: TyKind, found: TyKind },
}

impl TypeckErrorKind {
    pub fn summary(&self) -> String {
        match self {
            TypeckErrorKind::InferFailure => "could not infer the type of this".to_string(),
            TypeckErrorKind::ArityMismatch { expected, .. } => {
                format!("{} arguments needed", expected)
            }
            TypeckErrorKind::TyMismatch { expected, found } => {
                format!("expected `{}`, found `{}`", expected, found)
            }
        }
    }
}

pub type Result<T, E = TypeckError> = std::result::Result<T, E>;

pub fn check_rhir(prog: Program) -> Result<thir::Program, Vec<TypeckError>> {
    TypeChecker::from_program(prog).check()
}

#[derive(Debug)]
pub struct TypeChecker {
    prog: Program,
}

impl TypeChecker {
    pub fn from_program(prog: Program) -> Self {
        Self { prog }
    }

    pub fn check(self) -> Result<thir::Program, Vec<TypeckError>> {
        self.check_all()?;

        Ok(self.into_thir())
    }
}

macro_rules! panic_not_inferred {
    () => {
        panic!("internal error: type of assignment expression is not yet inferred")
    };
}

impl TypeChecker {
    fn check_all(&self) -> Result<(), Vec<TypeckError>> {
        let scope_id = self.prog.fndecl(self.prog.start_fn_id).scope_id;
        let mut visitor = VisitorContext {
            prog: &self.prog,
            scope_id,
            errors: Vec::new(),
        };
        visitor.visit_all(&self.prog);
        return if visitor.errors.is_empty() {
            Ok(())
        } else {
            Err(visitor.errors)
        };

        struct VisitorContext<'rhir> {
            prog: &'rhir Program,
            scope_id: ScopeId,
            errors: Vec<TypeckError>,
        }

        impl Visit for VisitorContext<'_> {
            fn visit_fnbody(&mut self, fnbody: &FnBody) {
                // set the scope id to the current function
                self.scope_id = fnbody.inner_scope_id;
                rhir_visit::visit_fnbody(self, fnbody);
            }

            fn visit_assg_stmt(&mut self, stmt: &AssgStmt) {
                rhir_visit::visit_assg_stmt(self, stmt);

                let scope = self.prog.scope(self.scope_id);
                let var = scope.var(stmt.var.res);

                // check lhs and rhs types match
                let rt = stmt.expr.ty.res.borrow().clone();
                let rt = match rt {
                    TypeckStatus::Infer => panic_not_inferred!(),
                    TypeckStatus::Revealed(ty) => ty,
                    TypeckStatus::Err => return,
                };

                let lt = var.ty.res.borrow().clone();
                let lt = match lt {
                    TypeckStatus::Infer => rt.clone(),
                    TypeckStatus::Revealed(ty) => ty,
                    TypeckStatus::Err => return,
                };

                if lt != rt {
                    self.errors.push(TypeckError {
                        span: stmt.expr.span,
                        kind: TypeckErrorKind::TyMismatch {
                            expected: lt,
                            found: rt,
                        },
                    });
                    return;
                }

                *var.ty.res.borrow_mut() = TypeckStatus::Revealed(lt);
            }

            fn visit_arith_expr(&mut self, expr: &ArithExpr) {
                rhir_visit::visit_arith_expr(self, expr);

                *expr.ty.res.borrow_mut() = TypeckStatus::Err;
                match &expr.kind {
                    ArithExprKind::Primary(e) => {
                        *expr.ty.res.borrow_mut() = e.ty.res.borrow().clone();
                    }
                    ArithExprKind::UnaryOp(_, e) => {
                        *expr.ty.res.borrow_mut() = e.ty.res.borrow().clone();
                    }
                    ArithExprKind::BinOp(_, l, r) => {
                        let lt = match l.ty.res.borrow().clone() {
                            TypeckStatus::Infer => panic_not_inferred!(),
                            TypeckStatus::Revealed(ty) => ty,
                            TypeckStatus::Err => return,
                        };
                        let rt = match r.ty.res.borrow().clone() {
                            TypeckStatus::Infer => panic_not_inferred!(),
                            TypeckStatus::Revealed(ty) => ty,
                            TypeckStatus::Err => return,
                        };

                        if lt != rt {
                            self.errors.push(TypeckError {
                                span: r.span,
                                kind: TypeckErrorKind::TyMismatch {
                                    expected: lt,
                                    found: rt,
                                },
                            });
                            return;
                        }
                        *expr.ty.res.borrow_mut() = TypeckStatus::Revealed(lt);
                    }
                }
            }

            fn visit_primary_expr(&mut self, expr: &PrimaryExpr) {
                rhir_visit::visit_primary_expr(self, expr);

                *expr.ty.res.borrow_mut() = TypeckStatus::Err;
                match &expr.kind {
                    PrimaryExprKind::Var(v) => {
                        let var = self.prog.scope(self.scope_id).var(v.res);
                        *expr.ty.res.borrow_mut() = var.ty.res.borrow().clone();
                    }
                    PrimaryExprKind::Const(c) => {
                        *expr.ty.res.borrow_mut() = c.ty.res.borrow().clone();
                    }
                    PrimaryExprKind::FnCall(fc) => {
                        let fndecl = self.prog.fndecl(fc.res);
                        *expr.ty.res.borrow_mut() = fndecl.ret_ty.res.borrow().clone()
                    }
                    PrimaryExprKind::Paren(e) => {
                        *expr.ty.res.borrow_mut() = e.ty.res.borrow().clone();
                    }
                }
            }

            fn visit_fncall(&mut self, fncall: &FnCall) {
                rhir_visit::visit_fncall(self, fncall);

                let fndecl = self.prog.fndecl(fncall.res);

                // check the number of args
                if fncall.args.len() != fndecl.params.len() {
                    self.errors.push(TypeckError {
                        span: fncall.span,
                        kind: TypeckErrorKind::ArityMismatch {
                            expected: fndecl.params.len(),
                            found: fncall.args.len(),
                        },
                    });
                    return;
                }

                // check the type of each args
                for (param, arg) in izip!(&fndecl.params, &fncall.args) {
                    let pt = match &*param.ty.res.borrow() {
                        TypeckStatus::Revealed(ty) => ty.clone(),
                        TypeckStatus::Err | TypeckStatus::Infer => {
                            panic!("type of the function parameter is unknown")
                        }
                    };
                    let at = match &*arg.ty.res.borrow() {
                        TypeckStatus::Revealed(ty) => ty.clone(),
                        TypeckStatus::Infer => panic_not_inferred!(),
                        TypeckStatus::Err => return,
                    };

                    if pt != at {
                        self.errors.push(TypeckError {
                            span: arg.span,
                            kind: TypeckErrorKind::TyMismatch {
                                expected: pt,
                                found: at,
                            },
                        });
                    }
                }
            }
        }
    }
}

impl TypeChecker {
    fn into_thir(self) -> thir::Program {
        let Program {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
        } = self.prog;

        let scopes = convert_scopes(scopes);
        let fndecls = convert_fndecls(fndecls);
        let fnbodies = convert_fnbodies(fnbodies);

        return thir::Program {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
        };

        fn convert_scopes(scopes: BTreeMap<ScopeId, Scope>) -> BTreeMap<ScopeId, thir::Scope> {
            scopes
                .into_iter()
                .map(|(id, scope)| (id, convert_scope(scope)))
                .collect()
        }

        fn convert_fndecls(fndecls: BTreeMap<FnId, FnDecl>) -> BTreeMap<FnId, thir::FnDecl> {
            fndecls
                .into_iter()
                .map(|(id, fndecl)| (id, convert_fndecl(fndecl)))
                .collect()
        }

        fn convert_fnbodies(fnbodies: BTreeMap<FnId, FnBody>) -> BTreeMap<FnId, thir::FnBody> {
            fnbodies
                .into_iter()
                .map(|(id, fnbody)| (id, convert_fnbody(fnbody)))
                .collect()
        }

        fn convert_scope(scope: Scope) -> thir::Scope {
            let Scope {
                id,
                parent_id,
                fn_ids,
                vars,
            } = scope;
            let vars = convert_vars(vars);

            thir::Scope {
                id,
                parent_id,
                fn_ids,
                vars,
            }
        }

        fn convert_vars(vars: BTreeMap<VarId, Var>) -> BTreeMap<VarId, thir::Var> {
            vars.into_iter()
                .map(|(id, var)| (id, convert_var(var)))
                .collect()
        }

        fn convert_var(var: Var) -> thir::Var {
            let Var { id, name, ty } = var;
            let ty = convert_ty(ty);

            thir::Var { id, name, ty }
        }

        fn convert_fndecl(fndecl: FnDecl) -> thir::FnDecl {
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

            thir::FnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_ty,
            }
        }

        fn convert_param(param: Param) -> thir::Param {
            let Param { span, name, ty } = param;
            let ty = convert_ty(ty);

            thir::Param { span, name, ty }
        }

        fn convert_ty(ty: Ty) -> thir::Ty {
            let Ty { span, res } = ty;
            let res = convert_res(res);

            thir::Ty { span, res }
        }

        fn convert_res(res: RefCell<TypeckStatus>) -> TyKind {
            match res.into_inner() {
                TypeckStatus::Revealed(ty) => ty,
                TypeckStatus::Infer => panic!("internal error: type not inferred yet"),
                TypeckStatus::Err => panic!("internal error: typeck error not reported"),
            }
        }

        fn convert_fnbody(fnbody: FnBody) -> thir::FnBody {
            let FnBody {
                id,
                inner_scope_id,
                kind,
            } = fnbody;
            let kind = match kind {
                FnBodyKind::Stmt(stmt) => {
                    thir::FnBodyKind::Stmt(Box::new(convert_begin_stmt(*stmt)))
                }
                FnBodyKind::Builtin(dynfn) => thir::FnBodyKind::Builtin(dynfn),
            };

            thir::FnBody {
                id,
                inner_scope_id,
                kind,
            }
        }

        fn convert_stmt(stmt: Stmt) -> thir::Stmt {
            let Stmt { span, kind } = stmt;
            let kind = match kind {
                StmtKind::FnDef(id) => thir::StmtKind::FnDef(id),
                StmtKind::If(stmt) => thir::StmtKind::If(convert_if_stmt(stmt)),
                StmtKind::While(stmt) => thir::StmtKind::While(convert_while_stmt(stmt)),
                StmtKind::Begin(stmt) => thir::StmtKind::Begin(convert_begin_stmt(stmt)),
                StmtKind::Assg(stmt) => thir::StmtKind::Assg(convert_assg_stmt(stmt)),
                StmtKind::Dump(stmt) => thir::StmtKind::Dump(convert_dump_stmt(stmt)),
            };

            thir::Stmt { span, kind }
        }

        fn convert_if_stmt(stmt: IfStmt) -> thir::IfStmt {
            let IfStmt {
                span,
                cond,
                then,
                otherwise,
            } = stmt;

            let cond = Box::new(convert_bool_expr(*cond));
            let then = Box::new(convert_stmt(*then));
            let otherwise = Box::new(convert_stmt(*otherwise));

            thir::IfStmt {
                span,
                cond,
                then,
                otherwise,
            }
        }

        fn convert_begin_stmt(stmt: BeginStmt) -> thir::BeginStmt {
            let BeginStmt { span, stmts } = stmt;
            let stmts = stmts.into_iter().map(convert_stmt).collect_vec();

            thir::BeginStmt { span, stmts }
        }

        fn convert_while_stmt(stmt: WhileStmt) -> thir::WhileStmt {
            let WhileStmt { span, cond, body } = stmt;
            let cond = Box::new(convert_bool_expr(*cond));
            let body = Box::new(convert_stmt(*body));

            thir::WhileStmt { span, cond, body }
        }

        fn convert_assg_stmt(stmt: AssgStmt) -> thir::AssgStmt {
            let AssgStmt { span, var, expr } = stmt;
            let var = Box::new(convert_var_ref(*var));
            let expr = Box::new(convert_arith_expr(*expr));

            thir::AssgStmt { span, var, expr }
        }

        fn convert_dump_stmt(stmt: DumpStmt) -> thir::DumpStmt {
            let DumpStmt { span, var } = stmt;
            let var = Box::new(convert_var_ref(*var));

            thir::DumpStmt { span, var }
        }

        fn convert_bool_expr(expr: BoolExpr) -> thir::BoolExpr {
            let BoolExpr { span, op, lhs, rhs } = expr;
            let lhs = Box::new(convert_arith_expr(*lhs));
            let rhs = Box::new(convert_arith_expr(*rhs));

            thir::BoolExpr { span, op, lhs, rhs }
        }

        fn convert_arith_expr(expr: ArithExpr) -> thir::ArithExpr {
            let ArithExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                ArithExprKind::Primary(e) => {
                    thir::ArithExprKind::Primary(Box::new(convert_primary_expr(*e)))
                }
                ArithExprKind::UnaryOp(op, e) => {
                    thir::ArithExprKind::UnaryOp(op, Box::new(convert_arith_expr(*e)))
                }
                ArithExprKind::BinOp(op, lhs, rhs) => thir::ArithExprKind::BinOp(
                    op,
                    Box::new(convert_arith_expr(*lhs)),
                    Box::new(convert_arith_expr(*rhs)),
                ),
            };

            thir::ArithExpr { span, ty, kind }
        }

        fn convert_primary_expr(expr: PrimaryExpr) -> thir::PrimaryExpr {
            let PrimaryExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                PrimaryExprKind::Var(var) => {
                    thir::PrimaryExprKind::Var(Box::new(convert_var_ref(*var)))
                }
                PrimaryExprKind::Const(cst) => {
                    thir::PrimaryExprKind::Const(Box::new(convert_const(*cst)))
                }
                PrimaryExprKind::FnCall(fncall) => {
                    thir::PrimaryExprKind::FnCall(Box::new(convert_fncall(*fncall)))
                }
                PrimaryExprKind::Paren(expr) => {
                    thir::PrimaryExprKind::Paren(Box::new(convert_arith_expr(*expr)))
                }
            };

            thir::PrimaryExpr { span, ty, kind }
        }

        fn convert_fncall(fncall: FnCall) -> thir::FnCall {
            let FnCall {
                span,
                span_name,
                res,
                args,
            } = fncall;
            let args = args.into_iter().map(convert_arith_expr).collect_vec();

            thir::FnCall {
                span,
                span_name,
                res,
                args,
            }
        }

        fn convert_var_ref(var: VarRef) -> thir::VarRef {
            let VarRef { span, res } = var;
            thir::VarRef { span, res }
        }

        fn convert_const(cst: Const) -> thir::Const {
            let Const { span, ty, value } = cst;
            let ty = convert_ty(ty);

            thir::Const { span, ty, value }
        }
    }
}
