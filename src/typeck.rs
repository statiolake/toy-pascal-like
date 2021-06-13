use crate::hir::*;
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

    #[error("type `{ty}` does not support `{op}`")]
    UnsupportedBinOp { ty: TyKind, op: BinOp },

    #[error("type `{ty}` does not support `{op}`")]
    UnsupportedUnaryOp { ty: TyKind, op: UnaryOp },
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
            TypeckErrorKind::UnsupportedBinOp { .. } => "unsupported binary operation".to_string(),
            TypeckErrorKind::UnsupportedUnaryOp { .. } => "unsupported unary operation".to_string(),
        }
    }
}

pub type Result<T, E = TypeckError> = std::result::Result<T, E>;

pub fn check_rhir(prog: RhirProgram) -> Result<thir::ThirProgram, Vec<TypeckError>> {
    TypeChecker::from_program(prog).check()
}

#[derive(Debug)]
pub struct TypeChecker {
    prog: RhirProgram,
}

impl TypeChecker {
    pub fn from_program(prog: RhirProgram) -> Self {
        Self { prog }
    }

    pub fn check(self) -> Result<thir::ThirProgram, Vec<TypeckError>> {
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
            prog: &'rhir RhirProgram,
            scope_id: ScopeId,
            errors: Vec<TypeckError>,
        }

        fn unary_op_ret_ty(op: UnaryOp, span_operand: Span, operand: &TyKind) -> Result<TyKind> {
            match op {
                UnaryOp::Neg => {
                    if operand.is_numeric() {
                        Ok(operand.clone())
                    } else {
                        Err(TypeckError {
                            span: span_operand,
                            kind: TypeckErrorKind::UnsupportedUnaryOp {
                                ty: operand.clone(),
                                op,
                            },
                        })
                    }
                }
            }
        }

        fn bin_op_ret_ty(
            op: BinOp,
            span_expr: Span,
            span_rhs: Span,
            lhs: &TyKind,
            rhs: &TyKind,
        ) -> Result<TyKind> {
            if op.is_arith() {
                return if lhs == rhs {
                    if lhs.is_numeric() {
                        Ok(lhs.clone())
                    } else {
                        Err(TypeckError {
                            span: span_expr,
                            kind: TypeckErrorKind::UnsupportedBinOp {
                                ty: lhs.clone(),
                                op,
                            },
                        })
                    }
                } else {
                    Err(TypeckError {
                        span: span_rhs,
                        kind: TypeckErrorKind::TyMismatch {
                            expected: lhs.clone(),
                            found: rhs.clone(),
                        },
                    })
                };
            }

            if op.is_compare() {
                return if lhs == rhs {
                    if lhs.is_comparable() {
                        Ok(TyKind::Bool)
                    } else {
                        Err(TypeckError {
                            span: span_expr,
                            kind: TypeckErrorKind::UnsupportedBinOp {
                                ty: lhs.clone(),
                                op,
                            },
                        })
                    }
                } else {
                    Err(TypeckError {
                        span: span_rhs,
                        kind: TypeckErrorKind::TyMismatch {
                            expected: lhs.clone(),
                            found: rhs.clone(),
                        },
                    })
                };
            }

            if op.is_equal() {
                return if lhs == rhs {
                    if lhs.is_equatable() {
                        Ok(TyKind::Bool)
                    } else {
                        Err(TypeckError {
                            span: span_expr,
                            kind: TypeckErrorKind::UnsupportedBinOp {
                                ty: lhs.clone(),
                                op,
                            },
                        })
                    }
                } else {
                    Err(TypeckError {
                        span: span_rhs,
                        kind: TypeckErrorKind::TyMismatch {
                            expected: lhs.clone(),
                            found: rhs.clone(),
                        },
                    })
                };
            }

            unreachable!("operator must be one of arith, compare, or equal")
        }

        impl Visit for VisitorContext<'_> {
            fn visit_fnbody(&mut self, fnbody: &RhirFnBody) {
                // set the scope id to the current function
                self.scope_id = fnbody.inner_scope_id;
                rhir_visit::visit_fnbody(self, fnbody);
            }

            fn visit_assg_stmt(&mut self, stmt: &RhirAssgStmt) {
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

            fn visit_arith_expr(&mut self, expr: &RhirArithExpr) {
                rhir_visit::visit_arith_expr(self, expr);

                *expr.ty.res.borrow_mut() = TypeckStatus::Err;
                match &expr.kind {
                    RhirArithExprKind::Primary(e) => {
                        *expr.ty.res.borrow_mut() = e.ty.res.borrow().clone();
                    }
                    RhirArithExprKind::UnaryOp(op, e) => {
                        let ty = match e.ty.res.borrow().clone() {
                            TypeckStatus::Infer => panic_not_inferred!(),
                            TypeckStatus::Revealed(ty) => ty,
                            TypeckStatus::Err => return,
                        };
                        let ret_ty = match unary_op_ret_ty(*op, e.span, &ty) {
                            Ok(ty) => ty,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };
                        *expr.ty.res.borrow_mut() = TypeckStatus::Revealed(ret_ty);
                    }
                    RhirArithExprKind::BinOp(op, l, r) => {
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

                        let ret_ty = match bin_op_ret_ty(*op, expr.span, r.span, &lt, &rt) {
                            Ok(ty) => ty,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };
                        *expr.ty.res.borrow_mut() = TypeckStatus::Revealed(ret_ty);
                    }
                }
            }

            fn visit_primary_expr(&mut self, expr: &RhirPrimaryExpr) {
                rhir_visit::visit_primary_expr(self, expr);

                *expr.ty.res.borrow_mut() = TypeckStatus::Err;
                match &expr.kind {
                    RhirPrimaryExprKind::Var(v) => {
                        let var = self.prog.scope(self.scope_id).var(v.res);
                        *expr.ty.res.borrow_mut() = var.ty.res.borrow().clone();
                    }
                    RhirPrimaryExprKind::Const(c) => {
                        *expr.ty.res.borrow_mut() = c.ty.res.borrow().clone();
                    }
                    RhirPrimaryExprKind::FnCall(fc) => {
                        let fndecl = self.prog.fndecl(fc.res);
                        *expr.ty.res.borrow_mut() = fndecl.ret_ty.res.borrow().clone()
                    }
                    RhirPrimaryExprKind::Paren(e) => {
                        *expr.ty.res.borrow_mut() = e.ty.res.borrow().clone();
                    }
                }
            }

            fn visit_fncall(&mut self, fncall: &RhirFnCall) {
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
    fn into_thir(self) -> thir::ThirProgram {
        let RhirProgram {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
        } = self.prog;

        let scopes = convert_scopes(scopes);
        let fndecls = convert_fndecls(fndecls);
        let fnbodies = convert_fnbodies(fnbodies);

        return thir::ThirProgram {
            scopes,
            start_fn_id,
            fndecls,
            fnbodies,
        };

        fn convert_scopes(
            scopes: BTreeMap<ScopeId, RhirScope>,
        ) -> BTreeMap<ScopeId, thir::ThirScope> {
            scopes
                .into_iter()
                .map(|(id, scope)| (id, convert_scope(scope)))
                .collect()
        }

        fn convert_fndecls(
            fndecls: BTreeMap<FnId, RhirFnDecl>,
        ) -> BTreeMap<FnId, thir::ThirFnDecl> {
            fndecls
                .into_iter()
                .map(|(id, fndecl)| (id, convert_fndecl(fndecl)))
                .collect()
        }

        fn convert_fnbodies(
            fnbodies: BTreeMap<FnId, RhirFnBody>,
        ) -> BTreeMap<FnId, thir::ThirFnBody> {
            fnbodies
                .into_iter()
                .map(|(id, fnbody)| (id, convert_fnbody(fnbody)))
                .collect()
        }

        fn convert_scope(scope: RhirScope) -> thir::ThirScope {
            let RhirScope {
                id,
                parent_id,
                fn_ids,
                vars,
            } = scope;
            let vars = convert_vars(vars);

            thir::ThirScope {
                id,
                parent_id,
                fn_ids,
                vars,
            }
        }

        fn convert_vars(vars: BTreeMap<VarId, RhirVar>) -> BTreeMap<VarId, thir::ThirVar> {
            vars.into_iter()
                .map(|(id, var)| (id, convert_var(var)))
                .collect()
        }

        fn convert_var(var: RhirVar) -> thir::ThirVar {
            let RhirVar { id, name, ty } = var;
            let ty = convert_ty(ty);

            thir::ThirVar { id, name, ty }
        }

        fn convert_fndecl(fndecl: RhirFnDecl) -> thir::ThirFnDecl {
            let RhirFnDecl {
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

            thir::ThirFnDecl {
                id,
                scope_id,
                span,
                name,
                params,
                ret_var,
                ret_ty,
            }
        }

        fn convert_param(param: RhirParam) -> thir::ThirParam {
            let RhirParam { span, res, ty } = param;
            let ty = convert_ty(ty);

            thir::ThirParam { span, res, ty }
        }

        fn convert_ty(ty: RhirTy) -> thir::ThirTy {
            let RhirTy { span, res } = ty;
            let res = convert_res(res);

            thir::ThirTy { span, res }
        }

        fn convert_res(res: RefCell<TypeckStatus>) -> TyKind {
            match res.into_inner() {
                TypeckStatus::Revealed(ty) => ty,
                TypeckStatus::Infer => panic!("internal error: type not inferred yet"),
                TypeckStatus::Err => panic!("internal error: typeck error not reported"),
            }
        }

        fn convert_fnbody(fnbody: RhirFnBody) -> thir::ThirFnBody {
            let RhirFnBody {
                id,
                inner_scope_id,
                kind,
            } = fnbody;
            let kind = match kind {
                RhirFnBodyKind::Stmt(stmt) => {
                    thir::ThirFnBodyKind::Stmt(Box::new(convert_begin_stmt(*stmt)))
                }
                RhirFnBodyKind::Builtin(dynfn) => thir::ThirFnBodyKind::Builtin(dynfn),
            };

            thir::ThirFnBody {
                id,
                inner_scope_id,
                kind,
            }
        }

        fn convert_stmt(stmt: RhirStmt) -> thir::ThirStmt {
            let RhirStmt { span, kind } = stmt;
            let kind = match kind {
                RhirStmtKind::FnDef(id) => thir::ThirStmtKind::FnDef(id),
                RhirStmtKind::If(stmt) => thir::ThirStmtKind::If(convert_if_stmt(stmt)),
                RhirStmtKind::While(stmt) => thir::ThirStmtKind::While(convert_while_stmt(stmt)),
                RhirStmtKind::Begin(stmt) => thir::ThirStmtKind::Begin(convert_begin_stmt(stmt)),
                RhirStmtKind::Assg(stmt) => thir::ThirStmtKind::Assg(convert_assg_stmt(stmt)),
                RhirStmtKind::Dump(stmt) => thir::ThirStmtKind::Dump(convert_dump_stmt(stmt)),
            };

            thir::ThirStmt { span, kind }
        }

        fn convert_if_stmt(stmt: RhirIfStmt) -> thir::ThirIfStmt {
            let RhirIfStmt {
                span,
                cond,
                then,
                otherwise,
            } = stmt;

            let cond = Box::new(convert_arith_expr(*cond));
            let then = Box::new(convert_stmt(*then));
            let otherwise = Box::new(convert_stmt(*otherwise));

            thir::ThirIfStmt {
                span,
                cond,
                then,
                otherwise,
            }
        }

        fn convert_begin_stmt(stmt: RhirBeginStmt) -> thir::ThirBeginStmt {
            let RhirBeginStmt { span, stmts } = stmt;
            let stmts = stmts.into_iter().map(convert_stmt).collect_vec();

            thir::ThirBeginStmt { span, stmts }
        }

        fn convert_while_stmt(stmt: RhirWhileStmt) -> thir::ThirWhileStmt {
            let RhirWhileStmt { span, cond, body } = stmt;
            let cond = Box::new(convert_arith_expr(*cond));
            let body = Box::new(convert_stmt(*body));

            thir::ThirWhileStmt { span, cond, body }
        }

        fn convert_assg_stmt(stmt: RhirAssgStmt) -> thir::ThirAssgStmt {
            let RhirAssgStmt { span, var, expr } = stmt;
            let var = Box::new(convert_var_ref(*var));
            let expr = Box::new(convert_arith_expr(*expr));

            thir::ThirAssgStmt { span, var, expr }
        }

        fn convert_dump_stmt(stmt: RhirDumpStmt) -> thir::ThirDumpStmt {
            let RhirDumpStmt { span, var } = stmt;
            let var = Box::new(convert_var_ref(*var));

            thir::ThirDumpStmt { span, var }
        }

        fn convert_arith_expr(expr: RhirArithExpr) -> thir::ThirArithExpr {
            let RhirArithExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                RhirArithExprKind::Primary(e) => {
                    thir::ThirArithExprKind::Primary(Box::new(convert_primary_expr(*e)))
                }
                RhirArithExprKind::UnaryOp(op, e) => {
                    thir::ThirArithExprKind::UnaryOp(op, Box::new(convert_arith_expr(*e)))
                }
                RhirArithExprKind::BinOp(op, lhs, rhs) => thir::ThirArithExprKind::BinOp(
                    op,
                    Box::new(convert_arith_expr(*lhs)),
                    Box::new(convert_arith_expr(*rhs)),
                ),
            };

            thir::ThirArithExpr { span, ty, kind }
        }

        fn convert_primary_expr(expr: RhirPrimaryExpr) -> thir::ThirPrimaryExpr {
            let RhirPrimaryExpr { span, ty, kind } = expr;
            let ty = convert_ty(ty);
            let kind = match kind {
                RhirPrimaryExprKind::Var(var) => {
                    thir::ThirPrimaryExprKind::Var(Box::new(convert_var_ref(*var)))
                }
                RhirPrimaryExprKind::Const(cst) => {
                    thir::ThirPrimaryExprKind::Const(Box::new(convert_const(*cst)))
                }
                RhirPrimaryExprKind::FnCall(fncall) => {
                    thir::ThirPrimaryExprKind::FnCall(Box::new(convert_fncall(*fncall)))
                }
                RhirPrimaryExprKind::Paren(expr) => {
                    thir::ThirPrimaryExprKind::Paren(Box::new(convert_arith_expr(*expr)))
                }
            };

            thir::ThirPrimaryExpr { span, ty, kind }
        }

        fn convert_fncall(fncall: RhirFnCall) -> thir::ThirFnCall {
            let RhirFnCall {
                span,
                span_name,
                res,
                args,
            } = fncall;
            let args = args.into_iter().map(convert_arith_expr).collect_vec();

            thir::ThirFnCall {
                span,
                span_name,
                res,
                args,
            }
        }

        fn convert_var_ref(var: RhirVarRef) -> thir::ThirVarRef {
            let RhirVarRef { span, res } = var;
            thir::ThirVarRef { span, res }
        }

        fn convert_const(cst: RhirConst) -> thir::ThirConst {
            let RhirConst { span, ty, value } = cst;
            let ty = convert_ty(ty);

            thir::ThirConst { span, ty, value }
        }
    }
}
