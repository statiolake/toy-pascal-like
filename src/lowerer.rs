use crate::ast::*;
use crate::builtins::Builtin;
use crate::hir::*;
use crate::span::Span;
use itertools::Itertools;
use maplit::btreemap;
use std::cell::RefCell;
use std::collections::BTreeMap;

pub fn lower_ast(stmt: &Ast<AstBeginStmt>, builtins: Vec<Builtin>) -> HirProgram {
    let (mut lctx, root_scope_id) = LoweringContext::with_builtins(builtins);

    // register script root function
    let (start_fn_id, start_scope_id) = lctx.register_fndecl(
        root_scope_id,
        stmt.span,
        Ident {
            span: Span::new_zero(),
            ident: SCRIPT_ROOT_FN_NAME.to_string(),
        },
        vec![],
        HirTy {
            span: Span::new_zero(),
            res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(
                TyKind::Void,
            ))),
        },
    );

    // lower the entire statement
    let stmt = lctx.lower_begin_stmt(start_fn_id, start_scope_id, stmt);
    let stmt_id = lctx.register_stmt(stmt.into());

    lctx.register_fnbody(
        root_scope_id,
        start_fn_id,
        HirFnBody {
            id: start_fn_id,
            inner_scope_id: start_scope_id,
            kind: HirFnBodyKind::Stmt(stmt_id),
        },
    );

    HirProgram {
        scopes: lctx.scopes,
        start_fn_id,
        fndecls: lctx.fndecls,
        fnbodies: lctx.fnbodies,
        stmts: lctx.stmts,
        exprs: lctx.exprs,
    }
}

struct LoweringContext {
    scopes: BTreeMap<ScopeId, HirScope>,
    fndecls: BTreeMap<FnId, HirFnDecl>,
    fnbodies: BTreeMap<FnId, HirFnBody>,
    stmts: BTreeMap<StmtId, HirStmt>,
    exprs: BTreeMap<ExprId, HirArithExpr>,
    id_gen: ItemIdGenerator,
}

impl LoweringContext {
    fn new() -> (Self, ScopeId) {
        let mut id_gen = ItemIdGenerator::new();

        // prepare root scope
        let root_scope_id = id_gen.gen_scope();
        let root_scope = HirScope::new(root_scope_id, None);

        let scopes = btreemap! { root_scope_id => root_scope };
        let lctx = Self {
            scopes,
            fndecls: BTreeMap::new(),
            fnbodies: BTreeMap::new(),
            stmts: BTreeMap::new(),
            exprs: BTreeMap::new(),
            id_gen,
        };

        (lctx, root_scope_id)
    }

    fn with_builtins(builtins: Vec<Builtin>) -> (Self, ScopeId) {
        let (mut lctx, root_scope_id) = Self::new();

        for builtin in builtins {
            let Builtin {
                name,
                params,
                ret_ty,
                body_kind,
            } = builtin;

            let name = Ident {
                span: Span::new_zero(),
                ident: name,
            };
            let params = params
                .into_iter()
                .map(|(_, ty_kind)| HirParam {
                    span: Span::new_zero(),
                    res: None,
                    ty: HirTy {
                        span: Span::new_zero(),
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(ty_kind))),
                    },
                })
                .collect_vec();
            let ret_ty = HirTy {
                span: Span::new_zero(),
                res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(ret_ty))),
            };

            let (fn_id, inner_scope_id) =
                lctx.register_fndecl(root_scope_id, Span::new_zero(), name, params, ret_ty);
            lctx.register_fnbody(
                root_scope_id,
                fn_id,
                HirFnBody {
                    id: fn_id,
                    inner_scope_id,
                    kind: body_kind,
                },
            );
        }

        (lctx, root_scope_id)
    }

    fn scope(&self, id: ScopeId) -> &HirScope {
        self.scopes
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: scope {:?} not registered", id))
    }

    fn scope_mut(&mut self, id: ScopeId) -> &mut HirScope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: scope {:?} not registered", id))
    }

    fn register_fndecl(
        &mut self,
        curr_scope_id: ScopeId,
        span: Span,
        name: Ident,
        params: Vec<HirParam>,
        ret_ty: HirTy,
    ) -> (FnId, ScopeId) {
        // Prepare function
        let new_fn_id = self.id_gen.gen_fn();
        let new_scope_id = self.id_gen.gen_scope();
        let new_fn = HirFnDecl {
            id: new_fn_id,
            scope_id: curr_scope_id,
            span,
            name: name.clone(),
            params: params.clone(),
            ret_var: RefCell::new(ResolveStatus::Unresolved(name.clone())),
            ret_ty: ret_ty.clone(),
        };

        // Register this FnDecl
        assert!(
            self.fndecls.insert(new_fn_id, new_fn).is_none(),
            "ID conflict for function {:?}",
            new_fn_id
        );
        self.scope_mut(curr_scope_id).fn_ids.push(new_fn_id);

        // Prepare new scope; variables for return value and parameters should also be registered as
        // a local variable.
        let new_scope = HirScope::new(new_scope_id, Some(curr_scope_id));
        self.scopes.insert(new_scope_id, new_scope);

        // return variable
        self.try_register_var(new_scope_id, name, ret_ty);

        // parameters
        for param in params {
            // Register parameters only when they have a name; i.e. except builtin functions written
            // in native Rust closure.
            if let Some(name) = param.res {
                if let ResolveStatus::Unresolved(name) = name.borrow().clone() {
                    self.try_register_var(new_scope_id, name, param.ty);
                } else {
                    panic!("internal error: parameters should not be resolved at this stage");
                }
            }
        }

        (new_fn_id, new_scope_id)
    }

    fn register_fnbody(&mut self, scope_id: ScopeId, reg_fn_id: FnId, body: HirFnBody) {
        self.fnbodies.insert(reg_fn_id, body);
        debug_assert!(
            self.scope_mut(scope_id).fn_ids.contains(&reg_fn_id),
            "function body of undeclared function ({:?}) is registered",
            reg_fn_id
        );
    }

    fn register_stmt(&mut self, stmt: HirStmt) -> StmtId {
        let stmt_id = self.id_gen.gen_stmt();
        self.stmts.insert(stmt_id, stmt);
        stmt_id
    }

    fn register_expr(&mut self, expr: HirArithExpr) -> ExprId {
        let expr_id = self.id_gen.gen_expr();
        self.exprs.insert(expr_id, expr);
        expr_id
    }

    fn try_register_var(&mut self, scope_id: ScopeId, name: Ident, ty: HirTy) {
        // If the variable of same name is already registered, skip this. Those correctness is
        // checked in resolve phase, where the variable is actually resolved.
        if self
            .scope(scope_id)
            .vars
            .values()
            .any(|v| v.name.ident == name.ident)
        {
            return;
        }

        // We currently does not attach this ID to anywhere; those connection is built in resolve
        // phase.
        let id = self.id_gen.gen_var();
        self.scope_mut(scope_id)
            .vars
            .insert(id, HirVar { id, name, ty });
    }

    fn lower_stmt(&mut self, fn_id: FnId, scope_id: ScopeId, stmt: &Ast<AstStmt>) -> StmtId {
        let lowered = match &stmt.ast {
            AstStmt::FuncdefStmt(s) => {
                HirStmtKind::FnDef(self.lower_fndef_stmt(fn_id, scope_id, s))
            }
            AstStmt::IfStmt(s) => HirStmtKind::If(self.lower_if_stmt(fn_id, scope_id, s)),
            AstStmt::WhileStmt(s) => HirStmtKind::While(self.lower_while_stmt(fn_id, scope_id, s)),
            AstStmt::BeginStmt(s) => HirStmtKind::Begin(self.lower_begin_stmt(fn_id, scope_id, s)),
            AstStmt::AssgStmt(s) => HirStmtKind::Assg(self.lower_assg_stmt(fn_id, scope_id, s)),
            AstStmt::DumpStmt(s) => HirStmtKind::Dump(self.lower_dump_stmt(fn_id, scope_id, s)),
        };

        let stmt = HirStmt {
            span: stmt.span,
            kind: lowered,
        };
        self.register_stmt(stmt)
    }

    fn lower_fndef_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstFuncdefStmt>,
    ) -> FnId {
        let mut params = vec![];
        let mut curr_param = &*stmt.ast.params;
        while let Ast {
            span,
            ast: AstParamList::Nonempty { ident, ty, next },
        } = curr_param
        {
            params.push(HirParam {
                span: *span,
                res: Some(RefCell::new(ResolveStatus::Unresolved(
                    self.lower_ident(fn_id, scope_id, ident),
                ))),
                ty: self.lower_ty(fn_id, scope_id, ty),
            });
            curr_param = &next;
        }

        let ret_ty = self.lower_ty(fn_id, scope_id, &stmt.ast.ret_ty);
        let ident = self.lower_ident(fn_id, scope_id, &stmt.ast.name);
        let (new_fn_id, new_scope_id) =
            self.register_fndecl(scope_id, stmt.span, ident, params, ret_ty);

        let begin_stmt = self.lower_begin_stmt(new_fn_id, new_scope_id, &stmt.ast.body);
        let begin_stmt_id = self.register_stmt(begin_stmt.into());
        let body = HirFnBody {
            id: new_fn_id,
            inner_scope_id: new_scope_id,
            kind: HirFnBodyKind::Stmt(begin_stmt_id),
        };
        self.register_fnbody(scope_id, new_fn_id, body);

        new_fn_id
    }

    fn lower_if_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstIfStmt>,
    ) -> HirIfStmt {
        HirIfStmt {
            span: stmt.span,
            cond_id: self.lower_arith_expr(fn_id, scope_id, &stmt.ast.cond),
            then_id: self.lower_stmt(fn_id, scope_id, &stmt.ast.then),
            otherwise_id: stmt
                .ast
                .otherwise
                .as_ref()
                .map(|otherwise| self.lower_stmt(fn_id, scope_id, otherwise)),
        }
    }

    fn lower_while_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstWhileStmt>,
    ) -> HirWhileStmt {
        HirWhileStmt {
            span: stmt.span,
            cond_id: self.lower_arith_expr(fn_id, scope_id, &stmt.ast.cond),
            body_id: self.lower_stmt(fn_id, scope_id, &stmt.ast.body),
        }
    }

    fn lower_begin_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstBeginStmt>,
    ) -> HirBeginStmt {
        let mut stmt_ids = vec![];
        let mut curr = &stmt.ast.list.ast;
        while let AstStmtList::Nonempty { stmt, next } = curr {
            stmt_ids.push(self.lower_stmt(fn_id, scope_id, stmt));
            curr = &next.ast;
        }

        HirBeginStmt {
            span: stmt.span,
            stmt_ids,
        }
    }

    fn lower_assg_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstAssgStmt>,
    ) -> HirAssgStmt {
        HirAssgStmt {
            span: stmt.span,
            var: Box::new(self.lower_var(fn_id, scope_id, &stmt.ast.var, true)),
            expr_id: self.lower_arith_expr(fn_id, scope_id, &stmt.ast.expr),
        }
    }

    fn lower_dump_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstDumpStmt>,
    ) -> HirDumpStmt {
        HirDumpStmt {
            span: stmt.span,
            var: Box::new(self.lower_var(fn_id, scope_id, &stmt.ast.var, false)),
        }
    }

    fn lower_arith_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstArithExpr>,
    ) -> ExprId {
        match &expr.ast {
            AstArithExpr::AddExpr(expr) => self.lower_add_expr(fn_id, scope_id, &*expr),
            AstArithExpr::CompareExpr(op, l, r) => {
                let op = match &op.ast {
                    AstCompareOp::Lt => BinOp::Lt,
                    AstCompareOp::Gt => BinOp::Gt,
                    AstCompareOp::Le => BinOp::Le,
                    AstCompareOp::Ge => BinOp::Ge,
                    AstCompareOp::Eq => BinOp::Eq,
                    AstCompareOp::Ne => BinOp::Ne,
                };
                let kind = HirArithExprKind::BinOp(
                    op,
                    self.lower_arith_expr(fn_id, scope_id, l),
                    self.lower_add_expr(fn_id, scope_id, r),
                );
                let expr = HirArithExpr {
                    span: expr.span,
                    ty: HirTy {
                        span: expr.span,
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                    },
                    kind,
                };

                self.register_expr(expr)
            }
        }
    }

    fn lower_add_expr(&mut self, fn_id: FnId, scope_id: ScopeId, expr: &Ast<AstAddExpr>) -> ExprId {
        match &expr.ast {
            AstAddExpr::MulExpr(e) => self.lower_mul_expr(fn_id, scope_id, e),
            AstAddExpr::Add(l, r) | AstAddExpr::Sub(l, r) => {
                let op = match &expr.ast {
                    AstAddExpr::MulExpr(_) => unreachable!(),
                    AstAddExpr::Add(_, _) => BinOp::Add,
                    AstAddExpr::Sub(_, _) => BinOp::Sub,
                };
                let kind = HirArithExprKind::BinOp(
                    op,
                    self.lower_add_expr(fn_id, scope_id, l),
                    self.lower_mul_expr(fn_id, scope_id, r),
                );
                let expr = HirArithExpr {
                    span: expr.span,
                    kind,
                    ty: HirTy {
                        span: expr.span,
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                    },
                };

                self.register_expr(expr)
            }
        }
    }

    fn lower_mul_expr(&mut self, fn_id: FnId, scope_id: ScopeId, expr: &Ast<AstMulExpr>) -> ExprId {
        match &expr.ast {
            AstMulExpr::UnaryExpr(e) => self.lower_unary_expr(fn_id, scope_id, e),
            AstMulExpr::Mul(l, r) | AstMulExpr::Div(l, r) => {
                let op = match &expr.ast {
                    AstMulExpr::UnaryExpr(_) => unreachable!(),
                    AstMulExpr::Mul(_, _) => BinOp::Mul,
                    AstMulExpr::Div(_, _) => BinOp::Div,
                };
                let kind = HirArithExprKind::BinOp(
                    op,
                    self.lower_mul_expr(fn_id, scope_id, l),
                    self.lower_unary_expr(fn_id, scope_id, r),
                );
                let expr = HirArithExpr {
                    span: expr.span,
                    kind,
                    ty: HirTy {
                        span: expr.span,
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                    },
                };

                self.register_expr(expr)
            }
        }
    }

    fn lower_unary_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstUnaryExpr>,
    ) -> ExprId {
        let expr = match &expr.ast {
            AstUnaryExpr::PrimaryExpr(e) => HirArithExpr {
                span: expr.span,
                ty: HirTy {
                    span: expr.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                },
                kind: HirArithExprKind::Primary(Box::new(
                    self.lower_primary_expr(fn_id, scope_id, &e),
                )),
            },
            AstUnaryExpr::Neg(e) => HirArithExpr {
                span: expr.span,
                ty: HirTy {
                    span: expr.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                },
                kind: HirArithExprKind::UnaryOp(
                    UnaryOp::Neg,
                    self.lower_unary_expr(fn_id, scope_id, &e),
                ),
            },
        };

        self.register_expr(expr)
    }

    fn lower_primary_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstPrimaryExpr>,
    ) -> HirPrimaryExpr {
        HirPrimaryExpr {
            span: expr.span,
            ty: HirTy {
                span: expr.span,
                res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
            },
            kind: match &expr.ast {
                AstPrimaryExpr::Var(var) => {
                    HirPrimaryExprKind::Var(Box::new(self.lower_var(fn_id, scope_id, var, false)))
                }
                AstPrimaryExpr::Const(cst) => {
                    HirPrimaryExprKind::Const(Box::new(self.lower_const(fn_id, scope_id, cst)))
                }
                AstPrimaryExpr::FnCall(call) => {
                    HirPrimaryExprKind::FnCall(Box::new(self.lower_fncall(fn_id, scope_id, call)))
                }
                AstPrimaryExpr::Paren(expr) => {
                    HirPrimaryExprKind::Paren(self.lower_arith_expr(fn_id, scope_id, expr))
                }
            },
        }
    }

    fn lower_fncall(&mut self, fn_id: FnId, scope_id: ScopeId, call: &Ast<AstFnCall>) -> HirFnCall {
        let mut args = vec![];
        let mut curr_arg = &*call.ast.args;
        while let Ast {
            ast: AstArgumentList::Nonempty { expr, next },
            ..
        } = curr_arg
        {
            args.push(self.lower_arith_expr(fn_id, scope_id, expr));
            curr_arg = &next;
        }

        HirFnCall {
            span: call.span,
            span_name: call.ast.ident.span,
            res: RefCell::new(ResolveStatus::Unresolved(self.lower_ident(
                fn_id,
                scope_id,
                &call.ast.ident,
            ))),
            args,
        }
    }

    fn lower_const(&mut self, _fn_id: FnId, _scope_id: ScopeId, cst: &Ast<AstConst>) -> HirConst {
        let (ty, value) = match &cst.ast {
            AstConst::Int(v) => (
                HirTy {
                    span: cst.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Int))),
                },
                Value::Int(*v),
            ),
            AstConst::Float(v) => (
                HirTy {
                    span: cst.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(
                        TyKind::Float,
                    ))),
                },
                Value::Float(*v),
            ),
            AstConst::Bool(v) => (
                HirTy {
                    span: cst.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(
                        TyKind::Bool,
                    ))),
                },
                Value::Bool(*v),
            ),
        };
        HirConst {
            span: cst.span,
            ty,
            value,
        }
    }

    fn lower_var(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        var: &Ast<AstVar>,
        try_register: bool,
    ) -> HirVarRef {
        if try_register {
            // try registering new variable
            let name = self.lower_ident(fn_id, scope_id, var.ast.ident());
            self.try_register_var(
                scope_id,
                name,
                HirTy {
                    span: var.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                },
            );
        }

        HirVarRef {
            span: var.span,
            res: RefCell::new(ResolveStatus::Unresolved(self.lower_ident(
                fn_id,
                scope_id,
                var.ast.ident(),
            ))),
        }
    }

    fn lower_ty(&mut self, fn_id: FnId, scope_id: ScopeId, ty: &Ast<AstTy>) -> HirTy {
        HirTy {
            span: ty.span,
            res: RefCell::new(ResolveStatus::Unresolved(self.lower_ident(
                fn_id,
                scope_id,
                ty.ast.ident(),
            ))),
        }
    }

    fn lower_ident(&mut self, _fn_id: FnId, _scope_id: ScopeId, ident: &Ast<AstIdent>) -> Ident {
        Ident {
            span: ident.span,
            ident: ident.ast.ident().to_owned(),
        }
    }
}
