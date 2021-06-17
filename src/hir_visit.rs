use crate::hir::*;

pub trait Visit {
    fn visit_all(&mut self, ctx: &HirContext) {
        visit_all(self, ctx);
    }

    fn visit_scope(&mut self, ctx: &HirContext, scope: &HirScope) {
        visit_scope(self, ctx, scope);
    }

    fn visit_fndecl(&mut self, ctx: &HirContext, fndecl: &HirFnDecl) {
        visit_fndecl(self, ctx, fndecl);
    }

    fn visit_param(&mut self, ctx: &HirContext, param: &HirParam) {
        visit_param(self, ctx, param);
    }

    fn visit_ident(&mut self, ctx: &HirContext, ident: &Ident) {
        visit_ident(self, ctx, ident);
    }

    fn visit_ty(&mut self, ctx: &HirContext, ty: &HirTy) {
        visit_ty(self, ctx, ty);
    }

    fn visit_fnbody(&mut self, ctx: &HirContext, fnbody: &HirFnBody) {
        visit_fnbody(self, ctx, fnbody);
    }

    fn visit_var(&mut self, ctx: &HirContext, var: &HirVar) {
        visit_var(self, ctx, var);
    }

    fn visit_stmt(&mut self, ctx: &HirContext, stmt: &HirStmt) {
        visit_stmt(self, ctx, stmt);
    }

    fn visit_if_stmt(&mut self, ctx: &HirContext, stmt: &HirIfStmt) {
        visit_if_stmt(self, ctx, stmt);
    }

    fn visit_while_stmt(&mut self, ctx: &HirContext, stmt: &HirWhileStmt) {
        visit_while_stmt(self, ctx, stmt);
    }

    fn visit_begin_stmt(&mut self, ctx: &HirContext, stmt: &HirBeginStmt) {
        visit_begin_stmt(self, ctx, stmt);
    }

    fn visit_assg_stmt(&mut self, ctx: &HirContext, stmt: &HirAssgStmt) {
        visit_assg_stmt(self, ctx, stmt);
    }

    fn visit_dump_stmt(&mut self, ctx: &HirContext, stmt: &HirDumpStmt) {
        visit_dump_stmt(self, ctx, stmt);
    }

    fn visit_expr(&mut self, ctx: &HirContext, expr: &HirExpr) {
        visit_expr(self, ctx, expr);
    }

    fn visit_fncall(&mut self, ctx: &HirContext, fncall: &HirFnCall) {
        visit_fncall(self, ctx, fncall);
    }

    fn visit_var_ref(&mut self, ctx: &HirContext, var: &HirVarRef) {
        visit_var_ref(self, ctx, var);
    }

    fn visit_const(&mut self, ctx: &HirContext, cst: &HirConst) {
        visit_const(self, ctx, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext) {
    for scope in ctx.scopes.values() {
        v.visit_scope(ctx, scope);
    }

    for fndecl in ctx.fndecls.values() {
        v.visit_fndecl(ctx, fndecl);
    }

    for fnbody in ctx.fnbodies.values() {
        v.visit_fnbody(ctx, fnbody);
    }
}

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, fndecl: &HirFnDecl) {
    v.visit_ident(ctx, &fndecl.name);
    for param in &fndecl.params {
        v.visit_param(ctx, param);
    }
    v.visit_ty(ctx, &fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, param: &HirParam) {
    v.visit_ty(ctx, &param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _ctx: &HirContext, _ident: &Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _ctx: &HirContext, _ty: &HirTy) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, fnbody: &HirFnBody) {
    if let HirFnBodyKind::Stmt(stmt_id) = fnbody.kind {
        let stmt = ctx.stmt(stmt_id);
        v.visit_stmt(ctx, stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, scope: &HirScope) {
    for var in scope.vars.values() {
        v.visit_var(ctx, var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, var: &HirVar) {
    v.visit_ident(ctx, &var.name);
    v.visit_ty(ctx, &var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, stmt: &HirStmt) {
    match &stmt.kind {
        HirStmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        HirStmtKind::If(stmt) => v.visit_if_stmt(ctx, stmt),
        HirStmtKind::While(stmt) => v.visit_while_stmt(ctx, stmt),
        HirStmtKind::Begin(stmt) => v.visit_begin_stmt(ctx, stmt),
        HirStmtKind::Assg(stmt) => v.visit_assg_stmt(ctx, stmt),
        HirStmtKind::Dump(stmt) => v.visit_dump_stmt(ctx, stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, stmt: &HirIfStmt) {
    let cond = ctx.expr(stmt.cond_id);
    v.visit_expr(ctx, cond);
    let then = ctx.stmt(stmt.then_id);
    v.visit_stmt(ctx, &then);
    if let Some(otherwise_id) = stmt.otherwise_id {
        let otherwise = ctx.stmt(otherwise_id);
        v.visit_stmt(ctx, otherwise);
    }
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, stmt: &HirWhileStmt) {
    let cond = ctx.expr(stmt.cond_id);
    v.visit_expr(ctx, cond);
    let body = ctx.stmt(stmt.body_id);
    v.visit_stmt(ctx, &body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, stmt: &HirBeginStmt) {
    for &stmt_id in &stmt.stmt_ids {
        let stmt = ctx.stmt(stmt_id);
        v.visit_stmt(ctx, stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, stmt: &HirAssgStmt) {
    v.visit_var_ref(ctx, &stmt.var);
    let expr = ctx.expr(stmt.expr_id);
    v.visit_expr(ctx, expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, stmt: &HirDumpStmt) {
    v.visit_var_ref(ctx, &stmt.var);
}

pub fn visit_expr<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, expr: &HirExpr) {
    match &expr.kind {
        HirExprKind::UnaryOp(_, id) => {
            let expr = ctx.expr(*id);
            v.visit_expr(ctx, expr)
        }
        HirExprKind::BinOp(_, lhs_id, rhs_id) => {
            let lhs = ctx.expr(*lhs_id);
            let rhs = ctx.expr(*rhs_id);
            v.visit_expr(ctx, lhs);
            v.visit_expr(ctx, rhs);
        }
        HirExprKind::Var(var) => v.visit_var_ref(ctx, var),
        HirExprKind::Const(cst) => v.visit_const(ctx, cst),
        HirExprKind::FnCall(fncall) => v.visit_fncall(ctx, fncall),
        HirExprKind::Paren(expr_id) => {
            let expr = ctx.expr(*expr_id);
            v.visit_expr(ctx, expr)
        }
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _ctx: &HirContext, _var: &HirVarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _ctx: &HirContext, _cst: &HirConst) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, ctx: &HirContext, fncall: &HirFnCall) {
    for arg_id in &fncall.args {
        let arg = ctx.expr(*arg_id);
        v.visit_expr(ctx, arg);
    }
}
