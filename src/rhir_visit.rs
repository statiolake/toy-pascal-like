use crate::hir;
use crate::rhir::*;

pub trait Visit {
    fn visit_all(&mut self, ctx: &RhirContext) {
        visit_all(self, ctx);
    }

    fn visit_scope(&mut self, ctx: &RhirContext, scope: &RhirScope) {
        visit_scope(self, ctx, scope);
    }

    fn visit_fndecl(&mut self, ctx: &RhirContext, fndecl: &RhirFnDecl) {
        visit_fndecl(self, ctx, fndecl);
    }

    fn visit_param(&mut self, ctx: &RhirContext, param: &RhirParam) {
        visit_param(self, ctx, param);
    }

    fn visit_ident(&mut self, ctx: &RhirContext, ident: &hir::Ident) {
        visit_ident(self, ctx, ident);
    }

    fn visit_ty(&mut self, ctx: &RhirContext, ty: &RhirTy) {
        visit_ty(self, ctx, ty);
    }

    fn visit_fnbody(&mut self, ctx: &RhirContext, fnbody: &RhirFnBody) {
        visit_fnbody(self, ctx, fnbody);
    }

    fn visit_var(&mut self, ctx: &RhirContext, var: &RhirVar) {
        visit_var(self, ctx, var);
    }

    fn visit_stmt(&mut self, ctx: &RhirContext, stmt: &RhirStmt) {
        visit_stmt(self, ctx, stmt);
    }

    fn visit_if_stmt(&mut self, ctx: &RhirContext, stmt: &RhirIfStmt) {
        visit_if_stmt(self, ctx, stmt);
    }

    fn visit_while_stmt(&mut self, ctx: &RhirContext, stmt: &RhirWhileStmt) {
        visit_while_stmt(self, ctx, stmt);
    }

    fn visit_begin_stmt(&mut self, ctx: &RhirContext, stmt: &RhirBeginStmt) {
        visit_begin_stmt(self, ctx, stmt);
    }

    fn visit_assg_stmt(&mut self, ctx: &RhirContext, stmt: &RhirAssgStmt) {
        visit_assg_stmt(self, ctx, stmt);
    }

    fn visit_dump_stmt(&mut self, ctx: &RhirContext, stmt: &RhirDumpStmt) {
        visit_dump_stmt(self, ctx, stmt);
    }

    fn visit_expr(&mut self, ctx: &RhirContext, expr: &RhirExpr) {
        visit_expr(self, ctx, expr);
    }

    fn visit_fncall(&mut self, ctx: &RhirContext, fncall: &RhirFnCall) {
        visit_fncall(self, ctx, fncall);
    }

    fn visit_var_ref(&mut self, ctx: &RhirContext, var: &RhirVarRef) {
        visit_var_ref(self, ctx, var);
    }

    fn visit_const(&mut self, ctx: &RhirContext, cst: &RhirConst) {
        visit_const(self, ctx, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext) {
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

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, fndecl: &RhirFnDecl) {
    v.visit_ident(ctx, &fndecl.name);
    for param in &fndecl.params {
        v.visit_param(ctx, param);
    }
    v.visit_ty(ctx, &fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, param: &RhirParam) {
    v.visit_ty(ctx, &param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _ctx: &RhirContext, _ident: &hir::Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _ctx: &RhirContext, _ty: &RhirTy) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, fnbody: &RhirFnBody) {
    if let RhirFnBodyKind::Stmt(stmt_id) = fnbody.kind {
        let stmt = ctx.stmt(stmt_id);
        v.visit_stmt(ctx, stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, scope: &RhirScope) {
    for var in scope.vars.values() {
        v.visit_var(ctx, var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, var: &RhirVar) {
    v.visit_ident(ctx, &var.name);
    v.visit_ty(ctx, &var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, stmt: &RhirStmt) {
    match &stmt.kind {
        RhirStmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        RhirStmtKind::If(stmt) => v.visit_if_stmt(ctx, stmt),
        RhirStmtKind::While(stmt) => v.visit_while_stmt(ctx, stmt),
        RhirStmtKind::Begin(stmt) => v.visit_begin_stmt(ctx, stmt),
        RhirStmtKind::Assg(stmt) => v.visit_assg_stmt(ctx, stmt),
        RhirStmtKind::Dump(stmt) => v.visit_dump_stmt(ctx, stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, stmt: &RhirIfStmt) {
    let cond = ctx.expr(stmt.cond_id);
    v.visit_expr(ctx, cond);
    let then = ctx.stmt(stmt.then_id);
    v.visit_stmt(ctx, then);
    if let Some(otherwise_id) = stmt.otherwise_id {
        let otherwise = ctx.stmt(otherwise_id);
        v.visit_stmt(ctx, otherwise);
    }
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, stmt: &RhirWhileStmt) {
    let cond = ctx.expr(stmt.cond_id);
    v.visit_expr(ctx, cond);
    let body = ctx.stmt(stmt.body_id);
    v.visit_stmt(ctx, body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, stmt: &RhirBeginStmt) {
    for &stmt_id in &stmt.stmt_ids {
        let stmt = ctx.stmt(stmt_id);
        v.visit_stmt(ctx, stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, stmt: &RhirAssgStmt) {
    v.visit_var_ref(ctx, &stmt.var);
    let expr = ctx.expr(stmt.expr_id);
    v.visit_expr(ctx, expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, stmt: &RhirDumpStmt) {
    v.visit_var_ref(ctx, &stmt.var);
}

pub fn visit_expr<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, expr: &RhirExpr) {
    match &expr.kind {
        RhirExprKind::UnaryOp(_, id) => {
            let expr = ctx.expr(*id);
            v.visit_expr(ctx, expr)
        }
        RhirExprKind::BinOp(_, lhs_id, rhs_id) => {
            let lhs = ctx.expr(*lhs_id);
            let rhs = ctx.expr(*rhs_id);
            v.visit_expr(ctx, lhs);
            v.visit_expr(ctx, rhs);
        }
        RhirExprKind::Var(var) => v.visit_var_ref(ctx, var),
        RhirExprKind::Const(cst) => v.visit_const(ctx, cst),
        RhirExprKind::FnCall(fncall) => v.visit_fncall(ctx, fncall),
        RhirExprKind::Paren(expr_id) => {
            let expr = ctx.expr(*expr_id);
            v.visit_expr(ctx, expr)
        }
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _ctx: &RhirContext, _var: &RhirVarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _ctx: &RhirContext, _cst: &RhirConst) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, ctx: &RhirContext, fncall: &RhirFnCall) {
    for arg_id in &fncall.arg_ids {
        let arg = ctx.expr(*arg_id);
        v.visit_expr(ctx, arg);
    }
}
