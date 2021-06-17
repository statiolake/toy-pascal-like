use crate::hir::*;

pub trait Visit {
    fn visit_all(&mut self, prog: &HirProgram) {
        visit_all(self, prog);
    }

    fn visit_scope(&mut self, prog: &HirProgram, scope: &HirScope) {
        visit_scope(self, prog, scope);
    }

    fn visit_fndecl(&mut self, prog: &HirProgram, fndecl: &HirFnDecl) {
        visit_fndecl(self, prog, fndecl);
    }

    fn visit_param(&mut self, prog: &HirProgram, param: &HirParam) {
        visit_param(self, prog, param);
    }

    fn visit_ident(&mut self, prog: &HirProgram, ident: &Ident) {
        visit_ident(self, prog, ident);
    }

    fn visit_ty(&mut self, prog: &HirProgram, ty: &HirTy) {
        visit_ty(self, prog, ty);
    }

    fn visit_fnbody(&mut self, prog: &HirProgram, fnbody: &HirFnBody) {
        visit_fnbody(self, prog, fnbody);
    }

    fn visit_var(&mut self, prog: &HirProgram, var: &HirVar) {
        visit_var(self, prog, var);
    }

    fn visit_stmt(&mut self, prog: &HirProgram, stmt: &HirStmt) {
        visit_stmt(self, prog, stmt);
    }

    fn visit_if_stmt(&mut self, prog: &HirProgram, stmt: &HirIfStmt) {
        visit_if_stmt(self, prog, stmt);
    }

    fn visit_while_stmt(&mut self, prog: &HirProgram, stmt: &HirWhileStmt) {
        visit_while_stmt(self, prog, stmt);
    }

    fn visit_begin_stmt(&mut self, prog: &HirProgram, stmt: &HirBeginStmt) {
        visit_begin_stmt(self, prog, stmt);
    }

    fn visit_assg_stmt(&mut self, prog: &HirProgram, stmt: &HirAssgStmt) {
        visit_assg_stmt(self, prog, stmt);
    }

    fn visit_dump_stmt(&mut self, prog: &HirProgram, stmt: &HirDumpStmt) {
        visit_dump_stmt(self, prog, stmt);
    }

    fn visit_expr(&mut self, prog: &HirProgram, expr: &HirExpr) {
        visit_expr(self, prog, expr);
    }

    fn visit_fncall(&mut self, prog: &HirProgram, fncall: &HirFnCall) {
        visit_fncall(self, prog, fncall);
    }

    fn visit_var_ref(&mut self, prog: &HirProgram, var: &HirVarRef) {
        visit_var_ref(self, prog, var);
    }

    fn visit_const(&mut self, prog: &HirProgram, cst: &HirConst) {
        visit_const(self, prog, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram) {
    for scope in prog.scopes.values() {
        v.visit_scope(prog, scope);
    }

    for fndecl in prog.fndecls.values() {
        v.visit_fndecl(prog, fndecl);
    }

    for fnbody in prog.fnbodies.values() {
        v.visit_fnbody(prog, fnbody);
    }
}

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, fndecl: &HirFnDecl) {
    v.visit_ident(prog, &fndecl.name);
    for param in &fndecl.params {
        v.visit_param(prog, param);
    }
    v.visit_ty(prog, &fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, param: &HirParam) {
    v.visit_ty(prog, &param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _prog: &HirProgram, _ident: &Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _prog: &HirProgram, _ty: &HirTy) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, fnbody: &HirFnBody) {
    if let HirFnBodyKind::Stmt(stmt_id) = fnbody.kind {
        let stmt = prog.stmt(stmt_id);
        v.visit_stmt(prog, stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, scope: &HirScope) {
    for var in scope.vars.values() {
        v.visit_var(prog, var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, var: &HirVar) {
    v.visit_ident(prog, &var.name);
    v.visit_ty(prog, &var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, stmt: &HirStmt) {
    match &stmt.kind {
        HirStmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        HirStmtKind::If(stmt) => v.visit_if_stmt(prog, stmt),
        HirStmtKind::While(stmt) => v.visit_while_stmt(prog, stmt),
        HirStmtKind::Begin(stmt) => v.visit_begin_stmt(prog, stmt),
        HirStmtKind::Assg(stmt) => v.visit_assg_stmt(prog, stmt),
        HirStmtKind::Dump(stmt) => v.visit_dump_stmt(prog, stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, stmt: &HirIfStmt) {
    let cond = prog.expr(stmt.cond_id);
    v.visit_expr(prog, cond);
    let then = prog.stmt(stmt.then_id);
    v.visit_stmt(prog, &then);
    if let Some(otherwise_id) = stmt.otherwise_id {
        let otherwise = prog.stmt(otherwise_id);
        v.visit_stmt(prog, otherwise);
    }
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, stmt: &HirWhileStmt) {
    let cond = prog.expr(stmt.cond_id);
    v.visit_expr(prog, cond);
    let body = prog.stmt(stmt.body_id);
    v.visit_stmt(prog, &body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, stmt: &HirBeginStmt) {
    for &stmt_id in &stmt.stmt_ids {
        let stmt = prog.stmt(stmt_id);
        v.visit_stmt(prog, stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, stmt: &HirAssgStmt) {
    v.visit_var_ref(prog, &stmt.var);
    let expr = prog.expr(stmt.expr_id);
    v.visit_expr(prog, expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, stmt: &HirDumpStmt) {
    v.visit_var_ref(prog, &stmt.var);
}

pub fn visit_expr<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, expr: &HirExpr) {
    match &expr.kind {
        HirExprKind::UnaryOp(_, id) => {
            let expr = prog.expr(*id);
            v.visit_expr(prog, expr)
        }
        HirExprKind::BinOp(_, lhs_id, rhs_id) => {
            let lhs = prog.expr(*lhs_id);
            let rhs = prog.expr(*rhs_id);
            v.visit_expr(prog, lhs);
            v.visit_expr(prog, rhs);
        }
        HirExprKind::Var(var) => v.visit_var_ref(prog, var),
        HirExprKind::Const(cst) => v.visit_const(prog, cst),
        HirExprKind::FnCall(fncall) => v.visit_fncall(prog, fncall),
        HirExprKind::Paren(expr_id) => {
            let expr = prog.expr(*expr_id);
            v.visit_expr(prog, expr)
        }
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _prog: &HirProgram, _var: &HirVarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _prog: &HirProgram, _cst: &HirConst) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram, fncall: &HirFnCall) {
    for arg_id in &fncall.args {
        let arg = prog.expr(*arg_id);
        v.visit_expr(prog, arg);
    }
}
