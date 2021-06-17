use crate::hir;
use crate::rhir::*;

pub trait Visit {
    fn visit_all(&mut self, prog: &RhirProgram) {
        visit_all(self, prog);
    }

    fn visit_scope(&mut self, prog: &RhirProgram, scope: &RhirScope) {
        visit_scope(self, prog, scope);
    }

    fn visit_fndecl(&mut self, prog: &RhirProgram, fndecl: &RhirFnDecl) {
        visit_fndecl(self, prog, fndecl);
    }

    fn visit_param(&mut self, prog: &RhirProgram, param: &RhirParam) {
        visit_param(self, prog, param);
    }

    fn visit_ident(&mut self, prog: &RhirProgram, ident: &hir::Ident) {
        visit_ident(self, prog, ident);
    }

    fn visit_ty(&mut self, prog: &RhirProgram, ty: &RhirTy) {
        visit_ty(self, prog, ty);
    }

    fn visit_fnbody(&mut self, prog: &RhirProgram, fnbody: &RhirFnBody) {
        visit_fnbody(self, prog, fnbody);
    }

    fn visit_var(&mut self, prog: &RhirProgram, var: &RhirVar) {
        visit_var(self, prog, var);
    }

    fn visit_stmt(&mut self, prog: &RhirProgram, stmt: &RhirStmt) {
        visit_stmt(self, prog, stmt);
    }

    fn visit_if_stmt(&mut self, prog: &RhirProgram, stmt: &RhirIfStmt) {
        visit_if_stmt(self, prog, stmt);
    }

    fn visit_while_stmt(&mut self, prog: &RhirProgram, stmt: &RhirWhileStmt) {
        visit_while_stmt(self, prog, stmt);
    }

    fn visit_begin_stmt(&mut self, prog: &RhirProgram, stmt: &RhirBeginStmt) {
        visit_begin_stmt(self, prog, stmt);
    }

    fn visit_assg_stmt(&mut self, prog: &RhirProgram, stmt: &RhirAssgStmt) {
        visit_assg_stmt(self, prog, stmt);
    }

    fn visit_dump_stmt(&mut self, prog: &RhirProgram, stmt: &RhirDumpStmt) {
        visit_dump_stmt(self, prog, stmt);
    }

    fn visit_expr(&mut self, prog: &RhirProgram, expr: &RhirExpr) {
        visit_expr(self, prog, expr);
    }

    fn visit_fncall(&mut self, prog: &RhirProgram, fncall: &RhirFnCall) {
        visit_fncall(self, prog, fncall);
    }

    fn visit_var_ref(&mut self, prog: &RhirProgram, var: &RhirVarRef) {
        visit_var_ref(self, prog, var);
    }

    fn visit_const(&mut self, prog: &RhirProgram, cst: &RhirConst) {
        visit_const(self, prog, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram) {
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

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, fndecl: &RhirFnDecl) {
    v.visit_ident(prog, &fndecl.name);
    for param in &fndecl.params {
        v.visit_param(prog, param);
    }
    v.visit_ty(prog, &fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, param: &RhirParam) {
    v.visit_ty(prog, &param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _prog: &RhirProgram, _ident: &hir::Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _prog: &RhirProgram, _ty: &RhirTy) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, fnbody: &RhirFnBody) {
    if let RhirFnBodyKind::Stmt(stmt_id) = fnbody.kind {
        let stmt = prog.stmt(stmt_id);
        v.visit_stmt(prog, stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, scope: &RhirScope) {
    for var in scope.vars.values() {
        v.visit_var(prog, var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, var: &RhirVar) {
    v.visit_ident(prog, &var.name);
    v.visit_ty(prog, &var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, stmt: &RhirStmt) {
    match &stmt.kind {
        RhirStmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        RhirStmtKind::If(stmt) => v.visit_if_stmt(prog, stmt),
        RhirStmtKind::While(stmt) => v.visit_while_stmt(prog, stmt),
        RhirStmtKind::Begin(stmt) => v.visit_begin_stmt(prog, stmt),
        RhirStmtKind::Assg(stmt) => v.visit_assg_stmt(prog, stmt),
        RhirStmtKind::Dump(stmt) => v.visit_dump_stmt(prog, stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, stmt: &RhirIfStmt) {
    let cond = prog.expr(stmt.cond_id);
    v.visit_expr(prog, cond);
    let then = prog.stmt(stmt.then_id);
    v.visit_stmt(prog, then);
    if let Some(otherwise_id) = stmt.otherwise_id {
        let otherwise = prog.stmt(otherwise_id);
        v.visit_stmt(prog, otherwise);
    }
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, stmt: &RhirWhileStmt) {
    let cond = prog.expr(stmt.cond_id);
    v.visit_expr(prog, cond);
    let body = prog.stmt(stmt.body_id);
    v.visit_stmt(prog, body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, stmt: &RhirBeginStmt) {
    for &stmt_id in &stmt.stmt_ids {
        let stmt = prog.stmt(stmt_id);
        v.visit_stmt(prog, stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, stmt: &RhirAssgStmt) {
    v.visit_var_ref(prog, &stmt.var);
    let expr = prog.expr(stmt.expr_id);
    v.visit_expr(prog, expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, stmt: &RhirDumpStmt) {
    v.visit_var_ref(prog, &stmt.var);
}

pub fn visit_expr<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, expr: &RhirExpr) {
    match &expr.kind {
        RhirExprKind::UnaryOp(_, id) => {
            let expr = prog.expr(*id);
            v.visit_expr(prog, expr)
        }
        RhirExprKind::BinOp(_, lhs_id, rhs_id) => {
            let lhs = prog.expr(*lhs_id);
            let rhs = prog.expr(*rhs_id);
            v.visit_expr(prog, lhs);
            v.visit_expr(prog, rhs);
        }
        RhirExprKind::Var(var) => v.visit_var_ref(prog, var),
        RhirExprKind::Const(cst) => v.visit_const(prog, cst),
        RhirExprKind::FnCall(fncall) => v.visit_fncall(prog, fncall),
        RhirExprKind::Paren(expr_id) => {
            let expr = prog.expr(*expr_id);
            v.visit_expr(prog, expr)
        }
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _prog: &RhirProgram, _var: &RhirVarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _prog: &RhirProgram, _cst: &RhirConst) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram, fncall: &RhirFnCall) {
    for arg_id in &fncall.arg_ids {
        let arg = prog.expr(*arg_id);
        v.visit_expr(prog, arg);
    }
}
