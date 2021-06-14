use crate::hir::*;

pub trait Visit {
    fn visit_all(&mut self, prog: &HirProgram) {
        visit_all(self, prog);
    }

    fn visit_scope(&mut self, scope: &HirScope) {
        visit_scope(self, scope);
    }

    fn visit_fndecl(&mut self, fndecl: &HirFnDecl) {
        visit_fndecl(self, fndecl);
    }

    fn visit_param(&mut self, param: &HirParam) {
        visit_param(self, param);
    }

    fn visit_ident(&mut self, ident: &Ident) {
        visit_ident(self, ident);
    }

    fn visit_ty(&mut self, ty: &HirTy) {
        visit_ty(self, ty);
    }

    fn visit_fnbody(&mut self, fnbody: &HirFnBody) {
        visit_fnbody(self, fnbody);
    }

    fn visit_var(&mut self, var: &HirVar) {
        visit_var(self, var);
    }

    fn visit_stmt(&mut self, stmt: &HirStmt) {
        visit_stmt(self, stmt);
    }

    fn visit_if_stmt(&mut self, stmt: &HirIfStmt) {
        visit_if_stmt(self, stmt);
    }

    fn visit_while_stmt(&mut self, stmt: &HirWhileStmt) {
        visit_while_stmt(self, stmt);
    }

    fn visit_begin_stmt(&mut self, stmt: &HirBeginStmt) {
        visit_begin_stmt(self, stmt);
    }

    fn visit_assg_stmt(&mut self, stmt: &HirAssgStmt) {
        visit_assg_stmt(self, stmt);
    }

    fn visit_dump_stmt(&mut self, stmt: &HirDumpStmt) {
        visit_dump_stmt(self, stmt);
    }

    fn visit_arith_expr(&mut self, expr: &HirArithExpr) {
        visit_arith_expr(self, expr);
    }

    fn visit_primary_expr(&mut self, expr: &HirPrimaryExpr) {
        visit_primary_expr(self, expr);
    }

    fn visit_fncall(&mut self, fncall: &HirFnCall) {
        visit_fncall(self, fncall);
    }

    fn visit_var_ref(&mut self, var: &HirVarRef) {
        visit_var_ref(self, var);
    }

    fn visit_const(&mut self, cst: &HirConst) {
        visit_const(self, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, prog: &HirProgram) {
    for scope in prog.scopes.values() {
        v.visit_scope(scope);
    }

    for fndecl in prog.fndecls.values() {
        v.visit_fndecl(fndecl);
    }

    for fnbody in prog.fnbodies.values() {
        v.visit_fnbody(fnbody);
    }
}

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, fndecl: &HirFnDecl) {
    v.visit_ident(&fndecl.name);
    for param in &fndecl.params {
        v.visit_param(param);
    }
    v.visit_ty(&fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, param: &HirParam) {
    v.visit_ty(&param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _ident: &Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _ty: &HirTy) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, fnbody: &HirFnBody) {
    if let HirFnBodyKind::Stmt(stmt) = &fnbody.kind {
        v.visit_begin_stmt(stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, scope: &HirScope) {
    for var in scope.vars.values() {
        v.visit_var(var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, var: &HirVar) {
    v.visit_ident(&var.name);
    v.visit_ty(&var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &HirStmt) {
    match &stmt.kind {
        HirStmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        HirStmtKind::If(stmt) => v.visit_if_stmt(stmt),
        HirStmtKind::While(stmt) => v.visit_while_stmt(stmt),
        HirStmtKind::Begin(stmt) => v.visit_begin_stmt(stmt),
        HirStmtKind::Assg(stmt) => v.visit_assg_stmt(stmt),
        HirStmtKind::Dump(stmt) => v.visit_dump_stmt(stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &HirIfStmt) {
    v.visit_arith_expr(&stmt.cond);
    v.visit_stmt(&stmt.then);
    if let Some(otherwise) = &stmt.otherwise {
        v.visit_stmt(otherwise);
    }
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &HirWhileStmt) {
    v.visit_arith_expr(&stmt.cond);
    v.visit_stmt(&stmt.body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &HirBeginStmt) {
    for stmt in &stmt.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &HirAssgStmt) {
    v.visit_var_ref(&stmt.var);
    v.visit_arith_expr(&stmt.expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &HirDumpStmt) {
    v.visit_var_ref(&stmt.var);
}

pub fn visit_arith_expr<V: Visit + ?Sized>(v: &mut V, expr: &HirArithExpr) {
    match &expr.kind {
        HirArithExprKind::Primary(e) => v.visit_primary_expr(e),
        HirArithExprKind::UnaryOp(_, e) => v.visit_arith_expr(e),
        HirArithExprKind::BinOp(_, lhs, rhs) => {
            v.visit_arith_expr(lhs);
            v.visit_arith_expr(rhs);
        }
    }
}

pub fn visit_primary_expr<V: Visit + ?Sized>(v: &mut V, expr: &HirPrimaryExpr) {
    match &expr.kind {
        HirPrimaryExprKind::Var(var) => v.visit_var_ref(var),
        HirPrimaryExprKind::Const(cst) => v.visit_const(cst),
        HirPrimaryExprKind::FnCall(fncall) => v.visit_fncall(fncall),
        HirPrimaryExprKind::Paren(expr) => v.visit_arith_expr(expr),
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _var: &HirVarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _cst: &HirConst) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, fncall: &HirFnCall) {
    for arg in &fncall.args {
        v.visit_arith_expr(arg);
    }
}
