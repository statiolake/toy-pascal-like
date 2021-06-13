use crate::hir;
use crate::rhir::*;

pub trait Visit {
    fn visit_all(&mut self, prog: &RhirProgram) {
        visit_all(self, prog);
    }

    fn visit_scope(&mut self, scope: &RhirScope) {
        visit_scope(self, scope);
    }

    fn visit_fndecl(&mut self, fndecl: &RhirFnDecl) {
        visit_fndecl(self, fndecl);
    }

    fn visit_param(&mut self, param: &RhirParam) {
        visit_param(self, param);
    }

    fn visit_ident(&mut self, ident: &hir::Ident) {
        visit_ident(self, ident);
    }

    fn visit_ty(&mut self, ty: &RhirTy) {
        visit_ty(self, ty);
    }

    fn visit_fnbody(&mut self, fnbody: &RhirFnBody) {
        visit_fnbody(self, fnbody);
    }

    fn visit_var(&mut self, var: &RhirVar) {
        visit_var(self, var);
    }

    fn visit_stmt(&mut self, stmt: &RhirStmt) {
        visit_stmt(self, stmt);
    }

    fn visit_if_stmt(&mut self, stmt: &RhirIfStmt) {
        visit_if_stmt(self, stmt);
    }

    fn visit_while_stmt(&mut self, stmt: &RhirWhileStmt) {
        visit_while_stmt(self, stmt);
    }

    fn visit_begin_stmt(&mut self, stmt: &RhirBeginStmt) {
        visit_begin_stmt(self, stmt);
    }

    fn visit_assg_stmt(&mut self, stmt: &RhirAssgStmt) {
        visit_assg_stmt(self, stmt);
    }

    fn visit_dump_stmt(&mut self, stmt: &RhirDumpStmt) {
        visit_dump_stmt(self, stmt);
    }

    fn visit_arith_expr(&mut self, expr: &RhirArithExpr) {
        visit_arith_expr(self, expr);
    }

    fn visit_primary_expr(&mut self, expr: &RhirPrimaryExpr) {
        visit_primary_expr(self, expr);
    }

    fn visit_fncall(&mut self, fncall: &RhirFnCall) {
        visit_fncall(self, fncall);
    }

    fn visit_var_ref(&mut self, var: &RhirVarRef) {
        visit_var_ref(self, var);
    }

    fn visit_const(&mut self, cst: &RhirConst) {
        visit_const(self, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, prog: &RhirProgram) {
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

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, fndecl: &RhirFnDecl) {
    v.visit_ident(&fndecl.name);
    for param in &fndecl.params {
        v.visit_param(param);
    }
    v.visit_ty(&fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, param: &RhirParam) {
    v.visit_ty(&param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _ident: &hir::Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _ty: &RhirTy) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, fnbody: &RhirFnBody) {
    if let RhirFnBodyKind::Stmt(stmt) = &fnbody.kind {
        v.visit_begin_stmt(stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, scope: &RhirScope) {
    for var in scope.vars.values() {
        v.visit_var(var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, var: &RhirVar) {
    v.visit_ident(&var.name);
    v.visit_ty(&var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &RhirStmt) {
    match &stmt.kind {
        RhirStmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        RhirStmtKind::If(stmt) => v.visit_if_stmt(stmt),
        RhirStmtKind::While(stmt) => v.visit_while_stmt(stmt),
        RhirStmtKind::Begin(stmt) => v.visit_begin_stmt(stmt),
        RhirStmtKind::Assg(stmt) => v.visit_assg_stmt(stmt),
        RhirStmtKind::Dump(stmt) => v.visit_dump_stmt(stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &RhirIfStmt) {
    v.visit_arith_expr(&stmt.cond);
    v.visit_stmt(&stmt.then);
    v.visit_stmt(&stmt.otherwise);
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &RhirWhileStmt) {
    v.visit_arith_expr(&stmt.cond);
    v.visit_stmt(&stmt.body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &RhirBeginStmt) {
    for stmt in &stmt.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &RhirAssgStmt) {
    v.visit_var_ref(&stmt.var);
    v.visit_arith_expr(&stmt.expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &RhirDumpStmt) {
    v.visit_var_ref(&stmt.var);
}

pub fn visit_arith_expr<V: Visit + ?Sized>(v: &mut V, expr: &RhirArithExpr) {
    match &expr.kind {
        RhirArithExprKind::Primary(e) => v.visit_primary_expr(e),
        RhirArithExprKind::UnaryOp(_, e) => v.visit_arith_expr(e),
        RhirArithExprKind::BinOp(_, lhs, rhs) => {
            v.visit_arith_expr(lhs);
            v.visit_arith_expr(rhs);
        }
    }
}

pub fn visit_primary_expr<V: Visit + ?Sized>(v: &mut V, expr: &RhirPrimaryExpr) {
    match &expr.kind {
        RhirPrimaryExprKind::Var(var) => v.visit_var_ref(var),
        RhirPrimaryExprKind::Const(cst) => v.visit_const(cst),
        RhirPrimaryExprKind::FnCall(fncall) => v.visit_fncall(fncall),
        RhirPrimaryExprKind::Paren(expr) => v.visit_arith_expr(expr),
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _var: &RhirVarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _cst: &RhirConst) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, fncall: &RhirFnCall) {
    for arg in &fncall.args {
        v.visit_arith_expr(arg);
    }
}
