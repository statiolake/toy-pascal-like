use crate::hir;
use crate::rhir::*;

pub trait Visit {
    fn visit_all(&mut self, prog: &Program) {
        visit_all(self, prog);
    }

    fn visit_scope(&mut self, scope: &Scope) {
        visit_scope(self, scope);
    }

    fn visit_fndecl(&mut self, fndecl: &FnDecl) {
        visit_fndecl(self, fndecl);
    }

    fn visit_param(&mut self, param: &Param) {
        visit_param(self, param);
    }

    fn visit_ident(&mut self, ident: &hir::Ident) {
        visit_ident(self, ident);
    }

    fn visit_ty(&mut self, ty: &Ty) {
        visit_ty(self, ty);
    }

    fn visit_fnbody(&mut self, fnbody: &FnBody) {
        visit_fnbody(self, fnbody);
    }

    fn visit_var(&mut self, var: &Var) {
        visit_var(self, var);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        visit_stmt(self, stmt);
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) {
        visit_if_stmt(self, stmt);
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) {
        visit_while_stmt(self, stmt);
    }

    fn visit_begin_stmt(&mut self, stmt: &BeginStmt) {
        visit_begin_stmt(self, stmt);
    }

    fn visit_assg_stmt(&mut self, stmt: &AssgStmt) {
        visit_assg_stmt(self, stmt);
    }

    fn visit_dump_stmt(&mut self, stmt: &DumpStmt) {
        visit_dump_stmt(self, stmt);
    }

    fn visit_bool_expr(&mut self, expr: &BoolExpr) {
        visit_bool_expr(self, expr);
    }

    fn visit_arith_expr(&mut self, expr: &ArithExpr) {
        visit_arith_expr(self, expr);
    }

    fn visit_primary_expr(&mut self, expr: &PrimaryExpr) {
        visit_primary_expr(self, expr);
    }

    fn visit_fncall(&mut self, fncall: &FnCall) {
        visit_fncall(self, fncall);
    }

    fn visit_var_ref(&mut self, var: &VarRef) {
        visit_var_ref(self, var);
    }

    fn visit_const(&mut self, cst: &Const) {
        visit_const(self, cst);
    }
}

pub fn visit_all<V: Visit + ?Sized>(v: &mut V, prog: &Program) {
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

pub fn visit_fndecl<V: Visit + ?Sized>(v: &mut V, fndecl: &FnDecl) {
    v.visit_ident(&fndecl.name);
    for param in &fndecl.params {
        v.visit_param(param);
    }
    v.visit_ty(&fndecl.ret_ty);
}

pub fn visit_param<V: Visit + ?Sized>(v: &mut V, param: &Param) {
    v.visit_ident(&param.name);
    v.visit_ty(&param.ty);
}

pub fn visit_ident<V: Visit + ?Sized>(_v: &mut V, _ident: &hir::Ident) {}

pub fn visit_ty<V: Visit + ?Sized>(_v: &mut V, _ty: &Ty) {}

pub fn visit_fnbody<V: Visit + ?Sized>(v: &mut V, fnbody: &FnBody) {
    if let FnBodyKind::Stmt(stmt) = &fnbody.kind {
        v.visit_begin_stmt(stmt)
    }
}

pub fn visit_scope<V: Visit + ?Sized>(v: &mut V, scope: &Scope) {
    for var in scope.vars.values() {
        v.visit_var(var);
    }
}

pub fn visit_var<V: Visit + ?Sized>(v: &mut V, var: &Var) {
    v.visit_ident(&var.name);
    v.visit_ty(&var.ty);
}

pub fn visit_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::FnDef(_) => {
            // We need to do nothing because other funcdefs are separately resolved.
        }
        StmtKind::If(stmt) => v.visit_if_stmt(stmt),
        StmtKind::While(stmt) => v.visit_while_stmt(stmt),
        StmtKind::Begin(stmt) => v.visit_begin_stmt(stmt),
        StmtKind::Assg(stmt) => v.visit_assg_stmt(stmt),
        StmtKind::Dump(stmt) => v.visit_dump_stmt(stmt),
    }
}

pub fn visit_if_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &IfStmt) {
    v.visit_bool_expr(&stmt.cond);
    v.visit_stmt(&stmt.then);
    v.visit_stmt(&stmt.otherwise);
}

pub fn visit_while_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &WhileStmt) {
    v.visit_bool_expr(&stmt.cond);
    v.visit_stmt(&stmt.body);
}

pub fn visit_begin_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &BeginStmt) {
    for stmt in &stmt.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_assg_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &AssgStmt) {
    v.visit_var_ref(&stmt.var);
    v.visit_arith_expr(&stmt.expr);
}

pub fn visit_dump_stmt<V: Visit + ?Sized>(v: &mut V, stmt: &DumpStmt) {
    v.visit_var_ref(&stmt.var);
}

pub fn visit_bool_expr<V: Visit + ?Sized>(v: &mut V, expr: &BoolExpr) {
    v.visit_arith_expr(&expr.lhs);
    v.visit_arith_expr(&expr.rhs);
}

pub fn visit_arith_expr<V: Visit + ?Sized>(v: &mut V, expr: &ArithExpr) {
    match &expr.kind {
        ArithExprKind::Primary(e) => v.visit_primary_expr(e),
        ArithExprKind::UnaryOp(_, e) => v.visit_arith_expr(e),
        ArithExprKind::BinOp(_, lhs, rhs) => {
            v.visit_arith_expr(lhs);
            v.visit_arith_expr(rhs);
        }
    }
}

pub fn visit_primary_expr<V: Visit + ?Sized>(v: &mut V, expr: &PrimaryExpr) {
    match &expr.kind {
        PrimaryExprKind::Var(var) => v.visit_var_ref(var),
        PrimaryExprKind::Const(cst) => v.visit_const(cst),
        PrimaryExprKind::FnCall(fncall) => v.visit_fncall(fncall),
        PrimaryExprKind::Paren(expr) => v.visit_arith_expr(expr),
    }
}

pub fn visit_var_ref<V: Visit + ?Sized>(_v: &mut V, _var: &VarRef) {}

pub fn visit_const<V: Visit + ?Sized>(_v: &mut V, _cst: &Const) {}

pub fn visit_fncall<V: Visit + ?Sized>(v: &mut V, fncall: &FnCall) {
    for arg in &fncall.args {
        v.visit_arith_expr(arg);
    }
}
