use crate::hir::*;

pub trait VisitMut {
    fn visit_all(&mut self, prog: &mut Program) {
        visit_all(self, prog);
    }

    fn visit_fndecl(&mut self, fndecl: &mut FnDecl) {
        visit_fndecl(self, fndecl);
    }

    fn visit_param(&mut self, param: &mut Param) {
        visit_param(self, param);
    }

    fn visit_ident(&mut self, ident: &mut Ident) {
        visit_ident(self, ident);
    }

    fn visit_ty(&mut self, ty: &mut Ty) {
        visit_ty(self, ty);
    }

    fn visit_fnbody(&mut self, fnbody: &mut FnBody) {
        visit_fnbody(self, fnbody);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        visit_stmt(self, stmt);
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStmt) {
        visit_if_stmt(self, stmt);
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) {
        visit_while_stmt(self, stmt);
    }

    fn visit_begin_stmt(&mut self, stmt: &mut BeginStmt) {
        visit_begin_stmt(self, stmt);
    }

    fn visit_assg_stmt(&mut self, stmt: &mut AssgStmt) {
        visit_assg_stmt(self, stmt);
    }

    fn visit_dump_stmt(&mut self, stmt: &mut DumpStmt) {
        visit_dump_stmt(self, stmt);
    }

    fn visit_bool_expr(&mut self, expr: &mut BoolExpr) {
        visit_bool_expr(self, expr);
    }

    fn visit_arith_expr(&mut self, expr: &mut ArithExpr) {
        visit_arith_expr(self, expr);
    }

    fn visit_primary_expr(&mut self, expr: &mut PrimaryExpr) {
        visit_primary_expr(self, expr);
    }

    fn visit_fncall(&mut self, fncall: &mut FnCall) {
        visit_fncall(self, fncall);
    }

    fn visit_var(&mut self, var: &mut Var) {
        visit_var(self, var);
    }

    fn visit_const(&mut self, cst: &mut Const) {
        visit_const(self, cst);
    }
}

pub fn visit_all<V: VisitMut + ?Sized>(v: &mut V, prog: &mut Program) {
    for &scope in prog.scopes.keys() {
        for &fn_id in &prog.scope_mut(scope).fn_ids {
            let fndecl = prog.fndecl_mut(fn_id);
            v.visit_fndecl(fndecl);
            let fnbody = prog.fnbody_mut(fn_id);
            v.visit_fnbody(fnbody);
        }
    }
}

pub fn visit_fndecl<V: VisitMut + ?Sized>(v: &mut V, fndecl: &mut FnDecl) {
    v.visit_ident(&mut fndecl.name);
    for param in &mut fndecl.params {
        v.visit_param(param);
    }
    v.visit_ty(&mut fndecl.ret_ty);
}

pub fn visit_param<V: VisitMut + ?Sized>(v: &mut V, param: &mut Param) {
    v.visit_ident(&mut param.name);
    v.visit_ty(&mut param.ty);
}

pub fn visit_ident<V: VisitMut + ?Sized>(v: &mut V, ident: &mut Ident) {}

pub fn visit_ty<V: VisitMut + ?Sized>(v: &mut V, ty: &mut Ty) {}

pub fn visit_fnbody<V: VisitMut + ?Sized>(v: &mut V, fnbody: &mut FnBody) {
    v.visit_begin_stmt(&mut *fnbody.stmt)
}

pub fn visit_stmt<V: VisitMut + ?Sized>(v: &mut V, stmt: &mut Stmt) {
    match &mut stmt.kind {
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

pub fn visit_if_stmt<V: VisitMut + ?Sized>(v: &mut V, stmt: &mut IfStmt) {
    v.visit_bool_expr(&mut stmt.cond);
    v.visit_stmt(&mut stmt.then);
    v.visit_stmt(&mut stmt.otherwise);
}

pub fn visit_while_stmt<V: VisitMut + ?Sized>(v: &mut V, stmt: &mut WhileStmt) {
    v.visit_bool_expr(&mut stmt.cond);
    v.visit_stmt(&mut stmt.body);
}

pub fn visit_begin_stmt<V: VisitMut + ?Sized>(v: &mut V, stmt: &mut BeginStmt) {
    for stmt in &mut stmt.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_assg_stmt<V: VisitMut + ?Sized>(v: &mut V, stmt: &mut AssgStmt) {
    v.visit_var(&mut stmt.var);
}

pub fn visit_dump_stmt<V: VisitMut + ?Sized>(v: &mut V, stmt: &mut DumpStmt) {
    v.visit_var(&mut stmt.var);
}

pub fn visit_bool_expr<V: VisitMut + ?Sized>(v: &mut V, expr: &mut BoolExpr) {
    v.visit_arith_expr(&mut expr.lhs);
    v.visit_arith_expr(&mut expr.rhs);
}

pub fn visit_arith_expr<V: VisitMut + ?Sized>(v: &mut V, expr: &mut ArithExpr) {
    match &mut expr.kind {
        ArithExprKind::Primary(e) => v.visit_primary_expr(e),
        ArithExprKind::UnaryOp(_, e) => v.visit_arith_expr(e),
        ArithExprKind::BinOp(_, lhs, rhs) => {
            v.visit_arith_expr(lhs);
            v.visit_arith_expr(rhs);
        }
    }
}

pub fn visit_primary_expr<V: VisitMut + ?Sized>(v: &mut V, expr: &mut PrimaryExpr) {
    match &mut expr.kind {
        PrimaryExprKind::Var(var) => v.visit_var(var),
        PrimaryExprKind::Const(cst) => v.visit_const(cst),
        PrimaryExprKind::FnCall(fncall) => v.visit_fncall(fncall),
        PrimaryExprKind::Paren(expr) => v.visit_arith_expr(expr),
    }
}

pub fn visit_var<V: VisitMut + ?Sized>(v: &mut V, var: &mut Var) {}

pub fn visit_const<V: VisitMut + ?Sized>(v: &mut V, cst: &mut Const) {}

pub fn visit_fncall<V: VisitMut + ?Sized>(v: &mut V, fncall: &mut FnCall) {
    for arg in &mut fncall.args {
        v.visit_arith_expr(arg);
    }
}
