use std::collections::HashMap;

use crate::ast::*;

pub fn run(stmt: &AstStmt) -> State {
    let mut state = State::new();
    state.run_stmt(stmt);
    state
}

pub struct State {
    vars: HashMap<String, i32>,
}

impl State {
    pub fn display(&self) {
        for (name, value) in self.variables() {
            println!("{} = {}", name, value);
        }
    }

    pub fn variables(&self) -> &HashMap<String, i32> {
        &self.vars
    }

    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn run_stmt(&mut self, stmt: &AstStmt) {
        match stmt {
            AstStmt::IfStmt(stmt) => self.run_if_stmt(stmt),
            AstStmt::WhileStmt(stmt) => self.run_while_stmt(stmt),
            AstStmt::BeginStmt(stmt) => self.run_begin_stmt(stmt),
            AstStmt::AssgStmt(stmt) => self.run_assg_stmt(stmt),
            AstStmt::DumpStmt(stmt) => self.run_dump_stmt(stmt),
        }
    }

    fn run_if_stmt(&mut self, stmt: &AstIfStmt) {
        if self.eval_bool_expr(&stmt.cond) {
            self.run_stmt(&stmt.then);
        } else {
            self.run_stmt(&stmt.otherwise);
        }
    }

    fn run_while_stmt(&mut self, stmt: &AstWhileStmt) {
        while self.eval_bool_expr(&stmt.cond) {
            self.run_stmt(&stmt.body);
        }
    }

    fn run_begin_stmt(&mut self, stmt: &AstBeginStmt) {
        self.run_stmt_list(&stmt.list);
    }

    fn run_stmt_list(&mut self, list: &AstStmtList) {
        self.run_stmt(&list.stmt);
        if let Some(stmt) = &list.next {
            self.run_stmt_list(stmt);
        }
    }

    fn run_assg_stmt(&mut self, stmt: &AstAssgStmt) {
        let name = stmt.var.ident();
        let value = self.eval_arith_expr(&stmt.expr);
        self.vars.insert(name, value);
    }

    fn run_dump_stmt(&mut self, stmt: &AstDumpStmt) {
        let name = stmt.var.ident();
        let value = *self
            .vars
            .get(&name)
            .unwrap_or_else(|| panic!("undeclared variable: {}", name));
        println!("dump: {} = {}", name, value);
    }

    fn eval_bool_expr(&mut self, expr: &AstBoolExpr) -> bool {
        let lhs = self.eval_arith_expr(&expr.lhs);
        let rhs = self.eval_arith_expr(&expr.rhs);
        match &*expr.op {
            AstCompareOp::Lt => lhs < rhs,
            AstCompareOp::Gt => lhs > rhs,
            AstCompareOp::Le => lhs <= rhs,
            AstCompareOp::Ge => lhs >= rhs,
            AstCompareOp::Eq => lhs == rhs,
            AstCompareOp::Ne => lhs != rhs,
        }
    }

    fn eval_arith_expr(&mut self, expr: &AstArithExpr) -> i32 {
        match expr {
            AstArithExpr::Var(var) => *self
                .vars
                .get(&var.ident())
                .unwrap_or_else(|| panic!("undeclared variable: {}", var.ident())),
            AstArithExpr::Const(value) => value.value(),
            AstArithExpr::Op { lhs, op, rhs } => {
                let lhs = self.eval_arith_expr(lhs);
                let rhs = self.eval_arith_expr(rhs);
                match **op {
                    AstArithOp::Add => lhs + rhs,
                    AstArithOp::Sub => lhs - rhs,
                    AstArithOp::Mul => lhs * rhs,
                    AstArithOp::Div => lhs / rhs,
                }
            }
        }
    }
}
