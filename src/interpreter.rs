use crate::ast::*;
use rand::prelude::*;
use std::collections::HashMap;
use std::io;

pub fn run(stmt: &AstStmt) -> State {
    let mut state = State::defaultenv();
    state.run_stmt(stmt);
    state
}

pub struct State {
    variables: HashMap<String, i32>,
    functions: HashMap<String, Function>,
}

struct Function {
    name: String,
    arity: usize,
    body: Box<dyn Fn(&[i32]) -> i32>,
}

impl Function {
    pub fn new(name: String, arity: usize, body: Box<dyn Fn(&[i32]) -> i32>) -> Self {
        Self { name, arity, body }
    }

    pub fn call(&self, args: &[i32]) -> i32 {
        if self.arity != args.len() {
            panic!(
                "arity mismatch: function {} requires {} arguments but provided {}",
                self.name,
                self.arity,
                args.len()
            );
        }

        (self.body)(args)
    }
}

impl State {
    pub fn display(&self) {
        for (name, value) in self.variables() {
            println!("{} = {}", name, value);
        }
    }

    pub fn variables(&self) -> &HashMap<String, i32> {
        &self.variables
    }

    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn defaultenv() -> Self {
        let mut state = State::new();

        let random_int = Function::new(
            "RandomInt".to_string(),
            2,
            Box::new(|args| {
                let low = args[0];
                let high = args[1];
                thread_rng().gen_range(low..=high)
            }),
        );
        state.register_func(random_int);

        let read_int = Function::new(
            "ReadInt".to_string(),
            0,
            Box::new(|_| {
                let mut line = String::new();
                io::stdin().read_line(&mut line).unwrap();
                line.trim().parse::<i32>().expect("failed to parse stdin")
            }),
        );
        state.register_func(read_int);

        state
    }

    fn register_func(&mut self, func: Function) {
        if let Some(old) = self.functions.insert(func.name.clone(), func) {
            panic!("Function named {} is already defined", old.name);
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
        let name = stmt.var.ident().to_string();
        let value = self.eval_arith_expr(&stmt.expr);
        self.variables.insert(name, value);
    }

    fn run_dump_stmt(&mut self, stmt: &AstDumpStmt) {
        let name = stmt.var.ident();
        let value = *self
            .variables
            .get(name)
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
                .variables
                .get(var.ident())
                .unwrap_or_else(|| panic!("undeclared variable: {}", var.ident())),
            AstArithExpr::Const(value) => value.value(),
            AstArithExpr::FnCall(fncall) => self.eval_fncall(fncall),
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

    fn eval_fncall(&mut self, fncall: &AstFnCall) -> i32 {
        let ident = fncall.ident.ident();
        let mut args = vec![];
        let mut curr = &*fncall.args;
        while let AstArgumentList::Nonempty { expr, next } = curr {
            args.push(self.eval_arith_expr(&*expr));
            curr = next;
        }

        let fnptr = self
            .functions
            .get(ident)
            .unwrap_or_else(|| panic!("unknown function: {}", ident));
        fnptr.call(&args)
    }
}
