use crate::ast::*;
use rand::prelude::*;
use std::collections::HashMap;
use std::io;

pub type Result<T, E = InterpreterError> = std::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
pub enum InterpreterError {
    #[error("undeclared function: `{}`", name)]
    UndeclaredFunction { name: String },

    #[error(
        "arity mismatch: function `{}` requires {} but provided {}",
        name,
        required,
        provided
    )]
    ArityMismatch {
        name: String,
        required: usize,
        provided: usize,
    },

    #[error("undeclared variable: `{}`", name)]
    UndeclaredVariable { name: String },

    #[error("function already declared: `{}`", name)]
    AlreadyDeclaredFunction { name: String },
}

impl InterpreterError {
    pub fn summary(&self) -> String {
        match self {
            InterpreterError::UndeclaredFunction { .. } => "undeclared function".to_string(),
            InterpreterError::ArityMismatch { .. } => "arity mismatch".to_string(),
            InterpreterError::UndeclaredVariable { .. } => "undeclared variable".to_string(),
            InterpreterError::AlreadyDeclaredFunction { .. } => {
                "function already declared".to_string()
            }
        }
    }
}

pub fn run(stmt: &AstStmt) -> Result<State> {
    let mut state = State::defaultenv();
    state.run_stmt(stmt)?;
    Ok(state)
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

    pub fn call(&self, args: &[i32]) -> Result<i32> {
        if self.arity != args.len() {
            Err(InterpreterError::ArityMismatch {
                name: self.name.clone(),
                required: self.arity,
                provided: args.len(),
            })
        } else {
            Ok((self.body)(args))
        }
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
        state.register_func(random_int).expect("internal error");

        let read_int = Function::new(
            "ReadInt".to_string(),
            0,
            Box::new(|_| {
                let mut line = String::new();
                io::stdin().read_line(&mut line).unwrap();
                line.trim().parse::<i32>().expect("failed to parse stdin")
            }),
        );
        state.register_func(read_int).expect("internal error");

        state
    }

    fn register_func(&mut self, func: Function) -> Result<()> {
        if let Some(old) = self.functions.insert(func.name.clone(), func) {
            Err(InterpreterError::AlreadyDeclaredFunction { name: old.name })
        } else {
            Ok(())
        }
    }

    fn run_stmt(&mut self, stmt: &AstStmt) -> Result<()> {
        match stmt {
            AstStmt::IfStmt(stmt) => self.run_if_stmt(stmt),
            AstStmt::WhileStmt(stmt) => self.run_while_stmt(stmt),
            AstStmt::BeginStmt(stmt) => self.run_begin_stmt(stmt),
            AstStmt::AssgStmt(stmt) => self.run_assg_stmt(stmt),
            AstStmt::DumpStmt(stmt) => self.run_dump_stmt(stmt),
        }
    }

    fn run_if_stmt(&mut self, stmt: &AstIfStmt) -> Result<()> {
        if self.eval_bool_expr(&stmt.cond)? {
            self.run_stmt(&stmt.then)?;
        } else {
            self.run_stmt(&stmt.otherwise)?;
        }

        Ok(())
    }

    fn run_while_stmt(&mut self, stmt: &AstWhileStmt) -> Result<()> {
        while self.eval_bool_expr(&stmt.cond)? {
            self.run_stmt(&stmt.body)?;
        }

        Ok(())
    }

    fn run_begin_stmt(&mut self, stmt: &AstBeginStmt) -> Result<()> {
        self.run_stmt_list(&stmt.list)
    }

    fn run_stmt_list(&mut self, list: &AstStmtList) -> Result<()> {
        self.run_stmt(&list.stmt)?;
        if let Some(stmt) = &list.next {
            self.run_stmt_list(stmt)?;
        }

        Ok(())
    }

    fn run_assg_stmt(&mut self, stmt: &AstAssgStmt) -> Result<()> {
        let name = stmt.var.ident().to_string();
        let value = self.eval_arith_expr(&stmt.expr)?;
        self.variables.insert(name, value);

        Ok(())
    }

    fn run_dump_stmt(&mut self, stmt: &AstDumpStmt) -> Result<()> {
        let name = stmt.var.ident();
        let value =
            self.variables
                .get(name)
                .ok_or_else(|| InterpreterError::UndeclaredVariable {
                    name: name.to_string(),
                })?;
        println!("dump: {} = {}", name, value);

        Ok(())
    }

    fn eval_bool_expr(&mut self, expr: &AstBoolExpr) -> Result<bool> {
        let lhs = self.eval_arith_expr(&expr.lhs)?;
        let rhs = self.eval_arith_expr(&expr.rhs)?;
        match &*expr.op {
            AstCompareOp::Lt => Ok(lhs < rhs),
            AstCompareOp::Gt => Ok(lhs > rhs),
            AstCompareOp::Le => Ok(lhs <= rhs),
            AstCompareOp::Ge => Ok(lhs >= rhs),
            AstCompareOp::Eq => Ok(lhs == rhs),
            AstCompareOp::Ne => Ok(lhs != rhs),
        }
    }

    fn eval_arith_expr(&mut self, expr: &AstArithExpr) -> Result<i32> {
        match expr {
            AstArithExpr::Var(var) => self.variables.get(var.ident()).copied().ok_or_else(|| {
                InterpreterError::UndeclaredVariable {
                    name: var.ident().to_string(),
                }
            }),
            AstArithExpr::Const(value) => Ok(value.value()),
            AstArithExpr::FnCall(fncall) => self.eval_fncall(fncall),
            AstArithExpr::Op { lhs, op, rhs } => {
                let lhs = self.eval_arith_expr(lhs)?;
                let rhs = self.eval_arith_expr(rhs)?;
                match **op {
                    AstArithOp::Add => Ok(lhs + rhs),
                    AstArithOp::Sub => Ok(lhs - rhs),
                    AstArithOp::Mul => Ok(lhs * rhs),
                    AstArithOp::Div => Ok(lhs / rhs),
                }
            }
        }
    }

    fn eval_fncall(&mut self, fncall: &AstFnCall) -> Result<i32> {
        let ident = fncall.ident.ident();
        let mut args = vec![];
        let mut curr = &*fncall.args;
        while let AstArgumentList::Nonempty { expr, next } = curr {
            args.push(self.eval_arith_expr(&*expr)?);
            curr = next;
        }

        let fnptr =
            self.functions
                .get(ident)
                .ok_or_else(|| InterpreterError::UndeclaredFunction {
                    name: ident.to_string(),
                })?;

        fnptr.call(&args)
    }
}
