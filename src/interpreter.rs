use crate::ast::*;
use crate::lexer::Span;
use dyn_clone::DynClone;
use rand::prelude::*;
use std::collections::{HashMap, HashSet};
use std::io;

pub type Result<T, E = InterpreterError> = std::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
#[error("{span}: {kind}")]
pub struct InterpreterError {
    pub span: Span,
    pub kind: InterpreterErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum InterpreterErrorKind {
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

    #[error("parameter name `{}` is already used", name)]
    AlreadyUsedParameterName { name: String },

    #[error("return value is not specified")]
    NoReturnValue { fnname: String },
}

impl InterpreterErrorKind {
    pub fn summary(&self) -> String {
        match self {
            InterpreterErrorKind::UndeclaredFunction { .. } => "undeclared function".to_string(),
            InterpreterErrorKind::ArityMismatch { .. } => "arity mismatch".to_string(),
            InterpreterErrorKind::UndeclaredVariable { .. } => "undeclared variable".to_string(),
            InterpreterErrorKind::AlreadyDeclaredFunction { .. } => {
                "function already declared".to_string()
            }
            InterpreterErrorKind::AlreadyUsedParameterName { .. } => "already used".to_string(),
            InterpreterErrorKind::NoReturnValue { .. } => "no return value".to_string(),
        }
    }

    pub fn hints(&self) -> Vec<String> {
        match self {
            InterpreterErrorKind::NoReturnValue { fnname } => vec![
                format!(
                    concat!(
                        "you can specify the return value by assigning it",
                        " to the variable named `{}` inside the function"
                    ),
                    fnname
                ),
                format!("for example: place `{} := 42` at the end", fnname),
            ],
            _ => vec![],
        }
    }
}

pub fn run(stmt: &Ast<AstStmt>) -> Result<State> {
    let mut state = State::defaultenv();
    state.run_stmt(stmt)?;
    Ok(state)
}

trait FunctionBody<'a>: Fn(&State<'a>, Span, &[i32]) -> Result<i32> + DynClone + 'a {}
impl Clone for Box<dyn FunctionBody<'_>> {
    fn clone(&self) -> Self {
        dyn_clone::clone_box(&**self)
    }
}

impl<'a, F> FunctionBody<'a> for F
where
    F: Fn(&State<'a>, Span, &[i32]) -> Result<i32>,
    F: DynClone,
    F: 'a,
{
}

#[derive(Clone)]
struct Function<'a> {
    name: String,
    arity: usize,
    body: Box<dyn FunctionBody<'a>>,
}

impl<'a> Function<'a> {
    pub fn new(name: String, arity: usize, body: Box<dyn FunctionBody<'a>>) -> Self {
        Self { name, arity, body }
    }

    pub fn call(&self, state: &State<'a>, span: Span, args: &[i32]) -> Result<i32> {
        if self.arity != args.len() {
            Err(InterpreterError {
                span,
                kind: InterpreterErrorKind::ArityMismatch {
                    name: self.name.clone(),
                    required: self.arity,
                    provided: args.len(),
                },
            })
        } else {
            (self.body)(state, span, args)
        }
    }
}

#[derive(Clone)]
pub struct State<'a> {
    vars: HashMap<String, i32>,
    funcs: HashMap<String, Function<'a>>,
}

impl<'a> State<'a> {
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
            funcs: HashMap::new(),
        }
    }

    fn defaultenv() -> Self {
        let mut state = State::new();

        let random_int = Function::new(
            "RandomInt".to_string(),
            2,
            Box::new(|_, _, args| {
                let low = args[0];
                let high = args[1];
                Ok(thread_rng().gen_range(low..=high))
            }),
        );
        state
            .register_func(Span::new_zero(), random_int)
            .expect("internal error");

        let read_int = Function::new(
            "ReadInt".to_string(),
            0,
            Box::new(|_, _, _| {
                let mut line = String::new();
                io::stdin().read_line(&mut line).unwrap();
                Ok(line.trim().parse::<i32>().expect("failed to parse stdin"))
            }),
        );
        state
            .register_func(Span::new_zero(), read_int)
            .expect("internal error");

        state
    }

    fn register_func(&mut self, span: Span, func: Function<'a>) -> Result<()> {
        if let Some(old) = self.funcs.insert(func.name.clone(), func) {
            Err(InterpreterError {
                span,
                kind: InterpreterErrorKind::AlreadyDeclaredFunction { name: old.name },
            })
        } else {
            Ok(())
        }
    }

    fn assign_to_var(&mut self, _span: Span, var: &str, value: i32) -> Result<()> {
        self.vars.insert(var.to_string(), value);
        Ok(())
    }

    fn get_var(&self, span: Span, name: &str) -> Result<i32> {
        self.vars
            .get(name)
            .copied()
            .ok_or_else(|| InterpreterError {
                span,
                kind: InterpreterErrorKind::UndeclaredVariable {
                    name: name.to_string(),
                },
            })
    }
}

impl<'a> State<'a> {
    fn run_stmt(&mut self, stmt: &'a Ast<AstStmt>) -> Result<()> {
        match &stmt.ast {
            AstStmt::FuncdefStmt(stmt) => self.run_funcdef_stmt(&*stmt),
            AstStmt::IfStmt(stmt) => self.run_if_stmt(&*stmt),
            AstStmt::WhileStmt(stmt) => self.run_while_stmt(&*stmt),
            AstStmt::BeginStmt(stmt) => self.run_begin_stmt(&*stmt),
            AstStmt::AssgStmt(stmt) => self.run_assg_stmt(&*stmt),
            AstStmt::DumpStmt(stmt) => self.run_dump_stmt(&*stmt),
        }
    }

    fn run_funcdef_stmt(&mut self, stmt: &'a Ast<AstFuncdefStmt>) -> Result<()> {
        let name = stmt.ast.name.ast.ident();
        let mut params = vec![];
        let mut curr = &stmt.ast.params.ast;
        while let AstParamList::Nonempty { ident, next } = curr {
            params.push(ident);
            curr = &next.ast;
        }

        // ensure that there is no same name params
        let mut used = HashSet::new();
        for param in &params {
            if !used.insert(param.ast.ident()) {
                return Err(InterpreterError {
                    span: param.span,
                    kind: InterpreterErrorKind::AlreadyUsedParameterName {
                        name: param.ast.ident().to_string(),
                    },
                });
            }
        }

        let func = Function::new(
            name.to_string(),
            params.len(),
            Box::new(move |state, span, args| {
                let mut inner_state = state.clone();
                for (idx, param) in params.iter().enumerate() {
                    inner_state.assign_to_var(span, param.ast.ident(), args[idx])?;
                }
                inner_state.run_begin_stmt(&stmt.ast.body)?;
                inner_state
                    .get_var(stmt.span, name)
                    .map_err(|_| InterpreterError {
                        span: stmt.span,
                        kind: InterpreterErrorKind::NoReturnValue {
                            fnname: name.to_string(),
                        },
                    })
            }),
        );

        self.register_func(stmt.span, func)?;

        Ok(())
    }

    fn run_if_stmt(&mut self, stmt: &'a Ast<AstIfStmt>) -> Result<()> {
        if self.eval_bool_expr(&stmt.ast.cond)? {
            self.run_stmt(&stmt.ast.then)?;
        } else {
            self.run_stmt(&stmt.ast.otherwise)?;
        }

        Ok(())
    }

    fn run_while_stmt(&mut self, stmt: &'a Ast<AstWhileStmt>) -> Result<()> {
        while self.eval_bool_expr(&stmt.ast.cond)? {
            self.run_stmt(&stmt.ast.body)?;
        }

        Ok(())
    }

    fn run_begin_stmt(&mut self, stmt: &'a Ast<AstBeginStmt>) -> Result<()> {
        self.run_stmt_list(&stmt.ast.list)
    }

    fn run_stmt_list(&mut self, list: &'a Ast<AstStmtList>) -> Result<()> {
        self.run_stmt(&list.ast.stmt)?;
        if let Some(stmt) = &list.ast.next {
            self.run_stmt_list(stmt)?;
        }

        Ok(())
    }

    fn run_assg_stmt(&mut self, stmt: &'a Ast<AstAssgStmt>) -> Result<()> {
        let name = stmt.ast.var.ast.ident();
        let value = self.eval_arith_expr(&stmt.ast.expr)?;
        self.assign_to_var(stmt.span, name, value)?;

        Ok(())
    }

    fn run_dump_stmt(&mut self, stmt: &'a Ast<AstDumpStmt>) -> Result<()> {
        let name = stmt.ast.var.ast.ident();
        let value = self.get_var(stmt.span, name)?;
        println!("dump: {} = {}", name, value);

        Ok(())
    }

    fn eval_bool_expr(&mut self, expr: &'a Ast<AstBoolExpr>) -> Result<bool> {
        let lhs = self.eval_arith_expr(&expr.ast.lhs)?;
        let rhs = self.eval_arith_expr(&expr.ast.rhs)?;
        match &expr.ast.op.ast {
            AstCompareOp::Lt => Ok(lhs < rhs),
            AstCompareOp::Gt => Ok(lhs > rhs),
            AstCompareOp::Le => Ok(lhs <= rhs),
            AstCompareOp::Ge => Ok(lhs >= rhs),
            AstCompareOp::Eq => Ok(lhs == rhs),
            AstCompareOp::Ne => Ok(lhs != rhs),
        }
    }

    fn eval_arith_expr(&mut self, expr: &'a Ast<AstArithExpr>) -> Result<i32> {
        match &expr.ast {
            AstArithExpr::Var(var) => self.get_var(expr.span, var.ast.ident()),
            AstArithExpr::Const(value) => Ok(value.ast.value()),
            AstArithExpr::FnCall(fncall) => self.eval_fncall(fncall),
            AstArithExpr::Op { lhs, op, rhs } => {
                let lhs = self.eval_arith_expr(lhs)?;
                let rhs = self.eval_arith_expr(rhs)?;
                match &op.ast {
                    AstArithOp::Add => Ok(lhs + rhs),
                    AstArithOp::Sub => Ok(lhs - rhs),
                    AstArithOp::Mul => Ok(lhs * rhs),
                    AstArithOp::Div => Ok(lhs / rhs),
                }
            }
        }
    }

    fn eval_fncall(&mut self, fncall: &'a Ast<AstFnCall>) -> Result<i32> {
        let ident = fncall.ast.ident.ast.ident();
        let mut args = vec![];
        let mut curr = &fncall.ast.args.ast;
        while let AstArgumentList::Nonempty { expr, next } = curr {
            args.push(self.eval_arith_expr(&*expr)?);
            curr = &next.ast;
        }

        let fnptr = self.funcs.get(ident).ok_or_else(|| InterpreterError {
            span: fncall.span,
            kind: InterpreterErrorKind::UndeclaredFunction {
                name: ident.to_string(),
            },
        })?;

        fnptr.call(self, fncall.span, &args)
    }
}
