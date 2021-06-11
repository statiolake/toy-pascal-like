use crate::ast::*;
use crate::span::Span;
use dyn_clone::DynClone;
use itertools::izip;
use itertools::Itertools as _;
use rand::prelude::*;
use std::cmp::{PartialEq, PartialOrd};
use std::collections::{HashMap, HashSet};
use std::{fmt, io};

pub type Result<T, E = InterpError> = std::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
#[error("{span}: {kind}")]
pub struct InterpError {
    pub span: Span,
    pub kind: InterpErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum InterpErrorKind {
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

    #[error("type mismatch: expected `{expected}`, actual `{actual}`")]
    TyMismatch { expected: ValueTy, actual: ValueTy },

    #[error("binary operation `{op}` is not supported for `{ty}`")]
    UnsupportedBinaryOperation { op: String, ty: ValueTy },

    #[error("unknown type: `{}`", name)]
    UnknownTy { name: String },
}

impl InterpErrorKind {
    pub fn summary(&self) -> String {
        match self {
            InterpErrorKind::UndeclaredFunction { .. } => "undeclared function".to_string(),
            InterpErrorKind::ArityMismatch { .. } => "arity mismatch".to_string(),
            InterpErrorKind::UndeclaredVariable { .. } => "undeclared variable".to_string(),
            InterpErrorKind::AlreadyDeclaredFunction { .. } => {
                "function already declared".to_string()
            }
            InterpErrorKind::AlreadyUsedParameterName { .. } => "already used".to_string(),
            InterpErrorKind::NoReturnValue { .. } => "no return value".to_string(),
            InterpErrorKind::TyMismatch { .. } => "type mismatch".to_string(),
            InterpErrorKind::UnsupportedBinaryOperation { op, .. } => {
                format!("{} not supported for this type", op)
            }
            InterpErrorKind::UnknownTy { .. } => "unknown type".to_string(),
        }
    }

    pub fn hints(&self) -> Vec<String> {
        match self {
            InterpErrorKind::NoReturnValue { fnname } => vec![
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

pub fn run(stmt: &Ast<AstBeginStmt>) -> Result<State> {
    let mut state = State::defaultenv();
    state.run_begin_stmt(stmt)?;
    Ok(state)
}

#[derive(Clone)]
struct Function<'a> {
    def_span: Span,
    name: String,
    params: Vec<Param>,
    ret_ty: ValueTy,
    body: Box<dyn FunctionBody<'a>>,
}

#[derive(Debug, Clone)]
struct Param {
    name: String,
    ty: ValueTy,
}

impl Param {
    fn new(name: String, ty: ValueTy) -> Self {
        Self { name, ty }
    }
}

#[derive(Debug, Clone)]
struct Arg {
    span: Span,
    value: Value,
}

impl Arg {
    pub fn new(span: Span, value: Value) -> Self {
        Self { span, value }
    }
}

impl<'a> Function<'a> {
    pub fn new(
        def_span: Span,
        name: String,
        params: Vec<Param>,
        ret_ty: ValueTy,
        body: Box<dyn FunctionBody<'a>>,
    ) -> Self {
        Self {
            def_span,
            name,
            params,
            ret_ty,
            body,
        }
    }

    pub fn call(&self, state: &State<'a>, span: Span, args: &[Arg]) -> Result<Value> {
        // arity check
        if self.params.len() != args.len() {
            return Err(InterpError {
                span,
                kind: InterpErrorKind::ArityMismatch {
                    name: self.name.clone(),
                    required: self.params.len(),
                    provided: args.len(),
                },
            });
        }

        // type check
        for (param, arg) in izip!(&self.params, args) {
            let arg_ty = arg.value.ty();
            if param.ty != arg_ty {
                return Err(InterpError {
                    span: arg.span,
                    kind: InterpErrorKind::TyMismatch {
                        expected: param.ty,
                        actual: arg_ty,
                    },
                });
            }
        }

        let args = args.iter().map(|arg| arg.value.clone()).collect_vec();
        (self.body)(state, span, &args)
    }
}

trait FunctionBody<'a>: Fn(&State<'a>, Span, &[Value]) -> Result<Value> + DynClone + 'a {}
impl Clone for Box<dyn FunctionBody<'_>> {
    fn clone(&self) -> Self {
        dyn_clone::clone_box(&**self)
    }
}

impl<'a, F> FunctionBody<'a> for F
where
    F: Fn(&State<'a>, Span, &[Value]) -> Result<Value>,
    F: DynClone,
    F: 'a,
{
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueTy {
    Int,
    Float,
}

impl Value {
    pub fn ty(&self) -> ValueTy {
        match self {
            Value::Int(_) => ValueTy::Int,
            Value::Float(_) => ValueTy::Float,
        }
    }

    pub fn int(&self, span: Span) -> Result<i64> {
        match self {
            Value::Int(int) => Ok(*int),
            _ => Err(InterpError {
                span,
                kind: InterpErrorKind::TyMismatch {
                    expected: ValueTy::Int,
                    actual: self.ty(),
                },
            }),
        }
    }

    pub fn float(&self, span: Span) -> Result<f64> {
        match self {
            Value::Float(float) => Ok(*float),
            _ => Err(InterpError {
                span,
                kind: InterpErrorKind::TyMismatch {
                    expected: ValueTy::Float,
                    actual: self.ty(),
                },
            }),
        }
    }

    pub fn unwrap_int(&self) -> i64 {
        match self {
            Value::Int(int) => *int,
            _ => panic!("value is actually not an int"),
        }
    }

    pub fn unwrap_float(&self) -> f64 {
        match self {
            Value::Float(float) => *float,
            _ => panic!("value is actually not an float"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(int) => write!(b, "{}", int),
            Value::Float(float) => write!(b, "{}", float),
        }
    }
}

impl fmt::Display for ValueTy {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueTy::Int => write!(b, "int"),
            ValueTy::Float => write!(b, "float"),
        }
    }
}

#[derive(Clone)]
pub struct State<'a> {
    vars: HashMap<String, Value>,
    funcs: HashMap<String, Function<'a>>,
}

impl<'a> State<'a> {
    pub fn display(&self) {
        for (name, value) in self.variables() {
            println!("{} = {}", name, value);
        }
    }

    pub fn variables(&self) -> &HashMap<String, Value> {
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
            Span::new_zero(),
            "RandomInt".to_string(),
            vec![
                Param::new("low".to_string(), ValueTy::Int),
                Param::new("high".to_string(), ValueTy::Int),
            ],
            ValueTy::Int,
            Box::new(|_, _, args| {
                let low = args[0].unwrap_int();
                let high = args[1].unwrap_int();
                Ok(Value::Int(thread_rng().gen_range(low..=high)))
            }),
        );
        state
            .register_func(Span::new_zero(), random_int)
            .expect("internal error");

        let random_float = Function::new(
            Span::new_zero(),
            "RandomFloat".to_string(),
            vec![],
            ValueTy::Float,
            Box::new(|_, _, _| Ok(Value::Float(thread_rng().gen()))),
        );
        state
            .register_func(Span::new_zero(), random_float)
            .expect("internal error");

        // TODO: more generic way
        let cast_int = Function::new(
            Span::new_zero(),
            "int".to_string(),
            vec![Param::new("value".to_string(), ValueTy::Float)],
            ValueTy::Int,
            Box::new(|_, _, args| Ok(Value::Int(args[0].unwrap_float() as _))),
        );
        state
            .register_func(Span::new_zero(), cast_int)
            .expect("internal error");

        // TODO: more generic way
        let cast_float = Function::new(
            Span::new_zero(),
            "float".to_string(),
            vec![Param::new("value".to_string(), ValueTy::Int)],
            ValueTy::Float,
            Box::new(|_, _, args| Ok(Value::Float(args[0].unwrap_int() as _))),
        );
        state
            .register_func(Span::new_zero(), cast_float)
            .expect("internal error");

        let read_int = Function::new(
            Span::new_zero(),
            "ReadInt".to_string(),
            vec![],
            ValueTy::Int,
            Box::new(|_, _, _| {
                let mut line = String::new();
                io::stdin().read_line(&mut line).unwrap();
                Ok(Value::Int(
                    line.trim().parse::<i64>().expect("failed to parse stdin"),
                ))
            }),
        );
        state
            .register_func(Span::new_zero(), read_int)
            .expect("internal error");

        state
    }

    fn register_func(&mut self, span: Span, func: Function<'a>) -> Result<()> {
        if let Some(old) = self.funcs.insert(func.name.clone(), func) {
            Err(InterpError {
                span,
                kind: InterpErrorKind::AlreadyDeclaredFunction { name: old.name },
            })
        } else {
            Ok(())
        }
    }

    fn assign_to_var(&mut self, _span: Span, var: &str, value: Value) -> Result<()> {
        self.vars.insert(var.to_string(), value);
        Ok(())
    }

    fn get_var(&self, span: Span, name: &str) -> Result<Value> {
        self.vars
            .get(name)
            .map(Clone::clone)
            .ok_or_else(|| InterpError {
                span,
                kind: InterpErrorKind::UndeclaredVariable {
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
        while let AstParamList::Nonempty { ident, ty, next } = curr {
            params.push((ident, ty));
            curr = &next.ast;
        }

        // ensure that there is no same name params
        let mut used = HashSet::new();
        for (param, _) in &params {
            if !used.insert(param.ast.ident()) {
                return Err(InterpError {
                    span: param.span,
                    kind: InterpErrorKind::AlreadyUsedParameterName {
                        name: param.ast.ident().to_string(),
                    },
                });
            }
        }

        let params = params
            .into_iter()
            .map(|(p, ty)| {
                let ty = self.resolve_ty(ty)?;
                Ok(Param::new(p.ast.ident().to_string(), ty))
            })
            .collect::<Result<Vec<_>>>()?;

        let ret_ty = self.resolve_ty(&stmt.ast.ret_ty)?;

        let func = Function::new(
            stmt.span,
            name.to_string(),
            params.clone(),
            ret_ty,
            Box::new(move |state, span, args| {
                let mut inner_state = state.clone();
                for (idx, param) in params.iter().enumerate() {
                    inner_state.assign_to_var(span, &param.name, args[idx].clone())?;
                }
                inner_state.run_begin_stmt(&stmt.ast.body)?;
                inner_state
                    .get_var(stmt.span, name)
                    .map_err(|_| InterpError {
                        span: stmt.span,
                        kind: InterpErrorKind::NoReturnValue {
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
        if let AstStmtList::Nonempty { stmt, next } = &list.ast {
            self.run_stmt(stmt)?;
            self.run_stmt_list(next)?;
        }

        Ok(())
    }

    fn run_assg_stmt(&mut self, stmt: &'a Ast<AstAssgStmt>) -> Result<()> {
        let name = stmt.ast.var.ast.ident();
        let value = self.eval_arith_expr(&stmt.ast.expr)?;
        self.assign_to_var(stmt.span, name.ast.ident(), value)?;

        Ok(())
    }

    fn run_dump_stmt(&mut self, stmt: &'a Ast<AstDumpStmt>) -> Result<()> {
        let name = stmt.ast.var.ast.ident();
        let value = self.get_var(stmt.span, name.ast.ident())?;
        println!("dump: {} = {}", name, value);

        Ok(())
    }

    fn eval_bool_expr(&mut self, expr: &'a Ast<AstBoolExpr>) -> Result<bool> {
        let lhs = self.eval_arith_expr(&expr.ast.lhs)?;
        let rhs = self.eval_arith_expr(&expr.ast.rhs)?;

        fn compare<T: PartialEq + PartialOrd>(op: &AstCompareOp, lhs: T, rhs: T) -> bool {
            match &op {
                AstCompareOp::Lt => lhs < rhs,
                AstCompareOp::Gt => lhs > rhs,
                AstCompareOp::Le => lhs <= rhs,
                AstCompareOp::Ge => lhs >= rhs,
                AstCompareOp::Eq => lhs == rhs,
                AstCompareOp::Ne => lhs != rhs,
            }
        }

        if let Ok(lhs) = lhs.int(expr.ast.lhs.span) {
            let rhs = rhs.int(expr.ast.rhs.span)?;
            Ok(compare(&expr.ast.op.ast, lhs, rhs))
        } else if let Ok(lhs) = lhs.float(expr.ast.lhs.span) {
            let rhs = rhs.float(expr.ast.rhs.span)?;
            Ok(compare(&expr.ast.op.ast, lhs, rhs))
        } else {
            Err(InterpError {
                span: expr.ast.lhs.span,
                kind: InterpErrorKind::UnsupportedBinaryOperation {
                    op: expr.ast.op.ast.to_string(),
                    ty: lhs.ty(),
                },
            })
        }
    }

    fn eval_arith_expr(&mut self, expr: &'a Ast<AstArithExpr>) -> Result<Value> {
        macro_rules! binop {
            ($lhs:ident $op:tt $rhs:ident) => {{
                let lhs = self.eval_arith_expr(&$lhs)?;
                let rhs = self.eval_mul_expr(&$rhs)?;
                if let Ok(lhs) = lhs.int($lhs.span) {
                    let rhs = rhs.int($rhs.span)?;
                    Ok(Value::Int(lhs $op rhs))
                } else if let Ok(lhs) = lhs.float($lhs.span) {
                    let rhs = rhs.float($rhs.span)?;
                    Ok(Value::Float(lhs $op rhs))
                } else {
                    Err(InterpError {
                        span: $lhs.span,
                        kind: InterpErrorKind::UnsupportedBinaryOperation {
                            op: stringify!($op).to_string(),
                            ty: lhs.ty(),
                        },
                    })
                }
            }};
        }

        match &expr.ast {
            AstArithExpr::MulExpr(expr) => self.eval_mul_expr(&expr),
            AstArithExpr::Add(lhs, rhs) => binop!(lhs + rhs),
            AstArithExpr::Sub(lhs, rhs) => binop!(lhs - rhs),
        }
    }

    fn eval_mul_expr(&mut self, expr: &'a Ast<AstMulExpr>) -> Result<Value> {
        macro_rules! binop {
            ($lhs:ident $op:tt $rhs:ident) => {{
                let lhs = self.eval_mul_expr(&$lhs)?;
                let rhs = self.eval_unary_expr(&$rhs)?;
                if let Ok(lhs) = lhs.int($lhs.span) {
                    let rhs = rhs.int($rhs.span)?;
                    Ok(Value::Int(lhs $op rhs))
                } else if let Ok(lhs) = lhs.float($lhs.span) {
                    let rhs = rhs.float($rhs.span)?;
                    Ok(Value::Float(lhs $op rhs))
                } else {
                    Err(InterpError {
                        span: $lhs.span,
                        kind: InterpErrorKind::UnsupportedBinaryOperation {
                            op: stringify!($op).to_string(),
                            ty: lhs.ty(),
                        },
                    })
                }
            }};
        }

        match &expr.ast {
            AstMulExpr::UnaryExpr(expr) => self.eval_unary_expr(expr),
            AstMulExpr::Mul(lhs, rhs) => binop!(lhs * rhs),
            AstMulExpr::Div(lhs, rhs) => binop!(lhs / rhs),
        }
    }

    fn eval_unary_expr(&mut self, expr: &'a Ast<AstUnaryExpr>) -> Result<Value> {
        match &expr.ast {
            AstUnaryExpr::PrimaryExpr(expr) => self.eval_primary_expr(expr),
            AstUnaryExpr::Neg(expr) => self.eval_unary_expr(expr).map(|value| match value {
                Value::Int(int) => Value::Int(-int),
                Value::Float(float) => Value::Float(-float),
            }),
        }
    }

    fn eval_primary_expr(&mut self, expr: &'a Ast<AstPrimaryExpr>) -> Result<Value> {
        match &expr.ast {
            AstPrimaryExpr::Var(var) => self.get_var(expr.span, var.ast.ident().ast.ident()),
            AstPrimaryExpr::Const(value) => self.eval_const(value),
            AstPrimaryExpr::FnCall(fncall) => self.eval_fncall(fncall),
            AstPrimaryExpr::Paren(expr) => self.eval_arith_expr(expr),
        }
    }

    fn eval_const(&mut self, value: &'a Ast<AstConst>) -> Result<Value> {
        match value.ast {
            AstConst::Int(int) => Ok(Value::Int(int)),
            AstConst::Float(float) => Ok(Value::Float(float)),
        }
    }

    fn eval_fncall(&mut self, fncall: &'a Ast<AstFnCall>) -> Result<Value> {
        let ident = fncall.ast.ident.ast.ident();
        let mut args = vec![];
        let mut curr = &fncall.ast.args.ast;
        while let AstArgumentList::Nonempty { expr, next } = curr {
            args.push(Arg::new(expr.span, self.eval_arith_expr(&*expr)?));
            curr = &next.ast;
        }

        let fnptr = self.funcs.get(ident).ok_or_else(|| InterpError {
            span: fncall.span,
            kind: InterpErrorKind::UndeclaredFunction {
                name: ident.to_string(),
            },
        })?;

        let value = fnptr.call(self, fncall.span, &args)?;
        match fnptr.ret_ty {
            ValueTy::Int => {
                value.int(fnptr.def_span)?;
            }
            ValueTy::Float => {
                value.float(fnptr.def_span)?;
            }
        }

        Ok(value)
    }

    fn resolve_ty(&mut self, ty: &'a Ast<AstTy>) -> Result<ValueTy> {
        match ty.ast.ident().ast.ident() {
            "int" => Ok(ValueTy::Int),
            "float" => Ok(ValueTy::Float),
            other => Err(InterpError {
                span: ty.span,
                kind: InterpErrorKind::UnknownTy {
                    name: other.to_string(),
                },
            }),
        }
    }
}
