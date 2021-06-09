use crate::lexer::Span;
use itertools::Itertools as _;
use std::fmt;

#[derive(Debug)]
pub struct Ast<A> {
    pub span: Span,
    pub ast: A,
}

macro_rules! derive_from_for_tuple_like {
    ($derived:ident => $super:ident::$variant:ident, $from_fn:ident) => {
        impl $super {
            pub fn $from_fn(ast: Ast<$derived>) -> Ast<$super> {
                Ast {
                    span: ast.span,
                    ast: $super::$variant(Box::new(ast)),
                }
            }
        }
    };
}

#[derive(Debug)]
pub enum AstStmt {
    IfStmt(Box<Ast<AstIfStmt>>),
    WhileStmt(Box<Ast<AstWhileStmt>>),
    BeginStmt(Box<Ast<AstBeginStmt>>),
    AssgStmt(Box<Ast<AstAssgStmt>>),
    DumpStmt(Box<Ast<AstDumpStmt>>),
}

derive_from_for_tuple_like!(AstIfStmt => AstStmt::IfStmt, from_if_stmt);
derive_from_for_tuple_like!(AstWhileStmt => AstStmt::WhileStmt, from_while_stmt);
derive_from_for_tuple_like!(AstBeginStmt => AstStmt::BeginStmt, from_begin_stmt);
derive_from_for_tuple_like!(AstAssgStmt => AstStmt::AssgStmt, from_assg_stmt);
derive_from_for_tuple_like!(AstDumpStmt => AstStmt::DumpStmt, from_dump_stmt);

#[derive(Debug)]
pub struct AstIfStmt {
    pub cond: Box<Ast<AstBoolExpr>>,
    pub then: Box<Ast<AstStmt>>,
    pub otherwise: Box<Ast<AstStmt>>,
}

impl AstIfStmt {
    pub fn from_elements(
        span: Span,
        cond: Ast<AstBoolExpr>,
        then: Ast<AstStmt>,
        otherwise: Ast<AstStmt>,
    ) -> Ast<AstIfStmt> {
        let ast = AstIfStmt {
            cond: Box::new(cond),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub struct AstWhileStmt {
    pub cond: Box<Ast<AstBoolExpr>>,
    pub body: Box<Ast<AstStmt>>,
}

impl AstWhileStmt {
    pub fn from_elements(
        span: Span,
        cond: Ast<AstBoolExpr>,
        body: Ast<AstStmt>,
    ) -> Ast<AstWhileStmt> {
        let ast = AstWhileStmt {
            cond: Box::new(cond),
            body: Box::new(body),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub struct AstBeginStmt {
    pub list: Box<Ast<AstStmtList>>,
}

impl AstBeginStmt {
    pub fn from_list(list: Ast<AstStmtList>) -> Ast<AstBeginStmt> {
        let span = list.span;
        let ast = AstBeginStmt {
            list: Box::new(list),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub struct AstStmtList {
    pub stmt: Box<Ast<AstStmt>>,
    pub next: Option<Box<Ast<AstStmtList>>>,
}

impl AstStmtList {
    pub fn from_elements(
        span: Span,
        stmt: Ast<AstStmt>,
        next: Option<Ast<AstStmtList>>,
    ) -> Ast<AstStmtList> {
        let ast = AstStmtList {
            stmt: Box::new(stmt),
            next: next.map(Box::new),
        };

        Ast { span, ast }
    }
}

#[derive(Debug)]
pub struct AstAssgStmt {
    pub var: Box<Ast<AstVar>>,
    pub expr: Box<Ast<AstArithExpr>>,
}

impl AstAssgStmt {
    pub fn from_elements(
        span: Span,
        var: Ast<AstVar>,
        expr: Ast<AstArithExpr>,
    ) -> Ast<AstAssgStmt> {
        let ast = AstAssgStmt {
            var: Box::new(var),
            expr: Box::new(expr),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub struct AstDumpStmt {
    pub var: Box<Ast<AstVar>>,
}

impl AstDumpStmt {
    pub fn from_var(var: Ast<AstVar>) -> Ast<AstDumpStmt> {
        Ast {
            span: var.span,
            ast: AstDumpStmt { var: Box::new(var) },
        }
    }
}

#[derive(Debug)]
pub struct AstBoolExpr {
    pub lhs: Box<Ast<AstArithExpr>>,
    pub op: Box<Ast<AstCompareOp>>,
    pub rhs: Box<Ast<AstArithExpr>>,
}

impl AstBoolExpr {
    pub fn from_elements(
        span: Span,
        lhs: Ast<AstArithExpr>,
        op: Ast<AstCompareOp>,
        rhs: Ast<AstArithExpr>,
    ) -> Ast<AstBoolExpr> {
        let ast = AstBoolExpr {
            lhs: Box::new(lhs),
            op: Box::new(op),
            rhs: Box::new(rhs),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub enum AstCompareOp {
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}

#[derive(Debug)]
pub enum AstArithExpr {
    Var(Box<Ast<AstVar>>),
    Const(Box<Ast<AstConst>>),
    FnCall(Box<Ast<AstFnCall>>),
    Op {
        lhs: Box<Ast<AstArithExpr>>,
        op: Box<Ast<AstArithOp>>,
        rhs: Box<Ast<AstArithExpr>>,
    },
}

derive_from_for_tuple_like!(AstVar => AstArithExpr::Var, from_var);
derive_from_for_tuple_like!(AstConst => AstArithExpr::Const, from_const);
derive_from_for_tuple_like!(AstFnCall => AstArithExpr::FnCall, from_fncall);

impl AstArithExpr {
    pub fn from_op_elements(
        span: Span,
        lhs: Ast<AstArithExpr>,
        op: Ast<AstArithOp>,
        rhs: Ast<AstArithExpr>,
    ) -> Ast<AstArithExpr> {
        let ast = AstArithExpr::Op {
            lhs: Box::new(lhs),
            op: Box::new(op),
            rhs: Box::new(rhs),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub struct AstFnCall {
    pub ident: Box<Ast<AstIdent>>,
    pub args: Box<Ast<AstArgumentList>>,
}

impl AstFnCall {
    pub fn from_elements(
        span: Span,
        ident: Ast<AstIdent>,
        args: Ast<AstArgumentList>,
    ) -> Ast<AstFnCall> {
        let ast = AstFnCall {
            ident: Box::new(ident),
            args: Box::new(args),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub enum AstArgumentList {
    Empty,
    Nonempty {
        expr: Box<Ast<AstArithExpr>>,
        next: Box<Ast<AstArgumentList>>,
    },
}

impl AstArgumentList {
    pub fn empty(span: Span) -> Ast<AstArgumentList> {
        Ast {
            ast: AstArgumentList::Empty,
            span,
        }
    }

    pub fn from_elements(
        span: Span,
        expr: Ast<AstArithExpr>,
        next: Ast<AstArgumentList>,
    ) -> Ast<AstArgumentList> {
        let ast = AstArgumentList::Nonempty {
            expr: Box::new(expr),
            next: Box::new(next),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub enum AstArithOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct AstConst(pub i32);

impl AstConst {
    pub fn value(&self) -> i32 {
        let AstConst(value) = *self;
        value
    }
}

#[derive(Debug)]
pub struct AstVar(pub AstIdent);

impl AstVar {
    pub fn from_ident(ident: Ast<AstIdent>) -> Ast<AstVar> {
        Ast {
            ast: AstVar(ident.ast),
            span: ident.span,
        }
    }

    pub fn ident(&self) -> &str {
        self.0.ident()
    }
}

#[derive(Debug)]
pub struct AstIdent(pub String);

impl AstIdent {
    pub fn ident(&self) -> &str {
        &self.0
    }
}

fn indent<T: fmt::Display>(s: &T, indent: usize) -> String {
    let indent = " ".repeat(indent);
    s.to_string()
        .trim()
        .lines()
        .map(|l| format!("{}{}", indent, l))
        .join("\n")
}

impl<A: fmt::Display> fmt::Display for Ast<A> {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "{} @ {}", self.span, self.ast)
    }
}

impl fmt::Display for AstStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        let child = match self {
            AstStmt::IfStmt(stmt) => stmt.to_string(),
            AstStmt::WhileStmt(stmt) => stmt.to_string(),
            AstStmt::BeginStmt(stmt) => stmt.to_string(),
            AstStmt::AssgStmt(stmt) => stmt.to_string(),
            AstStmt::DumpStmt(stmt) => stmt.to_string(),
        };

        writeln!(b, "Stmt")?;
        writeln!(b, "{}", indent(&child, 4))?;
        Ok(())
    }
}

impl fmt::Display for AstIfStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "IfStmt")?;
        writeln!(b, "  cond:")?;
        writeln!(b, "{}", indent(&self.cond, 4))?;
        writeln!(b, "  then:")?;
        writeln!(b, "{}", indent(&self.then, 4))?;
        writeln!(b, "  else:")?;
        writeln!(b, "{}", indent(&self.otherwise, 4))?;

        Ok(())
    }
}

impl fmt::Display for AstWhileStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "While")?;
        writeln!(b, "  cond:")?;
        writeln!(b, "{}", indent(&self.cond, 4))?;
        writeln!(b, "  body:")?;
        writeln!(b, "{}", indent(&self.body, 4))?;

        Ok(())
    }
}

impl fmt::Display for AstBeginStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "Begin")?;
        writeln!(b, "{}", indent(&self.list, 4))?;

        Ok(())
    }
}

impl fmt::Display for AstStmtList {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "{}", self.stmt.to_string().trim())?;
        if let Some(next) = &self.next {
            writeln!(b, "{}", next)?;
        }

        Ok(())
    }
}

impl fmt::Display for AstAssgStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "Assg")?;
        writeln!(b, "  var: {}", self.var)?;
        writeln!(b, "  expr:")?;
        writeln!(b, "{}", indent(&self.expr, 4))?;

        Ok(())
    }
}

impl fmt::Display for AstDumpStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "Dump")?;
        writeln!(b, "  {}", self.var)?;

        Ok(())
    }
}

impl fmt::Display for AstBoolExpr {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "BoolExpr")?;
        writeln!(b, "  lhs:")?;
        writeln!(b, "{}", indent(&self.lhs, 4))?;
        writeln!(b, "  op: {}", self.op)?;
        writeln!(b, "  rhs:")?;
        writeln!(b, "{}", indent(&self.rhs, 4))?;

        Ok(())
    }
}

impl fmt::Display for AstCompareOp {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            AstCompareOp::Lt => "<",
            AstCompareOp::Gt => ">",
            AstCompareOp::Le => "<=",
            AstCompareOp::Ge => ">=",
            AstCompareOp::Eq => "==",
            AstCompareOp::Ne => "!=",
        };
        write!(b, "CompareOp({})", op)
    }
}

impl fmt::Display for AstArithExpr {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "ArithExpr")?;
        match self {
            AstArithExpr::Var(var) => writeln!(b, "  {}", var)?,
            AstArithExpr::Const(value) => writeln!(b, "  {}", value)?,
            AstArithExpr::FnCall(fncall) => writeln!(b, "{}", indent(&fncall, 4))?,
            AstArithExpr::Op { lhs, op, rhs } => {
                writeln!(b, "  lhs:")?;
                writeln!(b, "{}", indent(&lhs, 4))?;
                writeln!(b, "  op: {}", op)?;
                writeln!(b, "  rhs:")?;
                writeln!(b, "{}", indent(&rhs, 4))?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for AstFnCall {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "  ident: {}", self.ident)?;
        writeln!(b, "  args:")?;
        writeln!(b, "{}", indent(&self.args, 4))?;

        Ok(())
    }
}

impl fmt::Display for AstArgumentList {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        if let AstArgumentList::Nonempty { expr, next } = self {
            writeln!(b, "{}", expr)?;
            writeln!(b, "{}", next)?;
        }

        Ok(())
    }
}

impl fmt::Display for AstArithOp {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            AstArithOp::Add => "+",
            AstArithOp::Sub => "-",
            AstArithOp::Mul => "*",
            AstArithOp::Div => "/",
        };
        write!(b, "ArithOp({})", op)
    }
}

impl fmt::Display for AstConst {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "Const({})", self.value())
    }
}

impl fmt::Display for AstVar {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "Var({})", self.0)
    }
}

impl fmt::Display for AstIdent {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "Ident({})", self.ident())
    }
}
