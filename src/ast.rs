use itertools::Itertools as _;
use std::fmt;

#[derive(Debug)]
pub enum AstStmt {
    IfStmt(Box<AstIfStmt>),
    WhileStmt(Box<AstWhileStmt>),
    BeginStmt(Box<AstBeginStmt>),
    AssgStmt(Box<AstAssgStmt>),
    DumpStmt(Box<AstDumpStmt>),
}

#[derive(Debug)]
pub struct AstIfStmt {
    pub cond: Box<AstBoolExpr>,
    pub then: Box<AstStmt>,
    pub otherwise: Box<AstStmt>,
}

#[derive(Debug)]
pub struct AstWhileStmt {
    pub cond: Box<AstBoolExpr>,
    pub body: Box<AstStmt>,
}

#[derive(Debug)]
pub struct AstBeginStmt {
    pub list: Box<AstStmtList>,
}

#[derive(Debug)]
pub struct AstStmtList {
    pub stmt: Box<AstStmt>,
    pub next: Option<Box<AstStmtList>>,
}

#[derive(Debug)]
pub struct AstAssgStmt {
    pub var: Box<AstVar>,
    pub expr: Box<AstArithExpr>,
}

#[derive(Debug)]
pub struct AstDumpStmt {
    pub var: Box<AstVar>,
}

#[derive(Debug)]
pub struct AstBoolExpr {
    pub lhs: Box<AstArithExpr>,
    pub op: Box<AstCompareOp>,
    pub rhs: Box<AstArithExpr>,
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
    Var(Box<AstVar>),
    Const(Box<AstConst>),
    Op {
        lhs: Box<AstArithExpr>,
        op: Box<AstArithOp>,
        rhs: Box<AstArithExpr>,
    },
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
pub struct AstVar(pub char);

impl AstVar {
    pub fn ident(&self) -> String {
        let AstVar(ch) = self;
        ch.to_string()
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
        write!(b, "Var({})", self.ident())
    }
}
