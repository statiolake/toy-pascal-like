#[derive(Debug)]
pub enum AstStmt {
    IfStmt(Box<AstIfStmt>),
    WhileStmt(Box<AstWhileStmt>),
    BeginStmt(Box<AstBeginStmt>),
    AssgStmt(Box<AstAssgStmt>),
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
