use crate::span::Span;
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
    FuncdefStmt(Box<Ast<AstFuncdefStmt>>),
    IfStmt(Box<Ast<AstIfStmt>>),
    WhileStmt(Box<Ast<AstWhileStmt>>),
    BeginStmt(Box<Ast<AstBeginStmt>>),
    AssgStmt(Box<Ast<AstAssgStmt>>),
    DumpStmt(Box<Ast<AstDumpStmt>>),
}

derive_from_for_tuple_like!(AstFuncdefStmt => AstStmt::FuncdefStmt, from_funcdef_stmt);
derive_from_for_tuple_like!(AstIfStmt => AstStmt::IfStmt, from_if_stmt);
derive_from_for_tuple_like!(AstWhileStmt => AstStmt::WhileStmt, from_while_stmt);
derive_from_for_tuple_like!(AstBeginStmt => AstStmt::BeginStmt, from_begin_stmt);
derive_from_for_tuple_like!(AstAssgStmt => AstStmt::AssgStmt, from_assg_stmt);
derive_from_for_tuple_like!(AstDumpStmt => AstStmt::DumpStmt, from_dump_stmt);

#[derive(Debug)]
pub struct AstFuncdefStmt {
    pub name: Box<Ast<AstIdent>>,
    pub params: Box<Ast<AstParamList>>,
    pub ret_ty: Box<Ast<AstTy>>,
    pub body: Box<Ast<AstBeginStmt>>,
}

#[derive(Debug)]
pub enum AstParamList {
    Empty,
    Nonempty {
        ident: Box<Ast<AstIdent>>,
        ty: Box<Ast<AstTy>>,
        next: Box<Ast<AstParamList>>,
    },
}

impl AstParamList {
    pub fn empty(span: Span) -> Ast<AstParamList> {
        Ast {
            ast: AstParamList::Empty,
            span,
        }
    }

    pub fn from_elements(
        span: Span,
        ident: Ast<AstIdent>,
        ty: Ast<AstTy>,
        next: Ast<AstParamList>,
    ) -> Ast<AstParamList> {
        let ast = AstParamList::Nonempty {
            ident: Box::new(ident),
            ty: Box::new(ty),
            next: Box::new(next),
        };
        Ast { span, ast }
    }
}

impl fmt::Display for AstParamList {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        if let AstParamList::Nonempty { ident, ty, next } = self {
            writeln!(b, "{}: {}", ident, ty)?;
            writeln!(b, "{}", next)?;
        }

        Ok(())
    }
}

impl AstFuncdefStmt {
    pub fn from_elements(
        span: Span,
        name: Ast<AstIdent>,
        params: Ast<AstParamList>,
        ret_ty: Ast<AstTy>,
        body: Ast<AstBeginStmt>,
    ) -> Ast<AstFuncdefStmt> {
        let ast = AstFuncdefStmt {
            name: Box::new(name),
            params: Box::new(params),
            ret_ty: Box::new(ret_ty),
            body: Box::new(body),
        };
        Ast { span, ast }
    }
}

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
    pub fn from_list(span: Span, list: Ast<AstStmtList>) -> Ast<AstBeginStmt> {
        let ast = AstBeginStmt {
            list: Box::new(list),
        };
        Ast { span, ast }
    }
}

#[derive(Debug)]
pub enum AstStmtList {
    Empty,
    Nonempty {
        stmt: Box<Ast<AstStmt>>,
        next: Box<Ast<AstStmtList>>,
    },
}

impl AstStmtList {
    pub fn empty(span: Span) -> Ast<AstStmtList> {
        Ast {
            ast: AstStmtList::Empty,
            span,
        }
    }

    pub fn from_elements(
        span: Span,
        stmt: Ast<AstStmt>,
        next: Ast<AstStmtList>,
    ) -> Ast<AstStmtList> {
        let ast = AstStmtList::Nonempty {
            stmt: Box::new(stmt),
            next: Box::new(next),
        };

        Ast { span, ast }
    }

    pub fn last_stmt(&self) -> Option<&Ast<AstStmt>> {
        let mut last = None;
        let mut curr = self;
        while let AstStmtList::Nonempty { stmt, next } = curr {
            last = Some(&**stmt);
            curr = &next.ast;
        }
        last
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
    pub fn from_var(span: Span, var: Ast<AstVar>) -> Ast<AstDumpStmt> {
        Ast {
            span,
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
    MulExpr(Box<Ast<AstMulExpr>>),
    Add(Box<Ast<AstArithExpr>>, Box<Ast<AstMulExpr>>),
    Sub(Box<Ast<AstArithExpr>>, Box<Ast<AstMulExpr>>),
}

derive_from_for_tuple_like!(AstMulExpr => AstArithExpr::MulExpr, from_mul);

impl AstArithExpr {
    pub fn add_from_elements(
        span: Span,
        lhs: Ast<AstArithExpr>,
        rhs: Ast<AstMulExpr>,
    ) -> Ast<AstArithExpr> {
        Ast {
            span,
            ast: AstArithExpr::Add(Box::new(lhs), Box::new(rhs)),
        }
    }

    pub fn sub_from_elements(
        span: Span,
        lhs: Ast<AstArithExpr>,
        rhs: Ast<AstMulExpr>,
    ) -> Ast<AstArithExpr> {
        Ast {
            span,
            ast: AstArithExpr::Sub(Box::new(lhs), Box::new(rhs)),
        }
    }
}

#[derive(Debug)]
pub enum AstMulExpr {
    UnaryExpr(Box<Ast<AstUnaryExpr>>),
    Mul(Box<Ast<AstMulExpr>>, Box<Ast<AstUnaryExpr>>),
    Div(Box<Ast<AstMulExpr>>, Box<Ast<AstUnaryExpr>>),
}

derive_from_for_tuple_like!(AstUnaryExpr => AstMulExpr::UnaryExpr, from_unary);

impl AstMulExpr {
    pub fn mul_from_elements(
        span: Span,
        lhs: Ast<AstMulExpr>,
        rhs: Ast<AstUnaryExpr>,
    ) -> Ast<AstMulExpr> {
        Ast {
            span,
            ast: AstMulExpr::Mul(Box::new(lhs), Box::new(rhs)),
        }
    }

    pub fn div_from_elements(
        span: Span,
        lhs: Ast<AstMulExpr>,
        rhs: Ast<AstUnaryExpr>,
    ) -> Ast<AstMulExpr> {
        Ast {
            span,
            ast: AstMulExpr::Div(Box::new(lhs), Box::new(rhs)),
        }
    }
}

#[derive(Debug)]
pub enum AstUnaryExpr {
    PrimaryExpr(Box<Ast<AstPrimaryExpr>>),
    Neg(Box<Ast<AstUnaryExpr>>),
}

derive_from_for_tuple_like!(AstPrimaryExpr => AstUnaryExpr::PrimaryExpr, from_primary);

impl AstUnaryExpr {
    pub fn neg_from(span: Span, expr: Ast<AstUnaryExpr>) -> Ast<AstUnaryExpr> {
        Ast {
            span,
            ast: AstUnaryExpr::Neg(Box::new(expr)),
        }
    }
}

#[derive(Debug)]
pub enum AstPrimaryExpr {
    Var(Box<Ast<AstVar>>),
    Const(Box<Ast<AstConst>>),
    FnCall(Box<Ast<AstFnCall>>),
    Paren(Box<Ast<AstArithExpr>>),
}

derive_from_for_tuple_like!(AstVar => AstPrimaryExpr::Var, from_var);
derive_from_for_tuple_like!(AstConst => AstPrimaryExpr::Const, from_const);
derive_from_for_tuple_like!(AstFnCall => AstPrimaryExpr::FnCall, from_fncall);

impl AstPrimaryExpr {
    pub fn paren_from_elements(span: Span, expr: Ast<AstArithExpr>) -> Ast<AstPrimaryExpr> {
        Ast {
            span,
            ast: AstPrimaryExpr::Paren(Box::new(expr)),
        }
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
pub enum AstConst {
    Int(i64),
    Float(f64),
}

#[derive(Debug)]
pub struct AstVar(pub Ast<AstIdent>);

impl AstVar {
    pub fn from_ident(ident: Ast<AstIdent>) -> Ast<AstVar> {
        Ast {
            span: ident.span,
            ast: AstVar(ident),
        }
    }

    pub fn ident(&self) -> &Ast<AstIdent> {
        &self.0
    }
}

#[derive(Debug)]
pub struct AstTy(pub Ast<AstIdent>);

impl AstTy {
    pub fn from_ident(ident: Ast<AstIdent>) -> Ast<AstTy> {
        Ast {
            span: ident.span,
            ast: AstTy(ident),
        }
    }

    pub fn ident(&self) -> &Ast<AstIdent> {
        &self.0
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
            AstStmt::FuncdefStmt(stmt) => stmt.to_string(),
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

impl fmt::Display for AstFuncdefStmt {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "FuncdefStmt")?;
        writeln!(b, "  name: {}", self.name)?;
        writeln!(b, "  args:")?;
        writeln!(b, "{}", indent(&self.params, 4))?;
        writeln!(b, "  body:")?;
        writeln!(b, "{}", indent(&self.body, 4))?;

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
        if let AstStmtList::Nonempty { stmt, next } = self {
            writeln!(b, "{}", stmt)?;
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
            AstArithExpr::MulExpr(expr) => writeln!(b, "{}", indent(&expr, 4))?,
            AstArithExpr::Add(lhs, rhs) => {
                writeln!(b, "  lhs:")?;
                writeln!(b, "{}", indent(&lhs, 4))?;
                writeln!(b, "  rhs:")?;
                writeln!(b, "{}", indent(&rhs, 4))?;
            }
            AstArithExpr::Sub(lhs, rhs) => {
                writeln!(b, "  lhs:")?;
                writeln!(b, "{}", indent(&lhs, 4))?;
                writeln!(b, "  rhs:")?;
                writeln!(b, "{}", indent(&rhs, 4))?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for AstMulExpr {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "MulExpr")?;
        match self {
            AstMulExpr::UnaryExpr(expr) => writeln!(b, "{}", indent(&expr, 4))?,
            AstMulExpr::Mul(lhs, rhs) => {
                writeln!(b, "  lhs:")?;
                writeln!(b, "{}", indent(&lhs, 4))?;
                writeln!(b, "  rhs:")?;
                writeln!(b, "{}", indent(&rhs, 4))?;
            }
            AstMulExpr::Div(lhs, rhs) => {
                writeln!(b, "  lhs:")?;
                writeln!(b, "{}", indent(&lhs, 4))?;
                writeln!(b, "  rhs:")?;
                writeln!(b, "{}", indent(&rhs, 4))?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for AstUnaryExpr {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "UnaryExpr")?;
        match self {
            AstUnaryExpr::PrimaryExpr(expr) => writeln!(b, "{}", indent(&expr, 4)),
            AstUnaryExpr::Neg(expr) => writeln!(b, "{}", indent(&expr, 4)),
        }
    }
}

impl fmt::Display for AstPrimaryExpr {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        writeln!(b, "PrimaryExpr")?;
        match self {
            AstPrimaryExpr::Var(var) => writeln!(b, "    {}", var)?,
            AstPrimaryExpr::Const(value) => writeln!(b, "    {}", value)?,
            AstPrimaryExpr::FnCall(fncall) => writeln!(b, "{}", indent(&fncall, 4))?,
            AstPrimaryExpr::Paren(expr) => writeln!(b, "{}", indent(&expr, 4))?,
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
        match self {
            AstConst::Int(int) => write!(b, "Const({})", int),
            AstConst::Float(float) => write!(b, "Float({})", float),
        }
    }
}

impl fmt::Display for AstVar {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "Var({})", self.0)
    }
}

impl fmt::Display for AstTy {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "Ty({})", self.0)
    }
}

impl fmt::Display for AstIdent {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(b, "Ident({})", self.ident())
    }
}
