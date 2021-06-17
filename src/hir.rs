use crate::span::Span;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;

pub const SCRIPT_ROOT_FN_NAME: &str = "__start";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FnId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StmtId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExprId(usize);

#[derive(Debug)]
pub struct ItemIdGenerator {
    current: usize,
}

impl ItemIdGenerator {
    pub fn new() -> Self {
        Self { current: 0 }
    }

    pub fn gen_fn(&mut self) -> FnId {
        FnId(self.next_id())
    }

    pub fn gen_scope(&mut self) -> ScopeId {
        ScopeId(self.next_id())
    }

    pub fn gen_var(&mut self) -> VarId {
        VarId(self.next_id())
    }

    pub fn gen_stmt(&mut self) -> StmtId {
        StmtId(self.next_id())
    }

    pub fn gen_expr(&mut self) -> ExprId {
        ExprId(self.next_id())
    }

    fn next_id(&mut self) -> usize {
        let id = self.current;
        self.current += 1;
        id
    }
}

impl Default for ItemIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct HirProgram {
    pub scopes: BTreeMap<ScopeId, HirScope>,
    pub start_fn_id: FnId,
    pub fndecls: BTreeMap<FnId, HirFnDecl>,
    pub fnbodies: BTreeMap<FnId, HirFnBody>,
    pub stmts: BTreeMap<StmtId, HirStmt>,
    pub exprs: BTreeMap<ExprId, HirArithExpr>,
}

impl HirProgram {
    pub fn scope(&self, id: ScopeId) -> &HirScope {
        self.scopes
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl(&self, id: FnId) -> &HirFnDecl {
        self.fndecls
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody(&self, id: FnId) -> &HirFnBody {
        self.fnbodies
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn stmt(&self, id: StmtId) -> &HirStmt {
        self.stmts
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: statement of id {:?} not registered", id))
    }

    pub fn expr(&self, id: ExprId) -> &HirArithExpr {
        self.exprs
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut HirScope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl_mut(&mut self, id: FnId) -> &mut HirFnDecl {
        self.fndecls
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody_mut(&mut self, id: FnId) -> &mut HirFnBody {
        self.fnbodies
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn stmt_mut(&mut self, id: StmtId) -> &mut HirStmt {
        self.stmts
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: statement of id {:?} not registered", id))
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut HirArithExpr {
        self.exprs
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct HirScope {
    /// ID for this scope
    pub id: ScopeId,

    /// ID for the parent scope (the parent in terms of tree structure)
    ///
    /// None means this is a root scope
    pub parent_id: Option<ScopeId>,

    /// IDs of functions declared in this scope
    ///
    /// Only functions exactly in this scope. In other words, it's not related to the visibility:
    /// functions in the parent scope is actually visible in the child scopes, but they will not be
    /// listed in here.
    pub fn_ids: Vec<FnId>,

    /// Variables in scope
    // In lowering phase, variables are just collected from function parameters, function name and
    // local variables. Because this language doesn't have explicit variable declaration statement
    // (yet), all I can do here is to collect variables of the same name just once. Variable name
    // conflicts are later checked in resolver.
    pub vars: BTreeMap<VarId, HirVar>,
}

impl HirScope {
    pub fn new(id: ScopeId, parent_id: Option<ScopeId>) -> HirScope {
        HirScope {
            id,
            parent_id,
            fn_ids: Vec::new(),
            vars: BTreeMap::new(),
        }
    }

    pub fn var(&self, id: VarId) -> &HirVar {
        self.vars
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: variable of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct HirFnDecl {
    /// ID for this function
    pub id: FnId,

    /// Scope which this function is in. Not what this function creates inside it.
    pub scope_id: ScopeId,

    pub span: Span,
    pub name: Ident,
    pub params: Vec<HirParam>,
    pub ret_var: RefCell<ResolveStatus<VarId>>,
    pub ret_ty: HirTy,
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub span: Span,
    // `name` is Option since builtin functions doesn't have parameter names. Their parameters
    // cannot be found as our interpreter's local variable table, since that's completely native
    // Rust closure arguments.
    pub res: Option<RefCell<ResolveStatus<VarId>>>,
    pub ty: HirTy,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub span: Span,
    pub ident: String,
}

#[derive(Debug, Clone)]
pub enum ResolveStatus<T: Clone> {
    Resolved(T),
    Unresolved(Ident),
    Err(Ident),
}

#[derive(Debug, Clone)]
pub enum TypeckStatus {
    Revealed(TyKind),
    Infer,
    Err,
}

#[derive(Debug, Clone)]
pub struct HirTy {
    pub span: Span,
    pub res: RefCell<ResolveStatus<TypeckStatus>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Void,
    Int,
    Float,
    Bool,
}

impl fmt::Display for TyKind {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyKind::Void => write!(b, "void"),
            TyKind::Int => write!(b, "int"),
            TyKind::Float => write!(b, "float"),
            TyKind::Bool => write!(b, "bool"),
        }
    }
}

impl TyKind {
    pub fn is_numeric(&self) -> bool {
        matches!(self, TyKind::Int | TyKind::Float)
    }

    pub fn is_comparable(&self) -> bool {
        matches!(self, TyKind::Int | TyKind::Float)
    }

    pub fn is_equatable(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct HirFnBody {
    pub id: FnId,
    pub inner_scope_id: ScopeId,
    pub kind: HirFnBodyKind,
}

pub enum HirFnBodyKind {
    Stmt(StmtId),
    Builtin(Box<dyn Fn(Vec<Value>) -> Value>),
}

impl fmt::Debug for HirFnBodyKind {
    fn fmt(&self, b: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HirFnBodyKind::Stmt(stmt) => b.debug_tuple("Stmt").field(&stmt).finish(),
            HirFnBodyKind::Builtin(_) => {
                b.debug_tuple("Builtin").field(&format_args!("_")).finish()
            }
        }
    }
}

#[derive(Debug)]
pub struct HirVar {
    pub id: VarId,
    pub name: Ident,
    pub ty: HirTy,
}

#[derive(Debug)]
pub struct HirStmt {
    pub span: Span,
    pub kind: HirStmtKind,
}

#[derive(Debug)]
pub enum HirStmtKind {
    FnDef(FnId),
    If(HirIfStmt),
    While(HirWhileStmt),
    Begin(HirBeginStmt),
    Assg(HirAssgStmt),
    Dump(HirDumpStmt),
}

impl From<HirDumpStmt> for HirStmt {
    fn from(v: HirDumpStmt) -> Self {
        HirStmt {
            span: v.span,
            kind: HirStmtKind::Dump(v),
        }
    }
}

impl From<HirAssgStmt> for HirStmt {
    fn from(v: HirAssgStmt) -> Self {
        HirStmt {
            span: v.span,
            kind: HirStmtKind::Assg(v),
        }
    }
}

impl From<HirBeginStmt> for HirStmt {
    fn from(v: HirBeginStmt) -> Self {
        HirStmt {
            span: v.span,
            kind: HirStmtKind::Begin(v),
        }
    }
}

impl From<HirWhileStmt> for HirStmt {
    fn from(v: HirWhileStmt) -> Self {
        HirStmt {
            span: v.span,
            kind: HirStmtKind::While(v),
        }
    }
}

impl From<HirIfStmt> for HirStmt {
    fn from(v: HirIfStmt) -> Self {
        HirStmt {
            span: v.span,
            kind: HirStmtKind::If(v),
        }
    }
}

#[derive(Debug)]
pub struct HirIfStmt {
    pub span: Span,
    pub cond_id: ExprId,
    pub then_id: StmtId,
    pub otherwise_id: Option<StmtId>,
}

#[derive(Debug)]
pub struct HirWhileStmt {
    pub span: Span,
    pub cond_id: ExprId,
    pub body_id: StmtId,
}

#[derive(Debug)]
pub struct HirBeginStmt {
    pub span: Span,
    pub stmt_ids: Vec<StmtId>,
}

#[derive(Debug)]
pub struct HirAssgStmt {
    pub span: Span,
    pub var: Box<HirVarRef>,
    pub expr_id: ExprId,
}

#[derive(Debug)]
pub struct HirDumpStmt {
    pub span: Span,
    pub var: Box<HirVarRef>,
}

#[derive(Debug)]
pub struct HirArithExpr {
    pub span: Span,
    pub ty: HirTy,
    pub kind: HirArithExprKind,
}

#[derive(Debug)]
pub enum HirArithExprKind {
    Primary(Box<HirPrimaryExpr>),
    UnaryOp(UnaryOp, ExprId),
    BinOp(BinOp, ExprId, ExprId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(b, "-"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}

impl BinOp {
    pub fn is_arith(&self) -> bool {
        matches!(self, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div)
    }

    pub fn is_compare(&self) -> bool {
        matches!(self, BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge)
    }

    pub fn is_equal(&self) -> bool {
        matches!(self, BinOp::Eq | BinOp::Ne)
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(b, "+"),
            BinOp::Sub => write!(b, "-"),
            BinOp::Mul => write!(b, "*"),
            BinOp::Div => write!(b, "/"),
            BinOp::Lt => write!(b, "<"),
            BinOp::Gt => write!(b, ">"),
            BinOp::Le => write!(b, "<="),
            BinOp::Ge => write!(b, ">="),
            BinOp::Eq => write!(b, "=="),
            BinOp::Ne => write!(b, "!="),
        }
    }
}

#[derive(Debug)]
pub struct HirPrimaryExpr {
    pub span: Span,
    pub ty: HirTy,
    pub kind: HirPrimaryExprKind,
}

#[derive(Debug)]
pub enum HirPrimaryExprKind {
    Var(Box<HirVarRef>),
    Const(Box<HirConst>),
    FnCall(Box<HirFnCall>),
    Paren(ExprId),
}

#[derive(Debug)]
pub struct HirVarRef {
    pub span: Span,
    pub res: RefCell<ResolveStatus<VarId>>,
}

#[derive(Debug)]
pub struct HirConst {
    pub span: Span,
    pub ty: HirTy,
    pub value: Value,
}

// FIXME: we shouldn't require Clone...
#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Void => write!(b, "(void)"),
            Value::Int(v) => write!(b, "{} (int)", v),
            Value::Float(v) => write!(b, "{} (float)", v),
            Value::Bool(v) => write!(b, "{} (bool)", v),
        }
    }
}

impl Value {
    pub fn unwrap_void(&self) {
        match self {
            Value::Void => (),
            _ => panic!("unwrap_void() called on a non-void value"),
        }
    }

    pub fn unwrap_int(&self) -> i64 {
        match self {
            Value::Int(v) => *v,
            _ => panic!("unwrap_int() called on a non-int value"),
        }
    }

    pub fn unwrap_float(&self) -> f64 {
        match self {
            Value::Float(v) => *v,
            _ => panic!("unwrap_float() called on a non-float value"),
        }
    }

    pub fn unwrap_bool(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            _ => panic!("unwrap_bool() called on a non-bool value"),
        }
    }
}

#[derive(Debug)]
pub struct HirFnCall {
    pub span: Span,
    pub span_name: Span,
    pub res: RefCell<ResolveStatus<FnId>>,
    pub args: Vec<ExprId>,
}
