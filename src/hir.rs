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
pub struct Program {
    pub scopes: BTreeMap<ScopeId, Scope>,
    pub start_fn_id: FnId,
    pub fndecls: BTreeMap<FnId, FnDecl>,
    pub fnbodies: BTreeMap<FnId, FnBody>,
}

impl Program {
    pub fn scope(&self, id: ScopeId) -> &Scope {
        self.scopes
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl(&self, id: FnId) -> &FnDecl {
        self.fndecls
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody(&self, id: FnId) -> &FnBody {
        self.fnbodies
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl_mut(&mut self, id: FnId) -> &mut FnDecl {
        self.fndecls
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody_mut(&mut self, id: FnId) -> &mut FnBody {
        self.fnbodies
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct Scope {
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
    pub vars: BTreeMap<VarId, Var>,
}

impl Scope {
    pub fn new(id: ScopeId, parent_id: Option<ScopeId>) -> Scope {
        Scope {
            id,
            parent_id,
            fn_ids: Vec::new(),
            vars: BTreeMap::new(),
        }
    }

    pub fn var(&self, id: VarId) -> &Var {
        self.vars
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: variable of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct FnDecl {
    /// ID for this function
    pub id: FnId,

    /// Scope which this function is in. Not what this function creates inside it.
    pub scope_id: ScopeId,

    pub span: Span,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_var: RefCell<ResolveStatus<VarId>>,
    pub ret_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    // `name` is Option since builtin functions doesn't have parameter names. Their parameters
    // cannot be found as our interpreter's local variable table, since that's completely native
    // Rust closure arguments.
    pub res: Option<RefCell<ResolveStatus<VarId>>>,
    pub ty: Ty,
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
pub struct Ty {
    pub span: Span,
    pub res: RefCell<ResolveStatus<TypeckStatus>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Void,
    Int,
    Float,
}

impl fmt::Display for TyKind {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyKind::Void => write!(b, "void"),
            TyKind::Int => write!(b, "int"),
            TyKind::Float => write!(b, "float"),
        }
    }
}

#[derive(Debug)]
pub struct FnBody {
    pub id: FnId,
    pub inner_scope_id: ScopeId,
    pub kind: FnBodyKind,
}

pub enum FnBodyKind {
    Stmt(Box<BeginStmt>),
    Builtin(Box<dyn Fn(Vec<Value>) -> Value>),
}

impl fmt::Debug for FnBodyKind {
    fn fmt(&self, b: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FnBodyKind::Stmt(stmt) => b.debug_tuple("Stmt").field(&stmt).finish(),
            FnBodyKind::Builtin(_) => b.debug_tuple("Builtin").field(&format_args!("_")).finish(),
        }
    }
}

#[derive(Debug)]
pub struct Var {
    pub id: VarId,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    FnDef(FnId),
    If(IfStmt),
    While(WhileStmt),
    Begin(BeginStmt),
    Assg(AssgStmt),
    Dump(DumpStmt),
}

#[derive(Debug)]
pub struct IfStmt {
    pub span: Span,
    pub cond: Box<BoolExpr>,
    pub then: Box<Stmt>,
    pub otherwise: Box<Stmt>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub span: Span,
    pub cond: Box<BoolExpr>,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct BeginStmt {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct AssgStmt {
    pub span: Span,
    pub var: Box<VarRef>,
    pub expr: Box<ArithExpr>,
}

#[derive(Debug)]
pub struct DumpStmt {
    pub span: Span,
    pub var: Box<VarRef>,
}

#[derive(Debug)]
pub struct BoolExpr {
    pub span: Span,
    pub op: Box<CompareOp>,
    pub lhs: Box<ArithExpr>,
    pub rhs: Box<ArithExpr>,
}

#[derive(Debug)]
pub struct CompareOp {
    pub span: Span,
    pub kind: CompareOpKind,
}

#[derive(Debug)]
pub enum CompareOpKind {
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}

#[derive(Debug)]
pub struct ArithExpr {
    pub span: Span,
    pub ty: Ty,
    pub kind: ArithExprKind,
}

#[derive(Debug)]
pub enum ArithExprKind {
    Primary(Box<PrimaryExpr>),
    UnaryOp(UnaryOp, Box<ArithExpr>),
    BinOp(BinOp, Box<ArithExpr>, Box<ArithExpr>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct PrimaryExpr {
    pub span: Span,
    pub ty: Ty,
    pub kind: PrimaryExprKind,
}

#[derive(Debug)]
pub enum PrimaryExprKind {
    Var(Box<VarRef>),
    Const(Box<Const>),
    FnCall(Box<FnCall>),
    Paren(Box<ArithExpr>),
}

#[derive(Debug)]
pub struct VarRef {
    pub span: Span,
    pub res: RefCell<ResolveStatus<VarId>>,
}

#[derive(Debug)]
pub struct Const {
    pub span: Span,
    pub ty: Ty,
    pub value: Value,
}

// FIXME: we shouldn't require Clone...
#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Void => write!(b, "(void)"),
            Value::Int(v) => write!(b, "{} (int)", v),
            Value::Float(v) => write!(b, "{} (float)", v),
        }
    }
}

#[derive(Debug)]
pub struct FnCall {
    pub span: Span,
    pub span_name: Span,
    pub res: RefCell<ResolveStatus<FnId>>,
    pub args: Vec<ArithExpr>,
}
