use crate::hir::{BinOp, CompareOp, FnId, Ident, ScopeId, TyKind, UnaryOp, Value, VarId};
use crate::span::Span;
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct Program {
    pub scopes: BTreeMap<ScopeId, Scope>,
    pub start_fn_id: FnId,
    pub fndecls: BTreeMap<FnId, FnDecl>,
    pub fnbodies: BTreeMap<FnId, FnBody>,
}

#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub fn_ids: Vec<FnId>,
    pub vars: BTreeMap<VarId, Var>,
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
pub struct FnDecl {
    /// ID for this function
    pub id: FnId,

    /// Scope which this function is in. Not what this function creates inside it.
    pub scope_id: ScopeId,

    pub span: Span,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub span: Span,
    pub res: TyKind,
}

#[derive(Debug)]
pub struct FnBody {
    pub id: FnId,
    pub inner_scope_id: ScopeId,
    pub stmt: Box<BeginStmt>,
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
    pub res: VarId,
}

#[derive(Debug)]
pub struct Const {
    pub span: Span,
    pub ty: Ty,
    pub value: Value,
}

#[derive(Debug)]
pub struct FnCall {
    pub span: Span,
    pub span_name: Span,
    pub res: FnId,
    pub args: Vec<ArithExpr>,
}
