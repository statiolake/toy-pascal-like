use crate::hir::*;
use crate::span::Span;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug)]
pub struct RhirProgram {
    pub scopes: BTreeMap<ScopeId, RhirScope>,
    pub start_fn_id: FnId,
    pub fndecls: BTreeMap<FnId, RhirFnDecl>,
    pub fnbodies: BTreeMap<FnId, RhirFnBody>,
    pub stmts: BTreeMap<StmtId, RhirStmt>,
    pub exprs: BTreeMap<ExprId, RhirArithExpr>,
}

impl RhirProgram {
    pub fn scope(&self, id: ScopeId) -> &RhirScope {
        self.scopes
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl(&self, id: FnId) -> &RhirFnDecl {
        self.fndecls
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody(&self, id: FnId) -> &RhirFnBody {
        self.fnbodies
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn stmt(&self, id: StmtId) -> &RhirStmt {
        self.stmts
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: statement of id {:?} not registered", id))
    }

    pub fn expr(&self, id: ExprId) -> &RhirArithExpr {
        self.exprs
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut RhirScope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl_mut(&mut self, id: FnId) -> &mut RhirFnDecl {
        self.fndecls
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody_mut(&mut self, id: FnId) -> &mut RhirFnBody {
        self.fnbodies
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn stmt_mut(&mut self, id: StmtId) -> &mut RhirStmt {
        self.stmts
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: statement of id {:?} not registered", id))
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut RhirArithExpr {
        self.exprs
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct RhirScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub fn_ids: Vec<FnId>,
    pub vars: BTreeMap<VarId, RhirVar>,
}

impl RhirScope {
    pub fn var(&self, id: VarId) -> &RhirVar {
        self.vars
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: variable of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct RhirFnDecl {
    /// ID for this function
    pub id: FnId,

    /// Scope which this function is in. Not what this function creates inside it.
    pub scope_id: ScopeId,

    pub span: Span,
    pub name: Ident,
    pub params: Vec<RhirParam>,
    pub ret_var: VarId,
    pub ret_ty: RhirTy,
}

#[derive(Debug, Clone)]
pub struct RhirParam {
    pub span: Span,
    // None if this parameter is of buitlin functions: they are just Rust native closures so they
    // don't have corresponding local variables for parameters.
    pub res: Option<VarId>,
    pub ty: RhirTy,
}

#[derive(Debug, Clone)]
pub struct RhirTy {
    pub span: Span,
    pub res: RefCell<TypeckStatus>,
}

#[derive(Debug)]
pub struct RhirFnBody {
    pub id: FnId,
    pub inner_scope_id: ScopeId,
    pub kind: RhirFnBodyKind,
}

pub enum RhirFnBodyKind {
    Stmt(StmtId),
    Builtin(Box<dyn Fn(Vec<Value>) -> Value>),
}

impl fmt::Debug for RhirFnBodyKind {
    fn fmt(&self, b: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RhirFnBodyKind::Stmt(stmt) => b.debug_tuple("Stmt").field(&stmt).finish(),
            RhirFnBodyKind::Builtin(_) => {
                b.debug_tuple("Builtin").field(&format_args!("_")).finish()
            }
        }
    }
}

#[derive(Debug)]
pub struct RhirVar {
    pub id: VarId,
    pub name: Ident,
    pub ty: RhirTy,
}

#[derive(Debug)]
pub struct RhirStmt {
    pub span: Span,
    pub kind: RhirStmtKind,
}

#[derive(Debug)]
pub enum RhirStmtKind {
    FnDef(FnId),
    If(RhirIfStmt),
    While(RhirWhileStmt),
    Begin(RhirBeginStmt),
    Assg(RhirAssgStmt),
    Dump(RhirDumpStmt),
}

#[derive(Debug)]
pub struct RhirIfStmt {
    pub span: Span,
    pub cond_id: ExprId,
    pub then_id: StmtId,
    pub otherwise_id: Option<StmtId>,
}

#[derive(Debug)]
pub struct RhirWhileStmt {
    pub span: Span,
    pub cond_id: ExprId,
    pub body_id: StmtId,
}

#[derive(Debug)]
pub struct RhirBeginStmt {
    pub span: Span,
    pub stmt_ids: Vec<StmtId>,
}

#[derive(Debug)]
pub struct RhirAssgStmt {
    pub span: Span,
    pub var: Box<RhirVarRef>,
    pub expr_id: ExprId,
}

#[derive(Debug)]
pub struct RhirDumpStmt {
    pub span: Span,
    pub var: Box<RhirVarRef>,
}

#[derive(Debug)]
pub struct RhirArithExpr {
    pub span: Span,
    pub ty: RhirTy,
    pub kind: RhirArithExprKind,
}

#[derive(Debug)]
pub enum RhirArithExprKind {
    Primary(Box<RhirPrimaryExpr>),
    UnaryOp(UnaryOp, ExprId),
    BinOp(BinOp, ExprId, ExprId),
}

#[derive(Debug)]
pub struct RhirPrimaryExpr {
    pub span: Span,
    pub ty: RhirTy,
    pub kind: RhirPrimaryExprKind,
}

#[derive(Debug)]
pub enum RhirPrimaryExprKind {
    Var(Box<RhirVarRef>),
    Const(Box<RhirConst>),
    FnCall(Box<RhirFnCall>),
    Paren(ExprId),
}

#[derive(Debug)]
pub struct RhirVarRef {
    pub span: Span,
    pub res: VarId,
}

#[derive(Debug)]
pub struct RhirConst {
    pub span: Span,
    pub ty: RhirTy,
    pub value: Value,
}

#[derive(Debug)]
pub struct RhirFnCall {
    pub span: Span,
    pub span_name: Span,
    pub res: FnId,
    pub arg_ids: Vec<ExprId>,
}
