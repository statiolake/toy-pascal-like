use crate::hir::*;
use crate::span::Span;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug)]
pub struct RhirContext {
    pub scopes: BTreeMap<ScopeId, RhirScope>,
    pub start_fn_id: FnId,
    pub fndecls: BTreeMap<FnId, RhirFnDecl>,
    pub fnbodies: BTreeMap<FnId, RhirFnBody>,
    pub stmts: BTreeMap<StmtId, RhirStmt>,
    pub exprs: BTreeMap<ExprId, RhirExpr>,
    pub res_ty_kinds: BTreeMap<ResTyKindId, RefCell<TypeckStatus>>,
    pub res_fn_ids: BTreeMap<ResFnIdId, FnId>,
    pub res_var_ids: BTreeMap<ResVarIdId, VarId>,
}

impl RhirContext {
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

    pub fn expr(&self, id: ExprId) -> &RhirExpr {
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

    pub fn expr_mut(&mut self, id: ExprId) -> &mut RhirExpr {
        self.exprs
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }

    pub fn res_ty_kind(&self, id: ResTyKindId) -> &RefCell<TypeckStatus> {
        self.res_ty_kinds.get(&id).unwrap_or_else(|| {
            panic!(
                "internal error: type resolution of id {:?} not registered",
                id
            )
        })
    }

    pub fn res_fn_id(&self, id: ResFnIdId) -> FnId {
        *self.res_fn_ids.get(&id).unwrap_or_else(|| {
            panic!(
                "internal error: FnId resolution of id {:?} not registered",
                id
            )
        })
    }

    pub fn res_var_id(&self, id: ResVarIdId) -> VarId {
        *self.res_var_ids.get(&id).unwrap_or_else(|| {
            panic!(
                "internal error: VarId resolution of id {:?} not registered",
                id
            )
        })
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

    pub fn var_mut(&mut self, id: VarId) -> &mut RhirVar {
        self.vars
            .get_mut(&id)
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
    pub ret_var: ResVarIdId,
    pub ret_ty: RhirTy,
}

#[derive(Debug, Clone)]
pub struct RhirParam {
    pub span: Span,
    // None if this parameter is of buitlin functions: they are just Rust native closures so they
    // don't have corresponding local variables for parameters.
    pub res_id: Option<ResVarIdId>,
    pub ty: RhirTy,
}

#[derive(Debug, Clone)]
pub struct RhirTy {
    pub span: Span,
    pub res_id: ResTyKindId,
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
    pub var: RhirVarRef,
    pub expr_id: ExprId,
}

#[derive(Debug)]
pub struct RhirDumpStmt {
    pub span: Span,
    pub var: RhirVarRef,
}

#[derive(Debug)]
pub struct RhirExpr {
    pub span: Span,
    pub ty: RhirTy,
    pub kind: RhirExprKind,
}

#[derive(Debug)]
pub enum RhirExprKind {
    UnaryOp(UnaryOp, ExprId),
    BinOp(BinOp, ExprId, ExprId),
    Var(RhirVarRef),
    Const(RhirConst),
    FnCall(RhirFnCall),
    Paren(ExprId),
}

#[derive(Debug)]
pub struct RhirVarRef {
    pub span: Span,
    pub res_id: ResVarIdId,
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
    pub res_id: ResFnIdId,
    pub arg_ids: Vec<ExprId>,
}
