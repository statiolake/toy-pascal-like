use crate::hir::*;
use crate::span::Span;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug)]
pub struct ThirContext {
    pub scopes: BTreeMap<ScopeId, ThirScope>,
    pub start_fn_id: FnId,
    pub fndecls: BTreeMap<FnId, ThirFnDecl>,
    pub fnbodies: BTreeMap<FnId, ThirFnBody>,
    pub stmts: BTreeMap<StmtId, ThirStmt>,
    pub exprs: BTreeMap<ExprId, ThirExpr>,
    pub res_ty_kinds: BTreeMap<ResTyKindId, TyKind>,
    pub res_fn_ids: BTreeMap<ResFnIdId, FnId>,
    pub res_var_ids: BTreeMap<ResVarIdId, VarId>,
}

impl ThirContext {
    pub fn scope(&self, id: ScopeId) -> &ThirScope {
        self.scopes
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl(&self, id: FnId) -> &ThirFnDecl {
        self.fndecls
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody(&self, id: FnId) -> &ThirFnBody {
        self.fnbodies
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn stmt(&self, id: StmtId) -> &ThirStmt {
        self.stmts
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: statement of id {:?} not registered", id))
    }

    pub fn expr(&self, id: ExprId) -> &ThirExpr {
        self.exprs
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut ThirScope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: scope of id {:?} not registered", id))
    }

    pub fn fndecl_mut(&mut self, id: FnId) -> &mut ThirFnDecl {
        self.fndecls
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn fnbody_mut(&mut self, id: FnId) -> &mut ThirFnBody {
        self.fnbodies
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: function of id {:?} not registered", id))
    }

    pub fn stmt_mut(&mut self, id: StmtId) -> &mut ThirStmt {
        self.stmts
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: statement of id {:?} not registered", id))
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut ThirExpr {
        self.exprs
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: expression of id {:?} not registered", id))
    }

    pub fn res_ty_kind(&self, id: ResTyKindId) -> &TyKind {
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
pub struct ThirScope {
    pub id: ScopeId,
    pub parent_id: Option<ScopeId>,
    pub fn_ids: Vec<FnId>,
    pub vars: BTreeMap<VarId, ThirVar>,
}

impl ThirScope {
    pub fn var(&self, id: VarId) -> &ThirVar {
        self.vars
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: variable of id {:?} not registered", id))
    }

    pub fn var_mut(&mut self, id: VarId) -> &mut ThirVar {
        self.vars
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: variable of id {:?} not registered", id))
    }
}

#[derive(Debug)]
pub struct ThirFnDecl {
    /// ID for this function
    pub id: FnId,

    /// Scope which this function is in. Not what this function creates inside it.
    pub scope_id: ScopeId,

    pub span: Span,
    pub name: Ident,
    pub params: Vec<ThirParam>,
    pub ret_var: ResVarIdId,
    pub ret_ty: ThirTy,
}

#[derive(Debug, Clone)]
pub struct ThirParam {
    pub span: Span,
    pub res_id: Option<ResVarIdId>,
    pub ty: ThirTy,
}

#[derive(Debug, Clone)]
pub struct ThirTy {
    pub span: Span,
    pub res_id: ResTyKindId,
}

#[derive(Debug)]
pub struct ThirFnBody {
    pub id: FnId,
    pub inner_scope_id: ScopeId,
    pub kind: ThirFnBodyKind,
}

pub enum ThirFnBodyKind {
    Stmt(StmtId),
    Builtin(Box<dyn Fn(Vec<Value>) -> Value>),
}

impl fmt::Debug for ThirFnBodyKind {
    fn fmt(&self, b: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ThirFnBodyKind::Stmt(stmt) => b.debug_tuple("Stmt").field(&stmt).finish(),
            ThirFnBodyKind::Builtin(_) => {
                b.debug_tuple("Builtin").field(&format_args!("_")).finish()
            }
        }
    }
}

#[derive(Debug)]
pub struct ThirVar {
    pub id: VarId,
    pub name: Ident,
    pub ty: ThirTy,
}

#[derive(Debug)]
pub struct ThirStmt {
    pub span: Span,
    pub kind: ThirStmtKind,
}

#[derive(Debug)]
pub enum ThirStmtKind {
    FnDef(FnId),
    If(ThirIfStmt),
    While(ThirWhileStmt),
    Begin(ThirBeginStmt),
    Assg(ThirAssgStmt),
    Dump(ThirDumpStmt),
}

#[derive(Debug)]
pub struct ThirIfStmt {
    pub span: Span,
    pub cond_id: ExprId,
    pub then_id: StmtId,
    pub otherwise_id: Option<StmtId>,
}

#[derive(Debug)]
pub struct ThirWhileStmt {
    pub span: Span,
    pub cond_id: ExprId,
    pub body_id: StmtId,
}

#[derive(Debug)]
pub struct ThirBeginStmt {
    pub span: Span,
    pub stmt_ids: Vec<StmtId>,
}

#[derive(Debug)]
pub struct ThirAssgStmt {
    pub span: Span,
    pub var: ThirVarRef,
    pub expr_id: ExprId,
}

#[derive(Debug)]
pub struct ThirDumpStmt {
    pub span: Span,
    pub var: ThirVarRef,
}

#[derive(Debug)]
pub struct ThirExpr {
    pub span: Span,
    pub ty: ThirTy,
    pub kind: ThirExprKind,
}

#[derive(Debug)]
pub enum ThirExprKind {
    UnaryOp(UnaryOp, ExprId),
    BinOp(BinOp, ExprId, ExprId),
    Var(ThirVarRef),
    Const(ThirConst),
    FnCall(ThirFnCall),
    Paren(ExprId),
}

#[derive(Debug)]
pub struct ThirVarRef {
    pub span: Span,
    pub res_id: ResVarIdId,
}

#[derive(Debug)]
pub struct ThirConst {
    pub span: Span,
    pub ty: ThirTy,
    pub value: Value,
}

#[derive(Debug)]
pub struct ThirFnCall {
    pub span: Span,
    pub span_name: Span,
    pub res_id: ResFnIdId,
    pub arg_ids: Vec<ExprId>,
}
