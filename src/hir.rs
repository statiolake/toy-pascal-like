use crate::ast::*;
use crate::span::Span;
use maplit::hashmap;
use std::collections::HashMap;

const SCRIPT_ROOT_FN_NAME: &str = "__start";

#[derive(thiserror::Error, Debug)]
#[error("todo")]
pub struct LoweringError {}

pub type Result<T, E = LoweringError> = std::result::Result<T, E>;

pub fn lower_ast(stmt: &Ast<AstStmt>) -> Result<Program> {
    let (mut lctx, root_scope) = LoweringContext::new();

    // register script root function
    let (start_fn, start_scope) = lctx.register_fndecl(
        root_scope,
        stmt.span,
        Ident {
            span: Span::new_zero(),
            ident: SCRIPT_ROOT_FN_NAME.to_string(),
        },
        vec![],
        Ty {
            span: Span::new_zero(),
            res: ResolveStatus::Resolved(TyKind::Void),
        },
    );
    lctx.lower_stmt(start_fn, start_scope, stmt);

    Ok(Program {
        scopes: lctx.scopes,
        start_fn,
        fndecls: lctx.fndecls,
        fnbodies: lctx.fnbodies,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ItemId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeId(usize);

#[derive(Debug)]
pub struct ItemIdGenerator {
    current: usize,
}

impl ItemIdGenerator {
    pub fn new() -> Self {
        Self { current: 0 }
    }

    pub fn gen_item(&mut self) -> ItemId {
        let id = self.current;
        self.current += 1;
        ItemId(id)
    }

    pub fn gen_scope(&mut self) -> ScopeId {
        let id = self.current;
        self.current += 1;
        ScopeId(id)
    }
}

impl Default for ItemIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Program {
    pub scopes: HashMap<ScopeId, Scope>,
    pub start_fn: ItemId,
    pub fndecls: HashMap<ItemId, FnDecl>,
    pub fnbodies: HashMap<ItemId, FnBody>,
}

#[derive(Debug)]
pub struct Scope {
    /// ID for this scope
    id: ScopeId,

    /// ID for the parent scope (the parent in terms of tree structure)
    ///
    /// None means this is a root scope
    parent: Option<ScopeId>,

    /// IDs of functions declared in this scope
    ///
    /// Only functions exactly in this scope. In other words, it's not related to the visibility:
    /// functions in the parent scope is actually visible in the child scopes, but they will not be
    /// listed in here.
    fnids: Vec<ItemId>,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<ScopeId>) -> Scope {
        Scope {
            id,
            parent,
            fnids: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct FnDecl {
    /// ID for this function
    id: ItemId,

    /// Scope which this function is in. Not what this function creates inside it.
    scope: ScopeId,

    span: Span,
    name: Ident,
    params: Vec<Param>,
    ret_ty: Ty,
}

#[derive(Debug)]
pub struct FnDef {
    /// ID for this function
    id: ItemId,

    /// Scope which this function created. Not where this function is placed.
    scope_inside: ScopeId,

    body: Box<Stmt>,
}

#[derive(Debug)]
pub struct Param {
    span: Span,
    name: Ident,
    ty: Ty,
}

#[derive(Debug)]
pub struct Ident {
    span: Span,
    ident: String,
}

#[derive(Debug)]
pub enum ResolveStatus<T> {
    Resolved(T),
    Unresolved(Ident),
    Unknown,
    Err,
}

#[derive(Debug)]
pub struct Ty {
    span: Span,
    res: ResolveStatus<TyKind>,
}

#[derive(Debug)]
pub enum TyKind {
    Void,
    Int,
    Float,
}

#[derive(Debug)]
pub struct FnBody {
    stmt: Box<BeginStmt>,
}

#[derive(Debug)]
pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    FnDef(ItemId),
    If(IfStmt),
    While(WhileStmt),
    Begin(BeginStmt),
    Assg(AssgStmt),
    Dump(DumpStmt),
}

#[derive(Debug)]
pub struct IfStmt {
    span: Span,
    cond: Box<BoolExpr>,
    then: Box<Stmt>,
    otherwise: Box<Stmt>,
}

#[derive(Debug)]
pub struct WhileStmt {
    span: Span,
    cond: Box<BoolExpr>,
    body: Box<Stmt>,
}

#[derive(Debug)]
pub struct BeginStmt {
    span: Span,
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct AssgStmt {
    span: Span,
    var: Box<Var>,
    expr: Box<ArithExpr>,
}

#[derive(Debug)]
pub struct DumpStmt {
    span: Span,
    var: Box<Var>,
}

#[derive(Debug)]
pub struct BoolExpr {
    span: Span,
    op: Box<CompareOp>,
    lhs: Box<ArithExpr>,
    rhs: Box<ArithExpr>,
}

#[derive(Debug)]
pub struct CompareOp {
    span: Span,
    kind: CompareOpKind,
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
    span: Span,
    ty: Ty,
    kind: ArithExprKind,
}

#[derive(Debug)]
pub enum ArithExprKind {
    MulExpr(Box<MulExpr>),
    Add(Box<ArithExpr>, Box<MulExpr>),
    Sub(Box<ArithExpr>, Box<MulExpr>),
}

#[derive(Debug)]
pub struct MulExpr {
    span: Span,
    ty: Ty,
    kind: MulExprKind,
}

#[derive(Debug)]
pub enum MulExprKind {
    UnaryExpr(Box<UnaryExpr>),
    Mul(Box<MulExpr>, Box<UnaryExpr>),
    Div(Box<MulExpr>, Box<UnaryExpr>),
}

#[derive(Debug)]
pub struct UnaryExpr {
    span: Span,
    ty: Ty,
    kind: UnaryExprKind,
}

#[derive(Debug)]
pub enum UnaryExprKind {
    PrimaryExpr(Box<PrimaryExpr>),
    Neg(Box<UnaryExpr>),
}

#[derive(Debug)]
pub struct PrimaryExpr {
    span: Span,
    ty: Ty,
    kind: PrimaryExprKind,
}

#[derive(Debug)]
pub enum PrimaryExprKind {
    Var(Box<Var>),
    Const(Box<Const>),
    FnCall(Box<FnCall>),
    Paren(Box<ArithExpr>),
}

#[derive(Debug)]
pub struct Var {
    span: Span,
    ty: Ty,
    name: Ident,
}

#[derive(Debug)]
pub struct Const {
    span: Span,
    ty: Ty,
    value: Value,
}

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
}

#[derive(Debug)]
pub struct FnCall {
    span: Span,
    span_name: Span,
    res: ResolveStatus<ItemId>,
    args: Vec<ArithExpr>,
}

struct LoweringContext {
    scopes: HashMap<ScopeId, Scope>,
    fndecls: HashMap<ItemId, FnDecl>,
    fnbodies: HashMap<ItemId, FnBody>,
    id_gen: ItemIdGenerator,
}

impl LoweringContext {
    fn new() -> (Self, ScopeId) {
        let mut id_gen = ItemIdGenerator::new();

        // prepare root scope
        let id_root_scope = id_gen.gen_scope();
        let root_scope = Scope::new(id_root_scope, None);

        let scopes = hashmap! { id_root_scope => root_scope };
        let lctx = Self {
            scopes,
            fndecls: HashMap::new(),
            fnbodies: HashMap::new(),
            id_gen,
        };

        (lctx, id_root_scope)
    }

    fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("scope {:?} not registered", id))
    }

    fn register_fndecl(
        &mut self,
        curr_scope: ScopeId,
        span: Span,
        name: Ident,
        params: Vec<Param>,
        ret_ty: Ty,
    ) -> (ItemId, ScopeId) {
        // Prepare function
        let id_new_fn = self.id_gen.gen_item();
        let id_new_scope = self.id_gen.gen_scope();
        let new_fn = FnDecl {
            id: id_new_fn,
            scope: curr_scope,
            span,
            name,
            params,
            ret_ty,
        };

        // register this FnDecl
        assert!(
            self.fndecls.insert(id_new_fn, new_fn).is_none(),
            "ID conflict for function {:?}",
            id_new_fn
        );
        self.scope_mut(curr_scope).fnids.push(id_new_fn);

        let new_scope = Scope::new(id_new_scope, Some(curr_scope));
        self.scopes.insert(id_new_scope, new_scope);

        (id_new_fn, id_new_scope)
    }

    fn register_fnbody(&mut self, curr_scope: ScopeId, reg_fn: ItemId, body: FnBody) -> Result<()> {
        self.fnbodies.insert(reg_fn, body);
        debug_assert!(
            self.scope_mut(curr_scope).fnids.contains(&reg_fn),
            "function body of undeclared function ({:?}) is registered",
            reg_fn
        );

        Ok(())
    }

    fn lower_stmt(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstStmt>,
    ) -> Result<Stmt> {
        let lowered = match &stmt.ast {
            AstStmt::FuncdefStmt(s) => StmtKind::FnDef(self.lower_fndef(curr_fn, curr_scope, s)?),
            AstStmt::IfStmt(s) => StmtKind::If(self.lower_if(curr_fn, curr_scope, s)?),
            AstStmt::WhileStmt(s) => StmtKind::While(self.lower_while(curr_fn, curr_scope, s)?),
            AstStmt::BeginStmt(s) => StmtKind::Begin(self.lower_begin(curr_fn, curr_scope, s)?),
            AstStmt::AssgStmt(s) => StmtKind::Assg(self.lower_assg(curr_fn, curr_scope, s)?),
            AstStmt::DumpStmt(s) => StmtKind::Dump(self.lower_dump(curr_fn, curr_scope, s)?),
        };

        Ok(Stmt {
            span: stmt.span,
            kind: lowered,
        })
    }

    fn lower_fndef(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstFuncdefStmt>,
    ) -> Result<ItemId> {
        let mut params = vec![];
        let mut curr_param = &*stmt.ast.params;
        while let Ast {
            span,
            ast: AstParamList::Nonempty { ident, ty, next },
        } = curr_param
        {
            params.push(Param {
                span: *span,
                name: self.lower_ident(curr_fn, curr_scope, ident)?,
                ty: self.lower_ty(curr_fn, curr_scope, ty)?,
            });
            curr_param = &next;
        }

        let ret_ty = self.lower_ty(curr_fn, curr_scope, &stmt.ast.ret_ty)?;
        let new_fn = self.id_gen.gen_item();
        let new_scope = self.id_gen.gen_scope();

        let (new_fn, new_scope) = self.register_fndecl(
            curr_scope,
            stmt.span,
            self.lower_ident(curr_fn, curr_scope, &stmt.ast.name)?,
            params,
            ret_ty,
        );

        let body = FnBody {
            stmt: Box::new(self.lower_begin(new_fn, new_scope, &stmt.ast.body)?),
        };
        self.register_fnbody(curr_scope, new_fn, body)?;

        Ok(new_fn)
    }

    fn lower_if(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstIfStmt>,
    ) -> Result<IfStmt> {
        Ok(IfStmt {
            span: stmt.span,
            cond: Box::new(self.lower_bool_expr(curr_fn, curr_scope, &stmt.ast.cond)?),
            then: Box::new(self.lower_stmt(curr_fn, curr_scope, &stmt.ast.then)?),
            otherwise: Box::new(self.lower_stmt(curr_fn, curr_scope, &stmt.ast.otherwise)?),
        })
    }

    fn lower_while(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstWhileStmt>,
    ) -> Result<WhileStmt> {
        Ok(WhileStmt {
            span: stmt.span,
            cond: Box::new(self.lower_bool_expr(curr_fn, curr_scope, &stmt.ast.cond)?),
            body: Box::new(self.lower_stmt(curr_fn, curr_scope, &stmt.ast.body)?),
        })
    }

    fn lower_begin(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstBeginStmt>,
    ) -> Result<BeginStmt> {
        let mut stmts = vec![];
        let mut curr = &stmt.ast.list.ast;
        while let AstStmtList::Nonempty { stmt, next } = curr {
            stmts.push(self.lower_stmt(curr_fn, curr_scope, stmt)?);
            curr = &next.ast;
        }

        Ok(BeginStmt {
            span: stmt.span,
            stmts,
        })
    }

    fn lower_assg(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstAssgStmt>,
    ) -> Result<AssgStmt> {
        Ok(AssgStmt {
            span: stmt.span,
            var: Box::new(self.lower_var(curr_fn, curr_scope, &stmt.ast.var)?),
            expr: Box::new(self.lower_arith_expr(curr_fn, curr_scope, &stmt.ast.expr)?),
        })
    }

    fn lower_dump(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        stmt: &Ast<AstDumpStmt>,
    ) -> Result<DumpStmt> {
        Ok(DumpStmt {
            span: stmt.span,
            var: Box::new(self.lower_var(curr_fn, curr_scope, &stmt.ast.var)?),
        })
    }

    fn lower_bool_expr(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        expr: &Ast<AstBoolExpr>,
    ) -> Result<BoolExpr> {
        Ok(BoolExpr {
            span: expr.span,
            op: Box::new(self.lower_compare_op(curr_fn, curr_scope, &expr.ast.op)?),
            lhs: Box::new(self.lower_arith_expr(curr_fn, curr_scope, &expr.ast.lhs)?),
            rhs: Box::new(self.lower_arith_expr(curr_fn, curr_scope, &expr.ast.rhs)?),
        })
    }

    fn lower_compare_op(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        op: &Ast<AstCompareOp>,
    ) -> Result<CompareOp> {
        let kind = match op.ast {
            AstCompareOp::Lt => CompareOpKind::Lt,
            AstCompareOp::Gt => CompareOpKind::Gt,
            AstCompareOp::Le => CompareOpKind::Le,
            AstCompareOp::Ge => CompareOpKind::Ge,
            AstCompareOp::Eq => CompareOpKind::Eq,
            AstCompareOp::Ne => CompareOpKind::Ne,
        };

        Ok(CompareOp {
            span: op.span,
            kind,
        })
    }

    fn lower_arith_expr(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        expr: &Ast<AstArithExpr>,
    ) -> Result<ArithExpr> {
        Ok(ArithExpr {
            span: expr.span,
            ty: Ty {
                span: expr.span,
                res: ResolveStatus::Unknown,
            },
            kind: match &expr.ast {
                AstArithExpr::MulExpr(e) => {
                    ArithExprKind::MulExpr(Box::new(self.lower_mul_expr(curr_fn, curr_scope, e)?))
                }
                AstArithExpr::Add(l, r) => ArithExprKind::Add(
                    Box::new(self.lower_arith_expr(curr_fn, curr_scope, l)?),
                    Box::new(self.lower_mul_expr(curr_fn, curr_scope, r)?),
                ),
                AstArithExpr::Sub(l, r) => ArithExprKind::Sub(
                    Box::new(self.lower_arith_expr(curr_fn, curr_scope, l)?),
                    Box::new(self.lower_mul_expr(curr_fn, curr_scope, r)?),
                ),
            },
        })
    }

    fn lower_mul_expr(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        expr: &Ast<AstMulExpr>,
    ) -> Result<MulExpr> {
        Ok(MulExpr {
            span: expr.span,
            ty: Ty {
                span: expr.span,
                res: ResolveStatus::Unknown,
            },
            kind: match &expr.ast {
                AstMulExpr::UnaryExpr(e) => MulExprKind::UnaryExpr({
                    Box::new(self.lower_unary_expr(curr_fn, curr_scope, &e)?)
                }),
                AstMulExpr::Mul(l, r) => MulExprKind::Mul(
                    Box::new(self.lower_mul_expr(curr_fn, curr_scope, l)?),
                    Box::new(self.lower_unary_expr(curr_fn, curr_scope, r)?),
                ),
                AstMulExpr::Div(l, r) => MulExprKind::Div(
                    Box::new(self.lower_mul_expr(curr_fn, curr_scope, l)?),
                    Box::new(self.lower_unary_expr(curr_fn, curr_scope, r)?),
                ),
            },
        })
    }

    fn lower_unary_expr(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        expr: &Ast<AstUnaryExpr>,
    ) -> Result<UnaryExpr> {
        Ok(UnaryExpr {
            span: expr.span,
            ty: Ty {
                span: expr.span,
                res: ResolveStatus::Unknown,
            },
            kind: match &expr.ast {
                AstUnaryExpr::PrimaryExpr(e) => UnaryExprKind::PrimaryExpr(Box::new(
                    self.lower_primary_expr(curr_fn, curr_scope, &e)?,
                )),
                AstUnaryExpr::Neg(e) => {
                    UnaryExprKind::Neg(Box::new(self.lower_unary_expr(curr_fn, curr_scope, &e)?))
                }
            },
        })
    }

    fn lower_primary_expr(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        expr: &Ast<AstPrimaryExpr>,
    ) -> Result<PrimaryExpr> {
        Ok(PrimaryExpr {
            span: expr.span,
            ty: Ty {
                span: expr.span,
                res: ResolveStatus::Unknown,
            },
            kind: match &expr.ast {
                AstPrimaryExpr::Var(var) => {
                    PrimaryExprKind::Var(Box::new(self.lower_var(curr_fn, curr_scope, var)?))
                }
                AstPrimaryExpr::Const(cst) => {
                    PrimaryExprKind::Const(Box::new(self.lower_const(curr_fn, curr_scope, cst)?))
                }
                AstPrimaryExpr::FnCall(call) => {
                    PrimaryExprKind::FnCall(Box::new(self.lower_fncall(curr_fn, curr_scope, call)?))
                }
                AstPrimaryExpr::Paren(expr) => PrimaryExprKind::Paren(Box::new(
                    self.lower_arith_expr(curr_fn, curr_scope, expr)?,
                )),
            },
        })
    }

    fn lower_fncall(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        call: &Ast<AstFnCall>,
    ) -> Result<FnCall> {
        let mut args = vec![];
        let mut curr_arg = &*call.ast.args;
        while let Ast {
            span,
            ast: AstArgumentList::Nonempty { expr, next },
        } = curr_arg
        {
            args.push(self.lower_arith_expr(curr_fn, curr_scope, expr)?);
            curr_arg = &next;
        }

        Ok(FnCall {
            span: call.span,
            span_name: call.ast.ident.span,
            res: ResolveStatus::Unresolved(self.lower_ident(
                curr_fn,
                curr_scope,
                &call.ast.ident,
            )?),
            args,
        })
    }

    fn lower_const(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        cst: &Ast<AstConst>,
    ) -> Result<Const> {
        let (ty, value) = match &cst.ast {
            AstConst::Int(v) => (
                Ty {
                    span: cst.span,
                    res: ResolveStatus::Resolved(TyKind::Int),
                },
                Value::Int(*v),
            ),
            AstConst::Float(v) => (
                Ty {
                    span: cst.span,
                    res: ResolveStatus::Resolved(TyKind::Float),
                },
                Value::Float(*v),
            ),
        };
        Ok(Const {
            span: cst.span,
            ty,
            value,
        })
    }

    fn lower_var(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        var: &Ast<AstVar>,
    ) -> Result<Var> {
        Ok(Var {
            span: var.span,
            ty: Ty {
                span: var.span,
                res: ResolveStatus::Unknown,
            },
            name: self.lower_ident(curr_fn, curr_scope, var.ast.ident())?,
        })
    }

    fn lower_ty(&mut self, curr_fn: ItemId, curr_scope: ScopeId, ty: &Ast<AstTy>) -> Result<Ty> {
        Ok(Ty {
            span: ty.span,
            res: ResolveStatus::Unresolved(self.lower_ident(
                curr_fn,
                curr_scope,
                ty.ast.ident(),
            )?),
        })
    }

    fn lower_ident(
        &mut self,
        curr_fn: ItemId,
        curr_scope: ScopeId,
        ident: &Ast<AstIdent>,
    ) -> Result<Ident> {
        Ok(Ident {
            span: ident.span,
            ident: ident.ast.ident().to_owned(),
        })
    }
}
