use crate::ast::*;
use crate::builtins::Builtin;
use crate::span::Span;
use itertools::Itertools;
use maplit::btreemap;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;

const SCRIPT_ROOT_FN_NAME: &str = "__start";

pub fn lower_ast(stmt: &Ast<AstBeginStmt>, builtins: Vec<Builtin>) -> Program {
    let (mut lctx, root_scope_id) = LoweringContext::with_builtins(builtins);

    // register script root function
    let (start_fn_id, start_scope_id) = lctx.register_fndecl(
        root_scope_id,
        stmt.span,
        Ident {
            span: Span::new_zero(),
            ident: SCRIPT_ROOT_FN_NAME.to_string(),
        },
        vec![],
        Ty {
            span: Span::new_zero(),
            res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(
                TyKind::Void,
            ))),
        },
    );

    // lower the entire statement
    let stmt = lctx.lower_begin_stmt(start_fn_id, start_scope_id, stmt);

    lctx.register_fnbody(
        root_scope_id,
        start_fn_id,
        FnBody {
            id: start_fn_id,
            inner_scope_id: start_scope_id,
            kind: FnBodyKind::Stmt(Box::new(stmt)),
        },
    );

    Program {
        scopes: lctx.scopes,
        start_fn_id,
        fndecls: lctx.fndecls,
        fnbodies: lctx.fnbodies,
    }
}

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

struct LoweringContext {
    scopes: BTreeMap<ScopeId, Scope>,
    fndecls: BTreeMap<FnId, FnDecl>,
    fnbodies: BTreeMap<FnId, FnBody>,
    id_gen: ItemIdGenerator,
}

impl LoweringContext {
    fn new() -> (Self, ScopeId) {
        let mut id_gen = ItemIdGenerator::new();

        // prepare root scope
        let root_scope_id = id_gen.gen_scope();
        let root_scope = Scope::new(root_scope_id, None);

        let scopes = btreemap! { root_scope_id => root_scope };
        let lctx = Self {
            scopes,
            fndecls: BTreeMap::new(),
            fnbodies: BTreeMap::new(),
            id_gen,
        };

        (lctx, root_scope_id)
    }

    fn with_builtins(builtins: Vec<Builtin>) -> (Self, ScopeId) {
        let (mut lctx, root_scope_id) = Self::new();

        for builtin in builtins {
            let Builtin {
                name,
                params,
                ret_ty,
                body_kind,
            } = builtin;

            let name = Ident {
                span: Span::new_zero(),
                ident: name,
            };
            let params = params
                .into_iter()
                .map(|(_, ty_kind)| Param {
                    span: Span::new_zero(),
                    res: None,
                    ty: Ty {
                        span: Span::new_zero(),
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(ty_kind))),
                    },
                })
                .collect_vec();
            let ret_ty = Ty {
                span: Span::new_zero(),
                res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(ret_ty))),
            };

            let (fn_id, inner_scope_id) =
                lctx.register_fndecl(root_scope_id, Span::new_zero(), name, params, ret_ty);
            lctx.register_fnbody(
                root_scope_id,
                fn_id,
                FnBody {
                    id: fn_id,
                    inner_scope_id,
                    kind: body_kind,
                },
            );
        }

        (lctx, root_scope_id)
    }

    fn scope(&self, id: ScopeId) -> &Scope {
        self.scopes
            .get(&id)
            .unwrap_or_else(|| panic!("internal error: scope {:?} not registered", id))
    }

    fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes
            .get_mut(&id)
            .unwrap_or_else(|| panic!("internal error: scope {:?} not registered", id))
    }

    fn register_fndecl(
        &mut self,
        curr_scope_id: ScopeId,
        span: Span,
        name: Ident,
        params: Vec<Param>,
        ret_ty: Ty,
    ) -> (FnId, ScopeId) {
        // Prepare function
        let new_fn_id = self.id_gen.gen_fn();
        let new_scope_id = self.id_gen.gen_scope();
        let new_fn = FnDecl {
            id: new_fn_id,
            scope_id: curr_scope_id,
            span,
            name: name.clone(),
            params: params.clone(),
            ret_var: RefCell::new(ResolveStatus::Unresolved(name.clone())),
            ret_ty: ret_ty.clone(),
        };

        // Register this FnDecl
        assert!(
            self.fndecls.insert(new_fn_id, new_fn).is_none(),
            "ID conflict for function {:?}",
            new_fn_id
        );
        self.scope_mut(curr_scope_id).fn_ids.push(new_fn_id);

        // Prepare new scope; variables for return value and parameters should also be registered as
        // a local variable.
        let new_scope = Scope::new(new_scope_id, Some(curr_scope_id));
        self.scopes.insert(new_scope_id, new_scope);

        // return variable
        self.try_register_var(new_scope_id, name, ret_ty);

        // parameters
        for param in params {
            // Register parameters only when they have a name; i.e. except builtin functions written
            // in native Rust closure.
            if let Some(name) = param.res {
                if let ResolveStatus::Unresolved(name) = name.borrow().clone() {
                    self.try_register_var(new_scope_id, name, param.ty);
                } else {
                    panic!("internal error: parameters should not be resolved at this stage");
                }
            }
        }

        (new_fn_id, new_scope_id)
    }

    fn register_fnbody(&mut self, scope_id: ScopeId, reg_fn_id: FnId, body: FnBody) {
        self.fnbodies.insert(reg_fn_id, body);
        debug_assert!(
            self.scope_mut(scope_id).fn_ids.contains(&reg_fn_id),
            "function body of undeclared function ({:?}) is registered",
            reg_fn_id
        );
    }

    fn try_register_var(&mut self, scope_id: ScopeId, name: Ident, ty: Ty) {
        // If the variable of same name is already registered, skip this. Those correctness is
        // checked in resolve phase, where the variable is actually resolved.
        if self
            .scope(scope_id)
            .vars
            .values()
            .any(|v| v.name.ident == name.ident)
        {
            return;
        }

        // We currently does not attach this ID to anywhere; those connection is built in resolve
        // phase.
        let id = self.id_gen.gen_var();
        self.scope_mut(scope_id)
            .vars
            .insert(id, Var { id, name, ty });
    }

    fn lower_stmt(&mut self, fn_id: FnId, scope_id: ScopeId, stmt: &Ast<AstStmt>) -> Stmt {
        let lowered = match &stmt.ast {
            AstStmt::FuncdefStmt(s) => StmtKind::FnDef(self.lower_fndef_stmt(fn_id, scope_id, s)),
            AstStmt::IfStmt(s) => StmtKind::If(self.lower_if_stmt(fn_id, scope_id, s)),
            AstStmt::WhileStmt(s) => StmtKind::While(self.lower_while_stmt(fn_id, scope_id, s)),
            AstStmt::BeginStmt(s) => StmtKind::Begin(self.lower_begin_stmt(fn_id, scope_id, s)),
            AstStmt::AssgStmt(s) => StmtKind::Assg(self.lower_assg_stmt(fn_id, scope_id, s)),
            AstStmt::DumpStmt(s) => StmtKind::Dump(self.lower_dump_stmt(fn_id, scope_id, s)),
        };

        Stmt {
            span: stmt.span,
            kind: lowered,
        }
    }

    fn lower_fndef_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstFuncdefStmt>,
    ) -> FnId {
        let mut params = vec![];
        let mut curr_param = &*stmt.ast.params;
        while let Ast {
            span,
            ast: AstParamList::Nonempty { ident, ty, next },
        } = curr_param
        {
            params.push(Param {
                span: *span,
                res: Some(RefCell::new(ResolveStatus::Unresolved(
                    self.lower_ident(fn_id, scope_id, ident),
                ))),
                ty: self.lower_ty(fn_id, scope_id, ty),
            });
            curr_param = &next;
        }

        let ret_ty = self.lower_ty(fn_id, scope_id, &stmt.ast.ret_ty);
        let ident = self.lower_ident(fn_id, scope_id, &stmt.ast.name);
        let (new_fn_id, new_scope_id) =
            self.register_fndecl(scope_id, stmt.span, ident, params, ret_ty);

        let body = FnBody {
            id: new_fn_id,
            inner_scope_id: new_scope_id,
            kind: FnBodyKind::Stmt(Box::new(self.lower_begin_stmt(
                new_fn_id,
                new_scope_id,
                &stmt.ast.body,
            ))),
        };
        self.register_fnbody(scope_id, new_fn_id, body);

        new_fn_id
    }

    fn lower_if_stmt(&mut self, fn_id: FnId, scope_id: ScopeId, stmt: &Ast<AstIfStmt>) -> IfStmt {
        IfStmt {
            span: stmt.span,
            cond: Box::new(self.lower_bool_expr(fn_id, scope_id, &stmt.ast.cond)),
            then: Box::new(self.lower_stmt(fn_id, scope_id, &stmt.ast.then)),
            otherwise: Box::new(self.lower_stmt(fn_id, scope_id, &stmt.ast.otherwise)),
        }
    }

    fn lower_while_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstWhileStmt>,
    ) -> WhileStmt {
        WhileStmt {
            span: stmt.span,
            cond: Box::new(self.lower_bool_expr(fn_id, scope_id, &stmt.ast.cond)),
            body: Box::new(self.lower_stmt(fn_id, scope_id, &stmt.ast.body)),
        }
    }

    fn lower_begin_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstBeginStmt>,
    ) -> BeginStmt {
        let mut stmts = vec![];
        let mut curr = &stmt.ast.list.ast;
        while let AstStmtList::Nonempty { stmt, next } = curr {
            stmts.push(self.lower_stmt(fn_id, scope_id, stmt));
            curr = &next.ast;
        }

        BeginStmt {
            span: stmt.span,
            stmts,
        }
    }

    fn lower_assg_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstAssgStmt>,
    ) -> AssgStmt {
        AssgStmt {
            span: stmt.span,
            var: Box::new(self.lower_var(fn_id, scope_id, &stmt.ast.var, true)),
            expr: Box::new(self.lower_arith_expr(fn_id, scope_id, &stmt.ast.expr)),
        }
    }

    fn lower_dump_stmt(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        stmt: &Ast<AstDumpStmt>,
    ) -> DumpStmt {
        DumpStmt {
            span: stmt.span,
            var: Box::new(self.lower_var(fn_id, scope_id, &stmt.ast.var, false)),
        }
    }

    fn lower_bool_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstBoolExpr>,
    ) -> BoolExpr {
        BoolExpr {
            span: expr.span,
            op: Box::new(self.lower_compare_op(fn_id, scope_id, &expr.ast.op)),
            lhs: Box::new(self.lower_arith_expr(fn_id, scope_id, &expr.ast.lhs)),
            rhs: Box::new(self.lower_arith_expr(fn_id, scope_id, &expr.ast.rhs)),
        }
    }

    fn lower_compare_op(
        &mut self,
        _fn_id: FnId,
        _scope_id: ScopeId,
        op: &Ast<AstCompareOp>,
    ) -> CompareOp {
        let kind = match op.ast {
            AstCompareOp::Lt => CompareOpKind::Lt,
            AstCompareOp::Gt => CompareOpKind::Gt,
            AstCompareOp::Le => CompareOpKind::Le,
            AstCompareOp::Ge => CompareOpKind::Ge,
            AstCompareOp::Eq => CompareOpKind::Eq,
            AstCompareOp::Ne => CompareOpKind::Ne,
        };

        CompareOp {
            span: op.span,
            kind,
        }
    }

    fn lower_arith_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstArithExpr>,
    ) -> ArithExpr {
        match &expr.ast {
            AstArithExpr::MulExpr(e) => self.lower_mul_expr(fn_id, scope_id, e),
            AstArithExpr::Add(l, r) | AstArithExpr::Sub(l, r) => {
                let op = match &expr.ast {
                    AstArithExpr::MulExpr(_) => unreachable!(),
                    AstArithExpr::Add(_, _) => BinOp::Add,
                    AstArithExpr::Sub(_, _) => BinOp::Sub,
                };
                let kind = ArithExprKind::BinOp(
                    op,
                    Box::new(self.lower_arith_expr(fn_id, scope_id, l)),
                    Box::new(self.lower_mul_expr(fn_id, scope_id, r)),
                );

                ArithExpr {
                    span: expr.span,
                    kind,
                    ty: Ty {
                        span: expr.span,
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                    },
                }
            }
        }
    }

    fn lower_mul_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstMulExpr>,
    ) -> ArithExpr {
        match &expr.ast {
            AstMulExpr::UnaryExpr(e) => self.lower_unary_expr(fn_id, scope_id, e),
            AstMulExpr::Mul(l, r) | AstMulExpr::Div(l, r) => {
                let op = match &expr.ast {
                    AstMulExpr::UnaryExpr(_) => unreachable!(),
                    AstMulExpr::Mul(_, _) => BinOp::Mul,
                    AstMulExpr::Div(_, _) => BinOp::Div,
                };
                let kind = ArithExprKind::BinOp(
                    op,
                    Box::new(self.lower_mul_expr(fn_id, scope_id, l)),
                    Box::new(self.lower_unary_expr(fn_id, scope_id, r)),
                );

                ArithExpr {
                    span: expr.span,
                    kind,
                    ty: Ty {
                        span: expr.span,
                        res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                    },
                }
            }
        }
    }

    fn lower_unary_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstUnaryExpr>,
    ) -> ArithExpr {
        match &expr.ast {
            AstUnaryExpr::PrimaryExpr(e) => ArithExpr {
                span: expr.span,
                ty: Ty {
                    span: expr.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                },
                kind: ArithExprKind::Primary(Box::new(
                    self.lower_primary_expr(fn_id, scope_id, &e),
                )),
            },
            AstUnaryExpr::Neg(e) => ArithExpr {
                span: expr.span,
                ty: Ty {
                    span: expr.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                },
                kind: ArithExprKind::UnaryOp(
                    UnaryOp::Neg,
                    Box::new(self.lower_unary_expr(fn_id, scope_id, &e)),
                ),
            },
        }
    }

    fn lower_primary_expr(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        expr: &Ast<AstPrimaryExpr>,
    ) -> PrimaryExpr {
        PrimaryExpr {
            span: expr.span,
            ty: Ty {
                span: expr.span,
                res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
            },
            kind: match &expr.ast {
                AstPrimaryExpr::Var(var) => {
                    PrimaryExprKind::Var(Box::new(self.lower_var(fn_id, scope_id, var, false)))
                }
                AstPrimaryExpr::Const(cst) => {
                    PrimaryExprKind::Const(Box::new(self.lower_const(fn_id, scope_id, cst)))
                }
                AstPrimaryExpr::FnCall(call) => {
                    PrimaryExprKind::FnCall(Box::new(self.lower_fncall(fn_id, scope_id, call)))
                }
                AstPrimaryExpr::Paren(expr) => {
                    PrimaryExprKind::Paren(Box::new(self.lower_arith_expr(fn_id, scope_id, expr)))
                }
            },
        }
    }

    fn lower_fncall(&mut self, fn_id: FnId, scope_id: ScopeId, call: &Ast<AstFnCall>) -> FnCall {
        let mut args = vec![];
        let mut curr_arg = &*call.ast.args;
        while let Ast {
            ast: AstArgumentList::Nonempty { expr, next },
            ..
        } = curr_arg
        {
            args.push(self.lower_arith_expr(fn_id, scope_id, expr));
            curr_arg = &next;
        }

        FnCall {
            span: call.span,
            span_name: call.ast.ident.span,
            res: RefCell::new(ResolveStatus::Unresolved(self.lower_ident(
                fn_id,
                scope_id,
                &call.ast.ident,
            ))),
            args,
        }
    }

    fn lower_const(&mut self, _fn_id: FnId, _scope_id: ScopeId, cst: &Ast<AstConst>) -> Const {
        let (ty, value) = match &cst.ast {
            AstConst::Int(v) => (
                Ty {
                    span: cst.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Int))),
                },
                Value::Int(*v),
            ),
            AstConst::Float(v) => (
                Ty {
                    span: cst.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Revealed(
                        TyKind::Float,
                    ))),
                },
                Value::Float(*v),
            ),
        };
        Const {
            span: cst.span,
            ty,
            value,
        }
    }

    fn lower_var(
        &mut self,
        fn_id: FnId,
        scope_id: ScopeId,
        var: &Ast<AstVar>,
        try_register: bool,
    ) -> VarRef {
        if try_register {
            // try registering new variable
            let name = self.lower_ident(fn_id, scope_id, var.ast.ident());
            self.try_register_var(
                scope_id,
                name,
                Ty {
                    span: var.span,
                    res: RefCell::new(ResolveStatus::Resolved(TypeckStatus::Infer)),
                },
            );
        }

        VarRef {
            span: var.span,
            res: RefCell::new(ResolveStatus::Unresolved(self.lower_ident(
                fn_id,
                scope_id,
                var.ast.ident(),
            ))),
        }
    }

    fn lower_ty(&mut self, fn_id: FnId, scope_id: ScopeId, ty: &Ast<AstTy>) -> Ty {
        Ty {
            span: ty.span,
            res: RefCell::new(ResolveStatus::Unresolved(self.lower_ident(
                fn_id,
                scope_id,
                ty.ast.ident(),
            ))),
        }
    }

    fn lower_ident(&mut self, _fn_id: FnId, _scope_id: ScopeId, ident: &Ast<AstIdent>) -> Ident {
        Ident {
            span: ident.span,
            ident: ident.ast.ident().to_owned(),
        }
    }
}
