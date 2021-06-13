use crate::hir::{CompareOpKind, FnId, ScopeId, TyKind, Value, VarId};
use crate::thir::*;
use itertools::izip;
use itertools::Itertools as _;
use std::collections::BTreeMap;

pub fn run(program: &Program) {
    let mut state = State::prepare_start(program);
    state.run();
}

pub struct State<'thir> {
    prog: &'thir Program,
    fn_id: FnId,
    scope_id: ScopeId,
    vars: BTreeMap<VarId, Option<Value>>,
}

impl<'thir> State<'thir> {
    pub fn new_for(prog: &'thir Program, fn_id: FnId) -> Self {
        // prepare variable tables
        let scope_id = prog.fnbody(fn_id).inner_scope_id;
        let scope = prog.scope(scope_id);
        let vars = scope.vars.keys().map(|&var_id| (var_id, None)).collect();

        Self {
            prog,
            fn_id,
            scope_id,
            vars,
        }
    }

    pub fn prepare_start(program: &'thir Program) -> Self {
        Self::new_for(program, program.start_fn_id)
    }
}

macro_rules! apply_op {
    ($op:tt, $expr:expr) => {
        match ($expr) {
            Value::Int(expr) => Value::Int($op expr),
            Value::Float(expr) => Value::Float($op expr),
            _ => unreachable!(),
        }
    };
    (bool $op:tt, $lhs:expr, $rhs:expr) => {
        match ($lhs, $rhs) {
            (Value::Int(l), Value::Int(r)) => l $op r,
            (Value::Float(l), Value::Float(r)) => l $op r,
            _ => unreachable!(),
        }
    };
    ($op:tt, $lhs:expr, $rhs:expr) => {
        match ($lhs, $rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l $op r),
            (Value::Float(l), Value::Float(r)) => Value::Float(l $op r),
            _ => unreachable!(),
        }
    };
}

impl State<'_> {
    fn run(&mut self) {
        self.run_fn(vec![]);
    }

    fn run_fn(&mut self, args: Vec<Value>) -> Value {
        let decl = self.prog.fndecl(self.fn_id);
        let body = self.prog.fnbody(self.fn_id);
        match &body.kind {
            FnBodyKind::Stmt(stmt) => {
                // Set args to the local variable
                for (param, arg) in izip!(&decl.params, args) {
                    if let Some(res) = param.res {
                        *self.vars.get_mut(&res).unwrap() = Some(arg);
                    }
                }

                // If the return type is void: the return value can be implicit
                if decl.ret_ty.res == TyKind::Void {
                    *self.vars.get_mut(&decl.ret_var).unwrap() = Some(Value::Void);
                }

                self.run_begin_stmt(&*stmt);
                self.vars[&decl.ret_var].as_ref().unwrap().clone()
            }
            FnBodyKind::Builtin(dynfn) => dynfn(args),
        }
    }

    fn run_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::FnDef(_) => (),
            StmtKind::If(stmt) => self.run_if_stmt(stmt),
            StmtKind::While(stmt) => self.run_while_stmt(stmt),
            StmtKind::Begin(stmt) => self.run_begin_stmt(stmt),
            StmtKind::Assg(stmt) => self.run_assg_stmt(stmt),
            StmtKind::Dump(stmt) => self.run_dump_stmt(stmt),
        }
    }

    fn run_if_stmt(&mut self, stmt: &IfStmt) {
        if self.eval_bool_expr(&*stmt.cond) {
            self.run_stmt(&*stmt.then);
        } else {
            self.run_stmt(&*stmt.otherwise);
        }
    }

    fn run_while_stmt(&mut self, stmt: &WhileStmt) {
        while self.eval_bool_expr(&*stmt.cond) {
            self.run_stmt(&*stmt.body);
        }
    }

    fn run_begin_stmt(&mut self, stmt: &BeginStmt) {
        for stmt in &stmt.stmts {
            self.run_stmt(stmt);
        }
    }

    fn run_assg_stmt(&mut self, stmt: &AssgStmt) {
        *self.vars.get_mut(&stmt.var.res).unwrap() = Some(self.eval_arith_expr(&*stmt.expr));
    }

    fn run_dump_stmt(&mut self, stmt: &DumpStmt) {
        let scope = &self.prog.scope(self.scope_id);
        let var = &scope.vars[&stmt.var.res];
        let value = &self.vars[&stmt.var.res];
        println!("{} = {}", var.name.ident, value.as_ref().unwrap());
    }

    fn eval_bool_expr(&self, stmt: &BoolExpr) -> bool {
        let lhs = self.eval_arith_expr(&*stmt.lhs);
        let rhs = self.eval_arith_expr(&*stmt.rhs);
        match &stmt.op.kind {
            CompareOpKind::Lt => apply_op!(bool <, lhs, rhs),
            CompareOpKind::Gt => apply_op!(bool >, lhs, rhs),
            CompareOpKind::Le => apply_op!(bool <=, lhs, rhs),
            CompareOpKind::Ge => apply_op!(bool >=, lhs, rhs),
            #[allow(clippy::float_cmp)]
            CompareOpKind::Eq => apply_op!(bool ==, lhs, rhs),
            #[allow(clippy::float_cmp)]
            CompareOpKind::Ne => apply_op!(bool !=, lhs, rhs),
        }
    }

    fn eval_arith_expr(&self, stmt: &ArithExpr) -> Value {
        match &stmt.kind {
            ArithExprKind::Primary(e) => self.eval_primary_expr(e),
            ArithExprKind::UnaryOp(op, e) => match op {
                crate::hir::UnaryOp::Neg => apply_op!(-, self.eval_arith_expr(e)),
            },
            ArithExprKind::BinOp(op, l, r) => match op {
                crate::hir::BinOp::Add => {
                    apply_op!(+, self.eval_arith_expr(l), self.eval_arith_expr(r))
                }
                crate::hir::BinOp::Sub => {
                    apply_op!(-, self.eval_arith_expr(l), self.eval_arith_expr(r))
                }
                crate::hir::BinOp::Mul => {
                    apply_op!(*, self.eval_arith_expr(l), self.eval_arith_expr(r))
                }
                crate::hir::BinOp::Div => {
                    apply_op!(/, self.eval_arith_expr(l), self.eval_arith_expr(r))
                }
            },
        }
    }

    fn eval_primary_expr(&self, stmt: &PrimaryExpr) -> Value {
        match &stmt.kind {
            PrimaryExprKind::Var(var) => self.eval_var(&*var),
            PrimaryExprKind::Const(cst) => self.eval_cst(&*cst),
            PrimaryExprKind::FnCall(fncall) => self.eval_fncall(&*fncall),
            PrimaryExprKind::Paren(expr) => self.eval_arith_expr(&*expr),
        }
    }

    fn eval_var(&self, var: &VarRef) -> Value {
        self.vars[&var.res].as_ref().unwrap().clone()
    }

    fn eval_cst(&self, cst: &Const) -> Value {
        cst.value.clone()
    }

    fn eval_fncall(&self, fncall: &FnCall) -> Value {
        let mut state = State::new_for(self.prog, fncall.res);
        let args = fncall
            .args
            .iter()
            .map(|arg| self.eval_arith_expr(arg))
            .collect_vec();
        state.run_fn(args)
    }
}
