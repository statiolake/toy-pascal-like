use crate::hir::*;
use crate::thir::*;
use itertools::izip;
use itertools::Itertools as _;
use std::collections::BTreeMap;

pub fn run(program: &ThirProgram) -> BTreeMap<String, Value> {
    let mut state = State::prepare_start(program);
    state.run();

    let scope = program.scope(state.scope_id);
    state
        .vars
        .iter()
        .filter_map(|(var_id, value)| {
            let name = scope.vars[var_id].name.ident.clone();
            value.clone().map(|value| (name, value))
        })
        .collect()
}

pub struct State<'thir> {
    prog: &'thir ThirProgram,
    fn_id: FnId,
    scope_id: ScopeId,
    vars: BTreeMap<VarId, Option<Value>>,
}

impl<'thir> State<'thir> {
    pub fn new_for(prog: &'thir ThirProgram, fn_id: FnId) -> Self {
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

    pub fn prepare_start(program: &'thir ThirProgram) -> Self {
        Self::new_for(program, program.start_fn_id)
    }
}

macro_rules! apply_op {
    (unary $op:tt, $expr:expr) => {
        match ($expr) {
            Value::Int(expr) => Value::Int($op expr),
            Value::Float(expr) => Value::Float($op expr),
            _ => unreachable!(),
        }
    };
    (compare $op:tt, $lhs:expr, $rhs:expr) => {
        #[allow(clippy::float_cmp)]
        match ($lhs, $rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Bool(l $op r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l $op r),
            _ => unreachable!(),
        }
    };
    (equal $op:tt, $lhs:expr, $rhs:expr) => {
        #[allow(clippy::float_cmp)]
        match ($lhs, $rhs) {
            (Value::Void, Value::Void) => Value::Bool(() $op ()),
            (Value::Int(l), Value::Int(r)) => Value::Bool(l $op r),
            (Value::Float(l), Value::Float(r)) => Value::Bool(l $op r),
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(l $op r),
            _ => unreachable!(),
        }
    };
    (arith $op:tt, $lhs:expr, $rhs:expr) => {
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
            &ThirFnBodyKind::Stmt(stmt_id) => {
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

                let stmt = self.prog.stmt(stmt_id);
                self.run_stmt(stmt);
                self.vars[&decl.ret_var].as_ref().unwrap().clone()
            }
            ThirFnBodyKind::Builtin(dynfn) => dynfn(args),
        }
    }

    fn run_stmt(&mut self, stmt: &ThirStmt) {
        match &stmt.kind {
            ThirStmtKind::FnDef(_) => (),
            ThirStmtKind::If(stmt) => self.run_if_stmt(stmt),
            ThirStmtKind::While(stmt) => self.run_while_stmt(stmt),
            ThirStmtKind::Begin(stmt) => self.run_begin_stmt(stmt),
            ThirStmtKind::Assg(stmt) => self.run_assg_stmt(stmt),
            ThirStmtKind::Dump(stmt) => self.run_dump_stmt(stmt),
        }
    }

    fn run_if_stmt(&mut self, stmt: &ThirIfStmt) {
        let cond = self.prog.expr(stmt.cond_id);
        if self.eval_arith_expr(cond).unwrap_bool() {
            let then = self.prog.stmt(stmt.then_id);
            self.run_stmt(then);
        } else if let Some(otherwise_id) = stmt.otherwise_id {
            let otherwise = self.prog.stmt(otherwise_id);
            self.run_stmt(otherwise);
        }
    }

    fn run_while_stmt(&mut self, stmt: &ThirWhileStmt) {
        let cond = self.prog.expr(stmt.cond_id);
        let body = self.prog.stmt(stmt.body_id);
        while self.eval_arith_expr(cond).unwrap_bool() {
            self.run_stmt(body);
        }
    }

    fn run_begin_stmt(&mut self, stmt: &ThirBeginStmt) {
        for &stmt_id in &stmt.stmt_ids {
            let stmt = self.prog.stmt(stmt_id);
            self.run_stmt(stmt);
        }
    }

    fn run_assg_stmt(&mut self, stmt: &ThirAssgStmt) {
        let expr = self.prog.expr(stmt.expr_id);
        *self.vars.get_mut(&stmt.var.res).unwrap() = Some(self.eval_arith_expr(expr));
    }

    fn run_dump_stmt(&mut self, stmt: &ThirDumpStmt) {
        let scope = &self.prog.scope(self.scope_id);
        let var = &scope.vars[&stmt.var.res];
        let value = &self.vars[&stmt.var.res];
        println!("{} = {}", var.name.ident, value.as_ref().unwrap());
    }

    fn eval_arith_expr(&self, stmt: &ThirArithExpr) -> Value {
        match &stmt.kind {
            ThirArithExprKind::UnaryOp(op, expr_id) => match op {
                UnaryOp::Neg => {
                    let expr = self.prog.expr(*expr_id);
                    apply_op!(unary -, self.eval_arith_expr(expr))
                }
            },
            ThirArithExprKind::BinOp(op, lhs_id, rhs_id) => {
                let lhs = self.prog.expr(*lhs_id);
                let rhs = self.prog.expr(*rhs_id);
                match op {
                    BinOp::Add => {
                        apply_op!(arith+, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Sub => {
                        apply_op!(arith -, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Mul => {
                        apply_op!(arith *, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Div => {
                        apply_op!(arith /, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Lt => {
                        apply_op!(compare <, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Gt => {
                        apply_op!(compare >, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Le => {
                        apply_op!(compare <=, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Ge => {
                        apply_op!(compare >=, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Eq => {
                        apply_op!(equal ==, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                    BinOp::Ne => {
                        apply_op!(equal !=, self.eval_arith_expr(lhs), self.eval_arith_expr(rhs))
                    }
                }
            }
            ThirArithExprKind::Var(var) => self.eval_var(&*var),
            ThirArithExprKind::Const(cst) => self.eval_cst(&*cst),
            ThirArithExprKind::FnCall(fncall) => self.eval_fncall(&*fncall),
            ThirArithExprKind::Paren(expr_id) => {
                let expr = self.prog.expr(*expr_id);
                self.eval_arith_expr(&*expr)
            }
        }
    }

    fn eval_var(&self, var: &ThirVarRef) -> Value {
        self.vars[&var.res].as_ref().unwrap().clone()
    }

    fn eval_cst(&self, cst: &ThirConst) -> Value {
        cst.value.clone()
    }

    fn eval_fncall(&self, fncall: &ThirFnCall) -> Value {
        let mut state = State::new_for(self.prog, fncall.res);
        let args = fncall
            .arg_ids
            .iter()
            .map(|&arg_id| {
                let arg = self.prog.expr(arg_id);
                self.eval_arith_expr(arg)
            })
            .collect_vec();
        state.run_fn(args)
    }
}
