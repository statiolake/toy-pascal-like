use crate::hir::*;
use crate::hir_visit;
use crate::hir_visit::Visit;
use crate::span::Span;
use std::collections::BTreeSet;

#[derive(thiserror::Error, Debug)]
#[error("{span:?}: {kind}")]
pub struct ResolverError {
    pub span: Span,
    pub kind: ResolverErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum ResolverErrorKind {
    #[error("undeclared function: `{ident}`")]
    UndeclaredFunction { ident: String },

    #[error("undeclared variable: `{ident}`")]
    UndeclaredVariable { ident: String },

    #[error("variable `{ident}` is read before initialization")]
    UseBeforeInit { ident: String },

    #[error("unknown type: `{ident}`")]
    UnknownTy { ident: String },

    // When two parameters are the same name, etc.
    #[error("already defined variable: `{ident}`")]
    AlreadyDefinedVariable { ident: String },
}

impl ResolverErrorKind {
    pub fn summary(&self) -> String {
        match self {
            ResolverErrorKind::UndeclaredFunction { .. } => "unknown function name".to_string(),
            ResolverErrorKind::UndeclaredVariable { .. } => "unknown variable name".to_string(),
            ResolverErrorKind::UseBeforeInit { .. } => "used but not initialized".to_string(),
            ResolverErrorKind::UnknownTy { .. } => "unknown type".to_string(),
            ResolverErrorKind::AlreadyDefinedVariable { .. } => "already defined".to_string(),
        }
    }
}

pub type Result<T, E = ResolverError> = std::result::Result<T, E>;

pub fn resolve_progam(prog: Program) -> Result<Program, Vec<ResolverError>> {
    Resolver::from_program(prog).resolve()
}

#[derive(Debug)]
pub struct Resolver {
    prog: Program,
}

impl Resolver {
    pub fn from_program(prog: Program) -> Self {
        Self { prog }
    }

    pub fn resolve(self) -> Result<Program, Vec<ResolverError>> {
        self.resolve_all()?;
        self.validate();

        Ok(self.prog)
    }
}

impl Resolver {
    fn resolve_all(&self) -> Result<(), Vec<ResolverError>> {
        let scope_id = self.prog.fndecl(self.prog.start_fn_id).scope_id;
        let mut visitor = VisitorContext {
            prog: &self.prog,
            scope_id,
            visible_vars: BTreeSet::new(),
            errors: vec![],
        };
        visitor.visit_all(&self.prog);
        return if visitor.errors.is_empty() {
            Ok(())
        } else {
            Err(visitor.errors)
        };

        struct VisitorContext<'hir> {
            prog: &'hir Program,
            scope_id: ScopeId,
            visible_vars: BTreeSet<VarId>,
            errors: Vec<ResolverError>,
        }

        fn resolve_primitive_ty(ty: &Ty) -> Result<()> {
            let cloned = ty.res.borrow().clone();
            if let ResolveStatus::Unresolved(name) = cloned {
                match &*name.ident {
                    "int" => {
                        *ty.res.borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Int))
                    }
                    "float" => {
                        *ty.res.borrow_mut() =
                            ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Float))
                    }
                    _ => {
                        *ty.res.borrow_mut() = ResolveStatus::Err(name.clone());
                        return Err(ResolverError {
                            span: ty.span,
                            kind: ResolverErrorKind::UnknownTy { ident: name.ident },
                        });
                    }
                }
            }

            Ok(())
        }

        fn find_var(scope: &Scope, name: &Ident) -> Result<VarId> {
            if let Some(var) = scope.vars.values().find(|v| v.name.ident == name.ident) {
                Ok(var.id)
            } else {
                Err(ResolverError {
                    span: name.span,
                    kind: ResolverErrorKind::UndeclaredVariable {
                        ident: name.ident.clone(),
                    },
                })
            }
        }

        impl VisitorContext<'_> {
            fn resolve_var_ref(&mut self, var_ref: &VarRef, is_assign: bool) -> Result<()> {
                let scope = self.prog.scope(self.scope_id);

                let cloned = var_ref.res.borrow().clone();
                if let ResolveStatus::Unresolved(name) = cloned {
                    let var_id = match find_var(scope, &name) {
                        Ok(id) => id,
                        Err(e) => {
                            // set the status to error
                            *var_ref.res.borrow_mut() = ResolveStatus::Err(name);
                            return Err(e);
                        }
                    };

                    // if the variable actually found but it's not yet visible, then register it as
                    // a visible variable if this is assign statement, and otherwise it's
                    // use-before-init error.
                    if !self.visible_vars.contains(&var_id) {
                        if is_assign {
                            self.visible_vars.insert(var_id);
                        } else {
                            let ident = name.ident.clone();
                            *var_ref.res.borrow_mut() = ResolveStatus::Err(name);
                            return Err(ResolverError {
                                span: var_ref.span,
                                kind: ResolverErrorKind::UseBeforeInit { ident },
                            });
                        }
                    }

                    // Successfully resolved now.
                    *var_ref.res.borrow_mut() = ResolveStatus::Resolved(var_id);
                }

                Ok(())
            }

            fn resolve_fncall(&mut self, fncall: &FnCall) -> Result<()> {
                let cloned = fncall.res.borrow().clone();
                if let ResolveStatus::Unresolved(name) = cloned {
                    let fndecl = self
                        .prog
                        .fndecls
                        .values()
                        .find(|fndecl| fndecl.name.ident == name.ident);
                    let fndecl = match fndecl {
                        Some(fndecl) => fndecl,
                        None => {
                            *fncall.res.borrow_mut() = ResolveStatus::Err(name.clone());
                            return Err(ResolverError {
                                span: fncall.span_name,
                                kind: ResolverErrorKind::UndeclaredFunction { ident: name.ident },
                            });
                        }
                    };
                    *fncall.res.borrow_mut() = ResolveStatus::Resolved(fndecl.id);
                }

                Ok(())
            }
        }

        impl Visit for VisitorContext<'_> {
            fn visit_fnbody(&mut self, fnbody: &FnBody) {
                // set up new local variable tables
                self.visible_vars = BTreeSet::new();
                self.scope_id = fnbody.inner_scope_id;
                let scope = self.prog.scope(self.scope_id);

                // params and return variable should be registered to the local vars table.
                let fndecl = self.prog.fndecl(fnbody.id);

                // return variable: it is added only when the return type is not void
                let ret_var_id = match find_var(scope, &fndecl.name) {
                    Ok(id) => id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                if !matches!(
                    &*scope.var(ret_var_id).ty.res.borrow(),
                    ResolveStatus::Resolved(TypeckStatus::Revealed(TyKind::Void))
                ) {
                    // only add when the return value is not void
                    self.visible_vars.insert(ret_var_id);
                }

                // find params and make it visible
                for param in &fndecl.params {
                    let param_var_id = match find_var(scope, &param.name) {
                        Ok(id) => id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if !self.visible_vars.insert(param_var_id) {
                        // if the specified var_id is already visible: more than one parameters have
                        // the same name in this fndecl.
                        self.errors.push(ResolverError {
                            span: param.span,
                            kind: ResolverErrorKind::AlreadyDefinedVariable {
                                ident: param.name.ident.clone(),
                            },
                        });
                        return;
                    }
                }

                hir_visit::visit_fnbody(self, fnbody);
            }

            fn visit_ty(&mut self, ty: &Ty) {
                if let Err(err) = resolve_primitive_ty(ty) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_assg_stmt(&mut self, stmt: &AssgStmt) {
                // We can't use hir_visit::visit_assg_stmt(), since that will try to resolve var_ref
                // of asignee.
                self.visit_arith_expr(&stmt.expr);
                if let Err(err) = self.resolve_var_ref(&*stmt.var, true) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_var_ref(&mut self, var_ref: &VarRef) {
                hir_visit::visit_var_ref(self, var_ref);
                if let Err(err) = self.resolve_var_ref(&*var_ref, false) {
                    self.errors.push(err);
                    return;
                }
            }

            fn visit_fncall(&mut self, fncall: &FnCall) {
                hir_visit::visit_fncall(self, fncall);
                if let Err(err) = self.resolve_fncall(fncall) {
                    self.errors.push(err);
                    return;
                }
            }
        }
    }

    fn validate(&self) {
        let mut visitor = ValidatorVisitor;
        visitor.visit_all(&self.prog);
        struct ValidatorVisitor;
        impl Visit for ValidatorVisitor {}
    }
}
