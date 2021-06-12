use crate::hir::FnBody;
use crate::hir::TyKind;

pub struct Builtin {
    pub name: String,
    pub params: Vec<(String, TyKind)>,
    pub ret_ty: TyKind,
    pub body: FnBody,
}

pub fn populate_builtins() -> Vec<Builtin> {
    // TODO: add builtin functions here
    vec![]
}
