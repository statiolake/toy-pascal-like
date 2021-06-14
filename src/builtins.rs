use crate::hir::{HirFnBodyKind, TyKind};

pub struct Builtin {
    pub name: String,
    pub params: Vec<(String, TyKind)>,
    pub ret_ty: TyKind,
    pub body_kind: HirFnBodyKind,
}

#[macro_export]
#[doc(hidden)]
macro_rules! panic_param_ty_mismatch {
    ($param:ident, $param_ty:tt, $arg:expr) => {
        panic!(
            "param `{}` expects a value of type `{}` but passed arg was `{:?}`",
            stringify!($param),
            stringify!($param_ty),
            $arg
        )
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! unwrap_param {
    ($param:ident, void, $arg:expr) => {{
        match $arg {
            $crate::hir::Value::Void => (),
            _ => $crate::panic_param_ty_mismatch!($param, void, $arg),
        }
    }};
    ($param:ident, int, $arg:expr) => {{
        match $arg {
            $crate::hir::Value::Int(value) => value,
            _ => $crate::panic_param_ty_mismatch!($param, int, $arg),
        }
    }};
    ($param:ident, float, $arg:expr) => {{
        match $arg {
            $crate::hir::Value::Float(value) => value,
            _ => $crate::panic_param_ty_mismatch!($param, float, $arg),
        }
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! wrap_ret {
    ($ret:expr, void) => {{
        // to check type
        let _: () = $ret;
        $crate::hir::Value::Void
    }};
    ($ret:expr, int) => {
        $crate::hir::Value::Int($ret)
    };
    ($ret:expr, float) => {
        $crate::hir::Value::Float($ret)
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! name_to_ty_kind {
    (void) => {
        $crate::hir::TyKind::Void
    };
    (int) => {
        $crate::hir::TyKind::Int
    };
    (float) => {
        $crate::hir::TyKind::Float
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! make_builtin {
    (fn $name:ident($($param:ident: $param_ty:tt),*) -> $ret_ty:tt $body:block) => {{
        let name = stringify!($name).to_owned();
        let params = vec![
            $(
                (stringify!($param).to_owned(), $crate::name_to_ty_kind!($param_ty)),
            )*
        ];
        let ret_ty = $crate::name_to_ty_kind!($ret_ty);
        let clos = |args: ::std::vec::Vec<$crate::hir::Value>| -> $crate::hir::Value {
            #[allow(unused_mut, unused_variables)]
            let mut args = args.into_iter();
            $(
                let $param = {
                    let arg = args
                        .next()
                        .expect("internal error: unexpected end of args in builtin");
                    $crate::unwrap_param!($param, $param_ty, arg)
                };
            )*
            let ret = $body;

            $crate::wrap_ret!(ret, $ret_ty)
        };
        let body_kind = $crate::hir::HirFnBodyKind::Builtin (Box::new(clos));

        Builtin {
            name,
            params,
            ret_ty,
            body_kind,
        }
    }};
}

pub fn populate_builtins() -> Vec<Builtin> {
    let mut builtins = Vec::new();

    builtins.extend(populate_casts());
    builtins.extend(populate_rands());

    builtins
}

fn populate_casts() -> Vec<Builtin> {
    let floattoint = make_builtin! {
        fn floattoint(x: float) -> int {
            x as _
        }
    };

    let inttofloat = make_builtin! {
        fn inttofloat(x: int) -> float {
            x as _
        }
    };

    vec![floattoint, inttofloat]
}

fn populate_rands() -> Vec<Builtin> {
    use rand::prelude::*;

    let random_int = make_builtin! {
        fn RandomInt(low: int, high: int) -> int {
            thread_rng().gen_range(low..high)
        }
    };

    let random_float = make_builtin! {
        fn RandomFloat() -> float {
            thread_rng().gen()
        }
    };

    vec![random_int, random_float]
}
