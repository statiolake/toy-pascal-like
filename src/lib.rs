pub mod span;

pub mod lexer;

pub mod parser;

pub mod ast;

pub mod lowerer;

pub mod hir;
pub mod hir_visit;

pub mod resolver;

pub mod rhir;
pub mod rhir_visit;

pub mod typeck;

pub mod thir;

pub mod builtins;

pub mod thir_interp;

pub mod error;

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::builtins::populate_builtins;
    use crate::hir::Value;
    use crate::lexer::tokenize;
    use crate::lowerer::lower_ast;
    use crate::parser::parse;
    use crate::resolver::resolve_hir;
    use crate::thir_interp::run;
    use crate::typeck::check_rhir;

    fn run_source(source: &str) -> BTreeMap<String, Value> {
        let tokens = tokenize(source);
        let ast = parse(&tokens).expect("it should parse");
        let hir = lower_ast(&ast, populate_builtins());
        let rhir = resolve_hir(hir).expect("it should resolved");
        let thir = check_rhir(rhir).expect("it should pass typeck");

        run(&thir)
    }

    #[test]
    fn fib() {
        let source = r#"
begin
    a_0 := 0;
    a_1 := 1;
    i := 0;
    while i < 000020 do begin
        dump a_0;
        tmp := (a_0 + a_1);
        a_0 := a_1;
        a_1 := tmp;
        i := (i + 1)
    end
end
"#;

        let vars = run_source(source);
        assert_eq!(vars["a_0"].unwrap_int(), 6765);
        assert_eq!(vars["i"].unwrap_int(), 20);
    }

    #[test]
    fn recursive_fncall() {
        let source = r#"
begin
    function Fib(n: int): int; begin
        if n = 0 then
            Fib := 0
        else if n = 1 then
            Fib := 1
        else
            Fib := (Fib((n - 1)) + Fib((n - 2)))
    end;

    x := Fib(10);
    dump x
end
"#;

        let vars = run_source(source);
        assert_eq!(vars["x"].unwrap_int(), 55);
    }

    #[test]
    fn natural_arith() {
        let source = r#"begin x := (1 + 2) * 3 - (4 + 5) + (-6 * (7 + 8)) / -9 + -10 end"#;

        let vars = run_source(source);
        assert_eq!(vars["x"].unwrap_int(), 0);
    }

    #[test]
    fn float_arith() {
        let source = r#"
begin
    x := 1.0;
    i := 0;
    sum := 0.0;
    while i < 100 do begin
        sum := sum + x;
        x := x / 2.0;
        i := i + 1
    end
end
"#;

        let vars = run_source(source);
        assert!((vars["sum"].unwrap_float() - 2.0).abs() < 1e-8);
    }
}
