pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests {
    use crate::interpreter::run;
    use crate::interpreter::State;
    use crate::lexer::tokenize;
    use crate::parser::Parser;
    use maplit::hashmap;

    fn run_source(source: &str) -> State {
        let tokens = tokenize(&source);
        let ast = Parser::new(&tokens).parse_stmt();
        run(&ast)
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
        let state = run_source(source);
        assert_eq! {
            state.variables(),
            &hashmap! {
                "a_0".to_string() => 6765,
                "a_1".to_string() => 10946,
                "i".to_string() => 20,
                "tmp".to_string() => 10946,
            }
        };
    }

    #[test]
    fn func_random_int() {
        let source = r#"
begin
    min := 10;
    max := 20;
    dice := RandomInt(min, max);
    dump dice
end
"#;
        let state = run_source(source);
        assert!((10..=20).contains(&state.variables()["dice"]));
    }
}
