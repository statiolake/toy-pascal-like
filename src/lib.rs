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
        let source = include_str!("../fib.pas");
        let state = run_source(source);
        assert_eq! {
            state.variables(),
            &hashmap! {
                "n".to_string() => 9,
                "t".to_string() => 55,
                "s".to_string() => 55,
                "f".to_string() => 34,
            }
        };
    }
}
