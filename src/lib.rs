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
                "a_0".to_string() => 6765,
                "a_1".to_string() => 10946,
                "i".to_string() => 20,
                "tmp".to_string() => 10946,
            }
        };
    }
}
