pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;

#[derive(thiserror::Error, Debug)]
pub enum Error<'a> {
    #[error("parse error: {0}")]
    ParserError(parser::ParserError<'a>),

    #[error("runtime error: {0}")]
    InterpreterError(interpreter::InterpreterError),
}

impl<'a> From<parser::ParserError<'a>> for Error<'a> {
    fn from(err: parser::ParserError<'a>) -> Self {
        Self::ParserError(err)
    }
}

impl<'a> From<interpreter::InterpreterError> for Error<'a> {
    fn from(err: interpreter::InterpreterError) -> Self {
        Self::InterpreterError(err)
    }
}

pub type Result<'a, T, E = Error<'a>> = std::result::Result<T, E>;

#[cfg(test)]
mod tests {
    use super::Result;
    use crate::ast::AstStmt;
    use crate::interpreter::run;
    use crate::interpreter::State;
    use crate::lexer::tokenize;
    use crate::parser::Parser;
    use maplit::hashmap;

    fn parse(source: &str) -> Result<AstStmt> {
        let tokens = tokenize(source);
        Parser::new(&tokens).parse_stmt().map_err(Into::into)
    }

    fn run_source(source: &str) -> Result<State> {
        let ast = parse(source).unwrap_or_else(|e| panic!("{}", e));
        run(&ast).map_err(Into::into)
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
        let state = run_source(source).expect("it should run");
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
        let state = run_source(source).expect("it shoudl run");
        assert!((10..=20).contains(&state.variables()["dice"]));
    }
}
