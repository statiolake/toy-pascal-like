pub mod ast;
pub mod interp;
pub mod lexer;
pub mod parser;
pub mod span;

#[derive(thiserror::Error, Debug)]
pub enum Error<'a> {
    #[error("parse error: {0}")]
    ParserError(parser::ParserError<'a>),

    #[error("runtime error: {0}")]
    InterpError(interp::InterpError),
}

impl<'a> From<parser::ParserError<'a>> for Error<'a> {
    fn from(err: parser::ParserError<'a>) -> Self {
        Self::ParserError(err)
    }
}

impl<'a> From<interp::InterpError> for Error<'a> {
    fn from(err: interp::InterpError) -> Self {
        Self::InterpError(err)
    }
}

pub type Result<'a, T, E = Error<'a>> = std::result::Result<T, E>;

#[cfg(test)]
mod tests {
    use crate::interp::run;
    use crate::lexer::tokenize;
    use crate::parser::parse;

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
        let tokens = tokenize(source);
        let ast = parse(&tokens).expect("it should parse");
        let state = run(&ast).expect("it should run");
        assert_eq!(state.variables()["a_0"].unwrap_int(), 6765);
        assert_eq!(state.variables()["i"].unwrap_int(), 20);
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
        let tokens = tokenize(source);
        let ast = parse(&tokens).expect("it should parse");
        let state = run(&ast).expect("it should run");
        assert!((10..=20).contains(&state.variables()["dice"].unwrap_int()));
    }

    #[test]
    fn recursive_fncall() {
        let source = r#"
begin
    function Fib(n: int): int; begin
        if n == 0 then
            Fib := 0
        else if n == 1 then
            Fib := 1
        else begin
            Fib := (Fib((n - 1)) + Fib((n - 2)))
        end
    end;

    x := Fib(10);
    dump x
end
"#;
        let tokens = tokenize(source);
        let ast = parse(&tokens).expect("it should parse");
        let state = run(&ast).expect("it should run");
        assert_eq!(state.variables()["x"].unwrap_int(), 55);
    }

    #[test]
    fn natural_arith() {
        let source = r#"x := (1 + 2) * 3 - (4 + 5) + (-6 * (7 + 8)) / -9 + -10"#;
        let tokens = tokenize(source);
        let ast = parse(&tokens).expect("it should parse");
        let state = run(&ast).expect("it should run");
        assert_eq!(state.variables()["x"].unwrap_int(), 0);
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
        let tokens = tokenize(source);
        let ast = parse(&tokens).expect("it should parse");
        let state = run(&ast).expect("it should run");
        assert!((state.variables()["sum"].unwrap_float() - 2.0).abs() < 1e-8);
    }
}
