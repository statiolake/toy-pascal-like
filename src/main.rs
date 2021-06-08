mod ast;
mod interpreter;
mod lexer;
mod parser;

use std::io::prelude::*;
use std::io::stdin;

use lexer::tokenize;
use parser::Parser;

use crate::interpreter::run;

fn main() {
    let mut source = String::new();
    stdin().read_to_string(&mut source).unwrap();

    let tokens = tokenize(&source);

    println!("--- tokens ---");
    for token in &tokens {
        println!("{:?}", token);
    }
    println!();

    let ast = Parser::new(&tokens).parse_stmt();

    println!("--- ast ---");
    println!("{}", ast);
    println!();

    println!("--- run ---");
    let state = run(&ast);
    println!();

    println!("--- variables ---");
    state.display();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::State;
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
