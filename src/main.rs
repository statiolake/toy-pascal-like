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
    println!("{:?}", tokens);
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
