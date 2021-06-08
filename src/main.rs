use std::io::prelude::*;
use std::io::stdin;

use pascal_like::interpreter::run;
use pascal_like::lexer::tokenize;
use pascal_like::parser::Parser;

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
