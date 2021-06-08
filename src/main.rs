use std::{env, fs};

use pascal_like::interpreter::run;
use pascal_like::lexer::tokenize;
use pascal_like::parser::Parser;

fn main() {
    let filename = match env::args().nth(1) {
        Some(f) => f,
        None => panic!("please specifiy the file name"),
    };
    let source = fs::read_to_string(filename).expect("failed to read source file");

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
