use once_cell::sync::Lazy;
use pascal_like::builtins;
use pascal_like::error::{print_parser_error, print_resolver_error, print_typeck_error};
use pascal_like::lexer::tokenize;
use pascal_like::lowerer::lower_ast;
use pascal_like::parser::parse;
use pascal_like::resolver::resolve_hir;
use pascal_like::thir_interp;
use pascal_like::typeck::check_rhir;
use std::sync::Mutex;
use std::{env, fs};
use termcolor::{ColorChoice, StandardStream};

static STDERR: Lazy<Mutex<StandardStream>> =
    Lazy::new(|| Mutex::new(StandardStream::stderr(ColorChoice::Auto)));

fn main() {
    let filename = match env::args().nth(1) {
        Some(f) => f,
        None => panic!("please specifiy the file name"),
    };
    let source = fs::read_to_string(&filename).expect("failed to read source file");

    let tokens = tokenize(&source);

    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            print_parser_error(&mut *STDERR.lock().unwrap(), &filename, &source, &err);
            return;
        }
    };

    println!("--- ast ---");
    println!("{:#?}", ast);
    println!();

    let builtins = builtins::populate_builtins();
    let hir = lower_ast(&ast, builtins);
    println!("--- hir ---");
    println!("{:#?}", hir);
    println!();

    let rhir = match resolve_hir(hir) {
        Ok(resolved) => resolved,
        Err(errors) => {
            for err in errors {
                print_resolver_error(&mut *STDERR.lock().unwrap(), &filename, &source, &err);
            }
            return;
        }
    };
    println!("--- resolved hir ---");
    println!("{:#?}", rhir);
    println!();

    let thir = match check_rhir(rhir) {
        Ok(checked) => checked,
        Err(errors) => {
            for err in errors {
                print_typeck_error(&mut *STDERR.lock().unwrap(), &filename, &source, &err);
            }
            return;
        }
    };
    println!("--- typed hir ---");
    println!("{:#?}", thir);
    println!();

    println!("--- run (THIR) ---");
    thir_interp::run(&thir);
}
