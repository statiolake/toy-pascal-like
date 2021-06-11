use itertools::Itertools as _;
use once_cell::sync::Lazy;
use pascal_like::hir::lower_ast;
use pascal_like::interp::run;
use pascal_like::interp::InterpError;
use pascal_like::lexer::tokenize;
use pascal_like::parser::{parse, ParserError};
use pascal_like::resolver::{resolve_progam, ResolverError};
use pascal_like::span::Span;
use std::cmp::min;
use std::io::prelude::*;
use std::sync::Mutex;
use std::{env, fmt, fs};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

static STDERR: Lazy<Mutex<StandardStream>> =
    Lazy::new(|| Mutex::new(StandardStream::stderr(ColorChoice::Auto)));

static COLOR_ERROR: Lazy<ColorSpec> = Lazy::new(|| {
    let mut cs = ColorSpec::new();
    cs.set_fg(Some(Color::Red));
    cs
});
static COLOR_INFO: Lazy<ColorSpec> = Lazy::new(|| {
    let mut cs = ColorSpec::new();
    cs.set_fg(Some(Color::Cyan));
    cs
});
static COLOR_MESSAGE: Lazy<ColorSpec> = Lazy::new(|| {
    let mut cs = ColorSpec::new();
    cs.set_bold(true);
    cs
});

fn print_with_color(display: fmt::Arguments, color: &ColorSpec) {
    let mut stderr = STDERR.lock().unwrap();
    stderr.set_color(&color).unwrap();
    stderr.write_fmt(display).unwrap();
    stderr.set_color(&ColorSpec::new()).unwrap();
}

macro_rules! eprint {
    ($fg:expr => $fmt:literal $($args:tt)*) => {
        print_with_color(format_args!($fmt $($args)*), $fg);
    };
    ($($args:tt)*) => {
        eprint!(&ColorSpec::new() => $($args)*);
    };
}

macro_rules! eprintln {
    () => {
        eprintln!(&ColorSpec::new() => "");
    };
    ($fg:expr => $fmt:literal $($args:tt)*) => {
        print_with_color(format_args!(concat!($fmt, "\n") $($args)*), $fg);
    };
    ($($args:tt)*) => {
        eprintln!(&ColorSpec::new() => $($args)*);
    };
}

fn show_span(filename: &str, source: &str, span: Span, summary: &str) {
    let lines = source.lines().enumerate().collect_vec();
    let line_number_width = source.len().to_string().len() + 1;
    let line_number_indent = " ".repeat(line_number_width);
    let Span { start, end } = span;
    let window = &lines[start.line.saturating_sub(3)..min(lines.len(), end.line + 3)];

    eprint!(&*COLOR_INFO => "{}-->", line_number_indent);
    eprintln!(" {}:{}:{}", filename, start.line + 1, start.column + 1);

    eprintln!(&*COLOR_INFO => "{} |", line_number_indent);
    for (number, line) in window {
        eprint!(&*COLOR_INFO => "{:>width$} |", number + 1, width=line_number_width);
        eprintln!(" {}", line);
        if (start.line..=end.line).contains(&number) {
            let start_column = if start.line == *number {
                start.column
            } else {
                0
            };
            let end_column = if end.line == *number {
                end.column
            } else {
                line.len()
            };

            eprint!(&*COLOR_INFO => "{} |", line_number_indent);
            eprint!(" {}", " ".repeat(start_column));
            eprint!(
                &*COLOR_ERROR => "{}",
                "^".repeat(end_column - start_column),
            );

            if end.line == *number {
                eprint!(&*COLOR_ERROR => " {}", summary);
            }

            eprintln!();
        }
    }
    eprintln!(&*COLOR_INFO => "{} |", line_number_indent);
}

fn print_parser_error(filename: &str, source: &str, err: &ParserError) {
    eprint!(&*COLOR_ERROR => "error:");
    eprint!(" ");
    eprintln!(&*COLOR_MESSAGE => "{}", err.kind);
    show_span(filename, source, err.span, &err.kind.summary());
    for hint in &err.hints {
        eprint!(&*COLOR_INFO => "hint:");
        eprint!(" ");
        eprintln!("{}", hint.message);
        if let Some((span, summary)) = &hint.span {
            show_span(filename, source, *span, summary);
        }
    }
}

fn print_resolver_error(filename: &str, source: &str, err: &ResolverError) {
    eprint!(&*COLOR_ERROR => "resolver error:");
    eprint!(" ");
    eprintln!(&*COLOR_MESSAGE => "{}", err.kind);
    show_span(filename, source, err.span, &err.kind.summary());

    // for hint in &err.kind.hints() {
    //     eprint!(&*COLOR_INFO => "hint:");
    //     eprint!(" ");
    //     eprintln!("{}", hint);
    // }
}

fn print_interpreter_error(filename: &str, source: &str, err: &InterpError) {
    eprint!(&*COLOR_ERROR => "runtime error:");
    eprint!(" ");
    eprintln!(&*COLOR_MESSAGE => "{}", err.kind);
    show_span(filename, source, err.span, &err.kind.summary());

    for hint in &err.kind.hints() {
        eprint!(&*COLOR_INFO => "hint:");
        eprint!(" ");
        eprintln!("{}", hint);
    }
}

fn main() {
    let filename = match env::args().nth(1) {
        Some(f) => f,
        None => panic!("please specifiy the file name"),
    };
    let source = fs::read_to_string(&filename).expect("failed to read source file");

    let tokens = tokenize(&source);

    // println!("--- tokens ---");
    // for token in &tokens {
    //     println!("{:?}", token);
    // }
    // println!();

    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            print_parser_error(&filename, &source, &err);
            return;
        }
    };

    println!("--- ast ---");
    println!("{}", ast);
    println!();

    let hir = lower_ast(&ast);
    println!("--- hir ---");
    println!("{:#?}", hir);

    let thir = match resolve_progam(hir) {
        Ok(resolved) => resolved,
        Err(err) => {
            print_resolver_error(&filename, &source, &err);
            return;
        }
    };
    println!("--- typed hir ---");
    println!("{:#?}", thir);

    println!("--- run ---");
    let state = match run(&ast) {
        Ok(state) => state,
        Err(err) => {
            print_interpreter_error(&filename, &source, &err);
            return;
        }
    };
    println!();

    println!("--- final state ---");
    state.display();
}
