use crate::span::Span;
use crate::{parser, resolver, typeck};
use itertools::Itertools as _;
use once_cell::sync::Lazy;
use std::cmp::min;
use std::fmt;
use termcolor::{Color, ColorSpec, WriteColor};

#[derive(thiserror::Error, Debug)]
pub enum Error<'i> {
    #[error("{0}")]
    ParserError(parser::ParserError<'i>),

    #[error("{0}")]
    ResolverError(resolver::ResolverError),

    #[error("{0}")]
    TypeckError(typeck::TypeckError),
}

impl<'i> From<typeck::TypeckError> for Error<'i> {
    fn from(v: typeck::TypeckError) -> Self {
        Self::TypeckError(v)
    }
}

impl<'i> From<resolver::ResolverError> for Error<'i> {
    fn from(v: resolver::ResolverError) -> Self {
        Self::ResolverError(v)
    }
}

impl<'i> From<parser::ParserError<'i>> for Error<'i> {
    fn from(v: parser::ParserError<'i>) -> Self {
        Self::ParserError(v)
    }
}

pub fn print_error<W: WriteColor>(eout: &mut W, filename: &str, source: &str, err: &Error) {
    match err {
        Error::ParserError(err) => print_parser_error(eout, filename, source, err),
        Error::ResolverError(err) => print_resolver_error(eout, filename, source, err),
        Error::TypeckError(err) => print_typeck_error(eout, filename, source, err),
    }
}

macro_rules! eprint {
    (=> $eout:expr, $fg:expr => $fmt:literal $($args:tt)*) => {
        print_with_color($eout, format_args!($fmt $($args)*), $fg);
    };
    (=> $eout:expr, $($args:tt)*) => {
        eprint!(=> $eout, &ColorSpec::new() => $($args)*);
    };
}

macro_rules! eprintln {
    (=> $eout:expr, $fg:expr => $fmt:literal $($args:tt)*) => {
        print_with_color($eout, format_args!(concat!($fmt, "\n") $($args)*), $fg);
    };
    (=> $eout:expr, $($args:tt)*) => {
        eprintln!(=> $eout, &ColorSpec::new() => $($args)*);
    };
}

pub fn print_parser_error<W: WriteColor>(
    eout: &mut W,
    filename: &str,
    source: &str,
    err: &parser::ParserError,
) {
    eprint!(=> eout, &*COLOR_ERROR => "parser error:");
    eprint!(=> eout, " ");
    eprintln!(=> eout, &*COLOR_MESSAGE => "{}", err.kind);
    show_span(eout, filename, source, err.span, &err.kind.summary());
    for hint in &err.hints {
        eprint!(=> eout, &*COLOR_INFO => "hint:");
        eprint!(=> eout, " ");
        eprintln!(=> eout, "{}", hint.message);
        if let Some((span, summary)) = &hint.span {
            show_span(eout, filename, source, *span, summary);
        }
    }
}

pub fn print_resolver_error<W: WriteColor>(
    eout: &mut W,
    filename: &str,
    source: &str,
    err: &resolver::ResolverError,
) {
    eprint!(=> eout, &*COLOR_ERROR => "resolver error:");
    eprint!(=> eout, " ");
    eprintln!(=> eout, &*COLOR_MESSAGE => "{}", err.kind);
    show_span(eout, filename, source, err.span, &err.kind.summary());

    // for hint in &err.kind.hints() {
    //     eprint!(&*COLOR_INFO => "hint:");
    //     eprint!(" ");
    //     eprintln!("{}", hint);
    // }
}

pub fn print_typeck_error<W: WriteColor>(
    eout: &mut W,
    filename: &str,
    source: &str,
    err: &typeck::TypeckError,
) {
    eprint!(=> eout, &*COLOR_ERROR => "type checker error:");
    eprint!(=> eout, " ");
    eprintln!(=> eout, &*COLOR_MESSAGE => "{}", err.kind);
    show_span(eout, filename, source, err.span, &err.kind.summary());

    // for hint in &err.kind.hints() {
    //     eprint!(&*COLOR_INFO => "hint:");
    //     eprint!(" ");
    //     eprintln!("{}", hint);
    // }
}

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

fn print_with_color<W: WriteColor>(eout: &mut W, display: fmt::Arguments, color: &ColorSpec) {
    eout.set_color(&color).unwrap();
    eout.write_fmt(display).unwrap();
    eout.set_color(&ColorSpec::new()).unwrap();
}

fn show_span<W: WriteColor>(eout: &mut W, filename: &str, source: &str, span: Span, summary: &str) {
    let lines = source.lines().enumerate().collect_vec();
    let line_number_width = source.len().to_string().len() + 1;
    let line_number_indent = " ".repeat(line_number_width);
    let Span { start, end } = span;
    let window = &lines[start.line.saturating_sub(3)..min(lines.len(), end.line + 3)];

    eprint!(=> eout, &*COLOR_INFO => "{}-->", line_number_indent);
    eprintln!(=> eout, " {}:{}:{}", filename, start.line + 1, start.column + 1);

    eprintln!(=> eout, &*COLOR_INFO => "{} |", line_number_indent);
    for (number, line) in window {
        eprint!(=> eout, &*COLOR_INFO => "{:>width$} |", number + 1, width=line_number_width);
        eprintln!(=> eout, " {}", line);
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

            eprint!(=> eout, &*COLOR_INFO => "{} |", line_number_indent);
            eprint!(=> eout, " {}", " ".repeat(start_column));
            eprint!(
                => eout, &*COLOR_ERROR => "{}",
                "^".repeat(end_column - start_column),
            );

            if end.line == *number {
                eprint!(=> eout, &*COLOR_ERROR => " {}", summary);
            }

            eprintln!(=> eout, "");
        }
    }
    eprintln!(=> eout, &*COLOR_INFO => "{} |", line_number_indent);
}
