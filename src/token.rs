use crate::span::Span;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(span: Span, kind: TokenKind<'a>) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'a> {
    Function,
    If,
    Then,
    Else,
    While,
    Do,
    Begin,
    End,
    Comma,
    Semicolon,
    AssgEqual,
    Colon,
    Dump,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    OpenPar,
    ClosePar,
    Add,
    Sub,
    Mul,
    Div,
    Ident(&'a str),
    IntConst(i64),
    FloatConst(f64),
    BoolConst(bool),
    Unknown(char),
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Function => write!(b, "function"),
            TokenKind::If => write!(b, "if"),
            TokenKind::Then => write!(b, "then"),
            TokenKind::Else => write!(b, "else"),
            TokenKind::While => write!(b, "while"),
            TokenKind::Do => write!(b, "do"),
            TokenKind::Begin => write!(b, "begin"),
            TokenKind::End => write!(b, "end"),
            TokenKind::Comma => write!(b, ","),
            TokenKind::Semicolon => write!(b, ";"),
            TokenKind::AssgEqual => write!(b, ":="),
            TokenKind::Colon => write!(b, ":"),
            TokenKind::Dump => write!(b, "dump"),
            TokenKind::Lt => write!(b, "<"),
            TokenKind::Le => write!(b, "<="),
            TokenKind::Gt => write!(b, ">"),
            TokenKind::Ge => write!(b, ">="),
            TokenKind::Eq => write!(b, "="),
            TokenKind::Ne => write!(b, "<>"),
            TokenKind::OpenPar => write!(b, "("),
            TokenKind::ClosePar => write!(b, ")"),
            TokenKind::Add => write!(b, "+"),
            TokenKind::Sub => write!(b, "-"),
            TokenKind::Mul => write!(b, "*"),
            TokenKind::Div => write!(b, "/"),
            TokenKind::Ident(ident) => write!(b, "{}", ident),
            TokenKind::IntConst(int) => write!(b, "{}", int),
            TokenKind::FloatConst(float) => write!(b, "{}", float),
            TokenKind::BoolConst(boolean) => write!(b, "{}", boolean),
            TokenKind::Unknown(unknown) => write!(b, "{}", unknown),
        }
    }
}

impl TokenKind<'_> {
    pub fn label(self) -> &'static str {
        match self {
            TokenKind::Function => "function",
            TokenKind::If => "if",
            TokenKind::Then => "then",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::Do => "do",
            TokenKind::Begin => "begin",
            TokenKind::End => "end",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Colon => ":",
            TokenKind::AssgEqual => ":=",
            TokenKind::Dump => "dump",
            TokenKind::Lt => "<",
            TokenKind::Le => "<=",
            TokenKind::Gt => ">",
            TokenKind::Ge => ">=",
            TokenKind::Eq => "=",
            TokenKind::Ne => "<>",
            TokenKind::OpenPar => "(",
            TokenKind::ClosePar => ")",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Ident(_) => "{identifier}",
            TokenKind::IntConst(_) => "{integer}",
            TokenKind::FloatConst(_) => "{float}",
            TokenKind::BoolConst(_) => "{bool}",
            TokenKind::Unknown(_) => "{unknown}",
        }
    }
}
