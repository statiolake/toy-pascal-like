use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }

    pub fn new_zero() -> Self {
        Self {
            start: LineColumn::new(0, 0),
            end: LineColumn::new(0, 0),
        }
    }

    pub fn min_wrapping(spans: &[Span]) -> Option<Self> {
        let start = spans.iter().map(|s| s.start).min()?;
        let end = spans.iter().map(|s| s.end).max()?;
        Some(Span { start, end })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(
            b,
            "{}:{}..{}:{}",
            self.start.line + 1,
            self.start.column + 1,
            self.end.line + 1,
            self.end.column + 1,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(span: Span, kind: TokenKind<'a>) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind<'a> {
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
    Number(i32),
    Unknown(char),
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
            TokenKind::Dump => write!(b, "dump"),
            TokenKind::Lt => write!(b, "<"),
            TokenKind::Le => write!(b, "<="),
            TokenKind::Gt => write!(b, ">"),
            TokenKind::Ge => write!(b, ">="),
            TokenKind::Eq => write!(b, "=="),
            TokenKind::Ne => write!(b, "!="),
            TokenKind::OpenPar => write!(b, "("),
            TokenKind::ClosePar => write!(b, ")"),
            TokenKind::Add => write!(b, "+"),
            TokenKind::Sub => write!(b, "-"),
            TokenKind::Mul => write!(b, "*"),
            TokenKind::Div => write!(b, "/"),
            TokenKind::Ident(ident) => write!(b, "{}", ident),
            TokenKind::Number(number) => write!(b, "{}", number),
            TokenKind::Unknown(unknown) => write!(b, "{}", unknown),
        }
    }
}

impl TokenKind<'_> {
    pub fn label(self) -> &'static str {
        match self {
            TokenKind::If => "if",
            TokenKind::Then => "then",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::Do => "do",
            TokenKind::Begin => "begin",
            TokenKind::End => "end",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::AssgEqual => ":=",
            TokenKind::Dump => "dump",
            TokenKind::Lt => "<",
            TokenKind::Le => "<=",
            TokenKind::Gt => ">",
            TokenKind::Ge => ">=",
            TokenKind::Eq => "==",
            TokenKind::Ne => "!=",
            TokenKind::OpenPar => "(",
            TokenKind::ClosePar => ")",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Ident(_) => "{identifier}",
            TokenKind::Number(_) => "{number}",
            TokenKind::Unknown(_) => "{unknown}",
        }
    }
}

pub fn tokenize(source: &str) -> Vec<Token> {
    Splitter::new(source).collect()
}

struct Splitter<'a> {
    source: &'a str,
    ptr: usize,
    linecol: LineColumn,
}

impl<'a> Splitter<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            ptr: 0,
            linecol: LineColumn::new(0, 0),
        }
    }

    fn rest(&self) -> &'a str {
        &self.source[self.ptr..]
    }

    fn peek(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn trim_left(&mut self) {
        while let Some(ch) = self.rest().chars().next() {
            if !ch.is_whitespace() {
                break;
            }
            self.eat(ch);
        }
    }

    fn eat(&mut self, expect: char) -> (&'a str, LineColumn, LineColumn) {
        assert!(
            self.rest().starts_with(expect),
            "failed to eat string '{}'",
            expect
        );

        let res = &self.rest()[..expect.len_utf8()];
        let start = self.linecol;

        self.ptr += expect.len_utf8();
        if expect == '\n' {
            self.linecol.line += 1;
            self.linecol.column = 0;
        } else {
            self.linecol.column += expect.len_utf8();
        }

        let end = self.linecol;

        (res, start, end)
    }

    fn eat_str(&mut self, expect: &str) -> (&'a str, LineColumn, LineColumn) {
        let res = &self.rest()[..expect.len()];
        assert_eq!(expect, res, "failed to eat string '{}'", expect);

        let start = self.linecol;
        for ch in res.chars() {
            self.eat(ch);
        }
        let end = self.linecol;

        (res, start, end)
    }
}

impl<'a> Iterator for Splitter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.trim_left();
        if self.ptr == self.source.len() {
            return None;
        }

        let map = vec![
            ("if", TokenKind::If),
            ("then", TokenKind::Then),
            ("else", TokenKind::Else),
            ("while", TokenKind::While),
            ("do", TokenKind::Do),
            ("begin", TokenKind::Begin),
            ("end", TokenKind::End),
            (",", TokenKind::Comma),
            (";", TokenKind::Semicolon),
            (":=", TokenKind::AssgEqual),
            ("dump", TokenKind::Dump),
            ("<=", TokenKind::Le),
            ("<", TokenKind::Lt),
            (">=", TokenKind::Ge),
            (">", TokenKind::Gt),
            ("==", TokenKind::Eq),
            ("!=", TokenKind::Ne),
            ("(", TokenKind::OpenPar),
            (")", TokenKind::ClosePar),
            ("+", TokenKind::Add),
            ("-", TokenKind::Sub),
            ("*", TokenKind::Mul),
            ("/", TokenKind::Div),
        ];

        for (raw, kind) in map {
            if self.rest().starts_with(raw) {
                let (_, start, end) = self.eat_str(raw);
                let span = Span::new(start, end);
                return Some(Token::new(span, kind));
            }
        }

        // otherwise, treat it as an ident or number
        let next = self.peek().expect("internal error: no chars left");
        let token = if next.is_ascii_digit() {
            // when next digit is number: read consecutive digits as a number
            let number = self
                .rest()
                .chars()
                .take_while(|ch| ch.is_ascii_digit())
                .collect::<String>();
            let (value, start, end) = self.eat_str(&number);
            let span = Span::new(start, end);
            let value = value.parse::<i32>().unwrap();
            Token::new(span, TokenKind::Number(value))
        } else if next.is_ascii_alphabetic() {
            // when next char is ascii alphabet: read consecutive alphabets, numbers and underscore
            // as a identifier
            let ident = self
                .rest()
                .chars()
                .take_while(|&ch| ch.is_ascii_alphanumeric() || ch == '_')
                .collect::<String>();
            let (ident, start, end) = self.eat_str(&ident);
            let span = Span::new(start, end);
            Token::new(span, TokenKind::Ident(ident))
        } else {
            // otherwise, it's unknown char.
            let (_, start, end) = self.eat(next);
            let span = Span::new(start, end);
            Token::new(span, TokenKind::Unknown(next))
        };

        Some(token)
    }
}
