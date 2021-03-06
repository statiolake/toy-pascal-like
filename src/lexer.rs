use crate::span::{LineColumn, Span};
use crate::token::*;

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
            ("function", TokenKind::Function),
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
            (":", TokenKind::Colon),
            ("dump", TokenKind::Dump),
            ("=", TokenKind::Eq),
            ("<>", TokenKind::Ne),
            ("<=", TokenKind::Le),
            ("<", TokenKind::Lt),
            (">=", TokenKind::Ge),
            (">", TokenKind::Gt),
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
            // when next digit is number: read consecutive digits and one as a number
            let mut first_dot = true;
            let number = self
                .rest()
                .chars()
                .take_while(|&ch| {
                    let take_this = ch.is_ascii_digit() || (first_dot && ch == '.');
                    first_dot = first_dot && ch != '.';
                    take_this
                })
                .collect::<String>();
            let (value, start, end) = self.eat_str(&number);
            let span = Span::new(start, end);
            if value.contains('.') {
                Token::new(span, TokenKind::FloatConst(value.parse::<f64>().unwrap()))
            } else {
                Token::new(span, TokenKind::IntConst(value.parse::<i64>().unwrap()))
            }
        } else if next.is_ascii_alphabetic() {
            // when next char is ascii alphabet: read consecutive alphabets, numbers and underscore
            // as a identifier. Parhaps it's a boolean constant `true` or `false`.
            let ident = self
                .rest()
                .chars()
                .take_while(|&ch| ch.is_ascii_alphanumeric() || ch == '_')
                .collect::<String>();
            let (ident, start, end) = self.eat_str(&ident);
            let span = Span::new(start, end);
            let kind = match ident {
                "true" => TokenKind::BoolConst(true),
                "false" => TokenKind::BoolConst(false),
                _ => TokenKind::Ident(ident),
            };
            Token::new(span, kind)
        } else {
            // otherwise, it's unknown char.
            let (_, start, end) = self.eat(next);
            let span = Span::new(start, end);
            Token::new(span, TokenKind::Unknown(next))
        };

        Some(token)
    }
}
