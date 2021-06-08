#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    If,
    Then,
    Else,
    While,
    Do,
    Begin,
    End,
    Semicolon,
    AssgEqual,
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
    Other(&'a str),
}

pub fn tokenize(source: &str) -> Vec<Token> {
    Splitter::new(source).collect()
}

struct Splitter<'a> {
    source: &'a str,
    ptr: usize,
}

impl<'a> Splitter<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, ptr: 0 }
    }

    fn rest(&self) -> &'a str {
        &self.source[self.ptr..]
    }

    fn trim_left(&mut self) {
        while let Some(ch) = self.rest().chars().next() {
            if !ch.is_whitespace() {
                break;
            }

            self.ptr += ch.len_utf8();
        }
    }

    fn eat(&mut self, expect: char) -> &'a str {
        assert!(
            self.rest().starts_with(expect),
            "failed to eat string '{}'",
            expect
        );
        let res = &self.rest()[..expect.len_utf8()];
        self.ptr += expect.len_utf8();

        res
    }

    fn eat_str(&mut self, expect: &str) -> &'a str {
        assert!(
            self.rest().starts_with(expect),
            "failed to eat string '{}'",
            expect
        );
        let res = &self.rest()[..expect.len()];
        self.ptr += expect.len();

        res
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
            ("if", Token::If),
            ("then", Token::Then),
            ("else", Token::Else),
            ("while", Token::While),
            ("do", Token::Do),
            ("begin", Token::Begin),
            ("end", Token::End),
            (";", Token::Semicolon),
            (":=", Token::AssgEqual),
            ("<=", Token::Le),
            ("<", Token::Lt),
            (">=", Token::Ge),
            (">", Token::Gt),
            ("==", Token::Eq),
            ("!=", Token::Ne),
            ("(", Token::OpenPar),
            (")", Token::ClosePar),
            ("+", Token::Add),
            ("-", Token::Sub),
            ("*", Token::Mul),
            ("/", Token::Div),
        ];

        for (raw, token) in map {
            if self.rest().starts_with(raw) {
                self.eat_str(raw);
                return Some(token);
            }
        }

        // otherwise, treat it as an other token
        let ch = self
            .rest()
            .chars()
            .next()
            .expect("internal error: no chars left");

        Some(Token::Other(self.eat(ch)))
    }
}
