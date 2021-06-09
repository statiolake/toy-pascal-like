use crate::ast::*;
use crate::lexer::{LineColumn, Span, Token, TokenKind};
use itertools::Itertools as _;

#[derive(thiserror::Error, Debug)]
#[error("{span}: {kind}")]
pub struct ParserError<'i> {
    pub span: Span,
    pub kind: ParserErrorKind<'i>,
}

#[derive(thiserror::Error, Debug)]
pub enum ParserErrorKind<'i> {
    #[error("unexpected token `{token_kind}`{}", format_expects(&.expects))]
    UnexpectedToken {
        token_kind: TokenKind<'i>,
        expects: Option<Vec<TokenKind<'i>>>,
    },
    #[error("unexpected EOF{}", format_expects(&.expects))]
    UnexpectedEof { expects: Option<Vec<TokenKind<'i>>> },
}

impl ParserErrorKind<'_> {
    pub fn summary(&self) -> String {
        match self {
            ParserErrorKind::UnexpectedToken { token_kind, .. } => {
                format!("unexpected {}", token_kind)
            }
            ParserErrorKind::UnexpectedEof { .. } => "unexpected eof".to_string(),
        }
    }
}

impl<'i> ParserError<'i> {
    pub fn unexpected_token(token: Token<'i>, expects: Option<Vec<TokenKind<'i>>>) -> Self {
        Self {
            span: token.span,
            kind: ParserErrorKind::UnexpectedToken {
                token_kind: token.kind,
                expects,
            },
        }
    }

    pub fn unexpected_eof(span: Span, expects: Option<Vec<TokenKind<'i>>>) -> Self {
        Self {
            span,
            kind: ParserErrorKind::UnexpectedEof { expects },
        }
    }
}

fn format_expects(expects: &Option<Vec<TokenKind>>) -> String {
    match expects {
        Some(expects) => format!(
            ": expected one of [{}]",
            expects.iter().map(|t| t.label()).format(", ")
        ),
        None => String::new(),
    }
}

type Result<'i, T, E = ParserError<'i>> = std::result::Result<T, E>;

#[derive(Debug, Clone, Copy)]
pub struct Parser<'i, 'toks> {
    input: &'toks [Token<'i>],
    ptr: usize,
}

impl<'i, 'toks> Parser<'i, 'toks> {
    pub fn new(input: &'toks [Token<'i>]) -> Self {
        Self { input, ptr: 0 }
    }

    pub fn is_finished(&self) -> bool {
        self.ptr == self.input.len()
    }

    pub fn lookahead(&self, n: usize) -> Result<'i, Token<'i>> {
        if self.ptr + n < self.input.len() {
            Ok(self.input[self.ptr + n])
        } else {
            Err(ParserError::unexpected_eof(self.span_eof(), None))
        }
    }

    pub fn next_token(&mut self) -> Result<'i, Token<'i>> {
        let res = self.lookahead(0);
        if !self.is_finished() {
            self.ptr += 1;
        }
        res
    }

    #[track_caller]
    pub fn eat(&mut self, expect: TokenKind<'i>) -> Result<'i, ()> {
        match self.next_token() {
            Ok(token) if token.kind == expect => Ok(()),
            Ok(token) => Err(ParserError::unexpected_token(token, Some(vec![expect]))),
            Err(_) => Err(ParserError::unexpected_eof(
                self.span_eof(),
                Some(vec![expect]),
            )),
        }
    }

    fn span_eof(&self) -> Span {
        self.input
            .last()
            .map(|t| t.span)
            .map(|s| Span::new(s.end, s.end))
            .unwrap_or_else(|| Span::new(LineColumn::new(0, 0), LineColumn::new(0, 0)))
    }
}

impl<'i, 'toks> Parser<'i, 'toks> {
    pub fn parse_stmt(&mut self) -> Result<'i, AstStmt> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (_, TokenKind::If) => Ok(AstStmt::IfStmt(Box::new(self.parse_if_stmt()?))),
            (_, TokenKind::While) => Ok(AstStmt::WhileStmt(Box::new(self.parse_while_stmt()?))),
            (_, TokenKind::Begin) => Ok(AstStmt::BeginStmt(Box::new(self.parse_begin_stmt()?))),
            (_, TokenKind::Dump) => Ok(AstStmt::DumpStmt(Box::new(self.parse_dump_stmt()?))),
            (_, TokenKind::Ident(_)) => Ok(AstStmt::AssgStmt(Box::new(self.parse_assg_stmt()?))),
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![
                    TokenKind::If,
                    TokenKind::While,
                    TokenKind::Begin,
                    TokenKind::Begin,
                    TokenKind::Dump,
                    TokenKind::Ident(""),
                ]),
            )),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<'i, AstIfStmt> {
        self.eat(TokenKind::If)?;
        let cond = Box::new(self.parse_bool_expr()?);
        self.eat(TokenKind::Then)?;
        let then = Box::new(self.parse_stmt()?);
        self.eat(TokenKind::Else)?;
        let otherwise = Box::new(self.parse_stmt()?);

        Ok(AstIfStmt {
            cond,
            then,
            otherwise,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<'i, AstWhileStmt> {
        self.eat(TokenKind::While)?;
        let cond = Box::new(self.parse_bool_expr()?);
        self.eat(TokenKind::Do)?;
        let body = Box::new(self.parse_stmt()?);

        Ok(AstWhileStmt { cond, body })
    }

    fn parse_begin_stmt(&mut self) -> Result<'i, AstBeginStmt> {
        self.eat(TokenKind::Begin)?;
        let list = Box::new(self.parse_stmt_list()?);
        self.eat(TokenKind::End)?;

        Ok(AstBeginStmt { list })
    }

    fn parse_stmt_list(&mut self) -> Result<'i, AstStmtList> {
        let stmt = Box::new(self.parse_stmt()?);

        let token = self.lookahead(0)?;
        let next = if token.kind == TokenKind::Semicolon {
            self.eat(TokenKind::Semicolon)?;
            Some(Box::new(self.parse_stmt_list()?))
        } else {
            None
        };

        Ok(AstStmtList { stmt, next })
    }

    fn parse_assg_stmt(&mut self) -> Result<'i, AstAssgStmt> {
        let var = Box::new(self.parse_var()?);
        self.eat(TokenKind::AssgEqual)?;
        let expr = Box::new(self.parse_arith_expr()?);

        Ok(AstAssgStmt { var, expr })
    }

    fn parse_dump_stmt(&mut self) -> Result<'i, AstDumpStmt> {
        self.eat(TokenKind::Dump)?;
        let var = Box::new(self.parse_var()?);

        Ok(AstDumpStmt { var })
    }

    fn parse_bool_expr(&mut self) -> Result<'i, AstBoolExpr> {
        let lhs = Box::new(self.parse_arith_expr()?);
        let op = Box::new(self.parse_compare_op()?);
        let rhs = Box::new(self.parse_arith_expr()?);

        Ok(AstBoolExpr { lhs, op, rhs })
    }

    fn parse_compare_op(&mut self) -> Result<'i, AstCompareOp> {
        match self.next_token().map(|t| (t, t.kind))? {
            (_, TokenKind::Lt) => Ok(AstCompareOp::Lt),
            (_, TokenKind::Gt) => Ok(AstCompareOp::Gt),
            (_, TokenKind::Le) => Ok(AstCompareOp::Le),
            (_, TokenKind::Ge) => Ok(AstCompareOp::Ge),
            (_, TokenKind::Eq) => Ok(AstCompareOp::Eq),
            (_, TokenKind::Ne) => Ok(AstCompareOp::Ne),
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![
                    TokenKind::Lt,
                    TokenKind::Gt,
                    TokenKind::Le,
                    TokenKind::Ge,
                    TokenKind::Eq,
                    TokenKind::Ne,
                ]),
            )),
        }
    }

    fn parse_arith_expr(&mut self) -> Result<'i, AstArithExpr> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (_, TokenKind::OpenPar) => {
                self.eat(TokenKind::OpenPar)?;
                let lhs = Box::new(self.parse_arith_expr()?);
                let op = Box::new(self.parse_arith_op()?);
                let rhs = Box::new(self.parse_arith_expr()?);
                self.eat(TokenKind::ClosePar)?;
                Ok(AstArithExpr::Op { lhs, op, rhs })
            }
            (_, TokenKind::Number(_)) => {
                let value = Box::new(self.parse_const()?);
                Ok(AstArithExpr::Const(value))
            }
            (_, TokenKind::Ident(_)) => {
                // peek next identifiers to determine whether it's fncall or variable, look one
                // token ahead.
                if self.lookahead(1).map(|t| t.kind).ok() == Some(TokenKind::OpenPar) {
                    // fncall
                    let fncall = Box::new(self.parse_fncall()?);
                    Ok(AstArithExpr::FnCall(fncall))
                } else {
                    let var = Box::new(self.parse_var()?);
                    Ok(AstArithExpr::Var(var))
                }
            }
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![
                    TokenKind::OpenPar,
                    TokenKind::Number(0),
                    TokenKind::Ident(""),
                ]),
            )),
        }
    }

    fn parse_fncall(&mut self) -> Result<'i, AstFnCall> {
        let ident = Box::new(self.parse_ident()?);
        self.eat(TokenKind::OpenPar)?;
        let args = Box::new(self.parse_argument_list()?);
        self.eat(TokenKind::ClosePar)?;

        Ok(AstFnCall { ident, args })
    }

    fn parse_argument_list(&mut self) -> Result<'i, AstArgumentList> {
        match self.lookahead(0).map(|t| t.kind)? {
            TokenKind::ClosePar => Ok(AstArgumentList::Empty),
            _ => {
                let expr = Box::new(self.parse_arith_expr()?);
                let next = if self.lookahead(0).map(|t| t.kind).ok() == Some(TokenKind::Comma) {
                    self.eat(TokenKind::Comma)?;
                    Box::new(self.parse_argument_list()?)
                } else {
                    Box::new(AstArgumentList::Empty)
                };

                Ok(AstArgumentList::Nonempty { expr, next })
            }
        }
    }

    fn parse_arith_op(&mut self) -> Result<'i, AstArithOp> {
        match self.next_token().map(|t| (t, t.kind))? {
            (_, TokenKind::Add) => Ok(AstArithOp::Add),
            (_, TokenKind::Sub) => Ok(AstArithOp::Sub),
            (_, TokenKind::Mul) => Ok(AstArithOp::Mul),
            (_, TokenKind::Div) => Ok(AstArithOp::Div),
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![
                    TokenKind::Add,
                    TokenKind::Sub,
                    TokenKind::Mul,
                    TokenKind::Div,
                ]),
            )),
        }
    }

    fn parse_const(&mut self) -> Result<'i, AstConst> {
        match self.next_token().map(|t| (t, t.kind))? {
            (_, TokenKind::Number(value)) => Ok(AstConst(value)),
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![TokenKind::Number(0)]),
            )),
        }
    }

    fn parse_var(&mut self) -> Result<'i, AstVar> {
        self.parse_ident().map(AstVar)
    }

    fn parse_ident(&mut self) -> Result<'i, AstIdent> {
        match self.next_token().map(|t| (t, t.kind))? {
            (_, TokenKind::Ident(ident)) => Ok(AstIdent(ident.to_string())),
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![TokenKind::Ident("")]),
            )),
        }
    }
}
