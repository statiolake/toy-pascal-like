use crate::ast::*;
use crate::lexer::{LineColumn, Span, Token, TokenKind};
use itertools::Itertools as _;

#[derive(Debug)]
pub struct Hint {
    pub message: String,
    pub span: Option<(Span, String)>,
}

#[derive(thiserror::Error, Debug)]
#[error("{span}: {kind}")]
pub struct ParserError<'i> {
    pub span: Span,
    pub kind: ParserErrorKind<'i>,
    pub hints: Vec<Hint>,
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
    #[error("some tokens are left unread")]
    NotEntirelyConsumed,
}

impl ParserErrorKind<'_> {
    pub fn summary(&self) -> String {
        match self {
            ParserErrorKind::UnexpectedToken { token_kind, .. } => {
                format!("unexpected {}", token_kind)
            }
            ParserErrorKind::UnexpectedEof { .. } => "unexpected eof".to_string(),
            ParserErrorKind::NotEntirelyConsumed => "those tokens were not parsed".to_string(),
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
            hints: vec![],
        }
    }

    pub fn unexpected_eof(span: Span, expects: Option<Vec<TokenKind<'i>>>) -> Self {
        Self {
            span,
            kind: ParserErrorKind::UnexpectedEof { expects },
            hints: vec![],
        }
    }

    pub fn not_entirely_consumed(span: Span) -> Self {
        Self {
            span,
            kind: ParserErrorKind::NotEntirelyConsumed,
            hints: vec![],
        }
    }
}

pub fn parse<'i>(tokens: &[Token<'i>]) -> Result<'i, Ast<AstStmt>> {
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_stmt()?;
    if let Some(span_left_tokens) = parser.span_left_tokens() {
        return Err(ParserError::not_entirely_consumed(span_left_tokens));
    }

    Ok(stmt)
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
    pub fn eat(&mut self, expect: TokenKind<'i>) -> Result<'i, Token> {
        match self.next_token() {
            Ok(token) if token.kind == expect => Ok(token),
            Ok(token) => Err(ParserError::unexpected_token(token, Some(vec![expect]))),
            Err(_) => Err(ParserError::unexpected_eof(
                self.span_eof(),
                Some(vec![expect]),
            )),
        }
    }

    fn span_left_tokens(&self) -> Option<Span> {
        let spans = (&self.input[self.ptr..])
            .iter()
            .map(|t| t.span)
            .collect_vec();
        Span::min_wrapping(&spans)
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
    pub fn parse_stmt(&mut self) -> Result<'i, Ast<AstStmt>> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (_, TokenKind::Function) => Ok(AstStmt::from_funcdef_stmt(self.parse_funcdef_stmt()?)),
            (_, TokenKind::If) => Ok(AstStmt::from_if_stmt(self.parse_if_stmt()?)),
            (_, TokenKind::While) => Ok(AstStmt::from_while_stmt(self.parse_while_stmt()?)),
            (_, TokenKind::Begin) => Ok(AstStmt::from_begin_stmt(self.parse_begin_stmt()?)),
            (_, TokenKind::Dump) => Ok(AstStmt::from_dump_stmt(self.parse_dump_stmt()?)),
            (_, TokenKind::Ident(_)) => Ok(AstStmt::from_assg_stmt(self.parse_assg_stmt()?)),
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

    fn parse_funcdef_stmt(&mut self) -> Result<'i, Ast<AstFuncdefStmt>> {
        let start = self.eat(TokenKind::Function)?.span.start;
        let name = self.parse_ident()?;
        self.eat(TokenKind::OpenPar)?;
        let params = self.parse_param_list()?;
        self.eat(TokenKind::ClosePar)?;
        self.eat(TokenKind::Semicolon)?;
        let body = self.parse_begin_stmt()?;
        let end = body.span.end;

        Ok(AstFuncdefStmt::from_elements(
            Span::new(start, end),
            name,
            params,
            body,
        ))
    }

    fn parse_param_list(&mut self) -> Result<'i, Ast<AstParamList>> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (t, TokenKind::ClosePar) => Ok(Ast {
                ast: AstParamList::Empty,
                span: Span::new(t.span.start, t.span.start),
            }),
            _ => {
                let ident = self.parse_ident()?;
                let next = self.lookahead(0);
                let next = if next.as_ref().map(|t| t.kind).ok() == Some(TokenKind::Comma) {
                    self.eat(TokenKind::Comma)?;
                    self.parse_param_list()?
                } else {
                    let span = next
                        .map(|t| Span::new(t.span.start, t.span.start))
                        .unwrap_or_else(|_| self.span_eof());
                    Ast {
                        ast: AstParamList::Empty,
                        span,
                    }
                };
                let start = ident.span.start;
                let end = next.span.end;

                Ok(AstParamList::from_elements(
                    Span::new(start, end),
                    ident,
                    next,
                ))
            }
        }
    }

    fn parse_if_stmt(&mut self) -> Result<'i, Ast<AstIfStmt>> {
        let start = self.eat(TokenKind::If)?.span.start;
        let cond = self.parse_bool_expr()?;
        self.eat(TokenKind::Then)?;
        let then = self.parse_stmt()?;
        self.eat(TokenKind::Else)?;
        let otherwise = self.parse_stmt()?;
        let end = otherwise.span.end;

        Ok(AstIfStmt::from_elements(
            Span::new(start, end),
            cond,
            then,
            otherwise,
        ))
    }

    fn parse_while_stmt(&mut self) -> Result<'i, Ast<AstWhileStmt>> {
        let start = self.eat(TokenKind::While)?.span.start;
        let cond = self.parse_bool_expr()?;
        self.eat(TokenKind::Do)?;
        let body = self.parse_stmt()?;
        let end = body.span.end;

        Ok(AstWhileStmt::from_elements(
            Span::new(start, end),
            cond,
            body,
        ))
    }

    fn parse_begin_stmt(&mut self) -> Result<'i, Ast<AstBeginStmt>> {
        let start = self.eat(TokenKind::Begin)?.span.start;
        let list = self.parse_stmt_list()?;
        let end = match self.eat(TokenKind::End) {
            Ok(token) => token.span.end,
            Err(mut err) => {
                err.hints.push(Hint {
                    span: Some((
                        list.ast.last_stmt().span,
                        "try adding semicolon after this".to_string(),
                    )),
                    message: "parhaps you missed a semicolon".to_string(),
                });
                return Err(err);
            }
        };

        Ok(AstBeginStmt::from_list(Span::new(start, end), list))
    }

    fn parse_stmt_list(&mut self) -> Result<'i, Ast<AstStmtList>> {
        let stmt = self.parse_stmt()?;
        let start = stmt.span.start;
        let token = self.lookahead(0)?;
        let (next, end) = if token.kind == TokenKind::Semicolon {
            let semi_span = self.eat(TokenKind::Semicolon)?.span;
            // peek the next token for a trailing semicolon hint
            let next_is_end = self.lookahead(0).map(|t| t.kind).ok() == Some(TokenKind::End);
            let rest = match self.parse_stmt_list() {
                Ok(rest) => rest,
                Err(mut err) => {
                    if next_is_end {
                        err.hints.push(Hint {
                            span: Some((semi_span, "trailing ; is not allowed".to_string())),
                            message: "perhaps removing this semicolon may help".to_string(),
                        });
                    }
                    return Err(err);
                }
            };
            let end = rest.span.end;
            (Some(rest), end)
        } else {
            (None, stmt.span.end)
        };

        Ok(AstStmtList::from_elements(
            Span::new(start, end),
            stmt,
            next,
        ))
    }

    fn parse_assg_stmt(&mut self) -> Result<'i, Ast<AstAssgStmt>> {
        let var = self.parse_var()?;
        self.eat(TokenKind::AssgEqual)?;
        let expr = self.parse_arith_expr()?;
        let start = var.span.start;
        let end = expr.span.end;

        Ok(AstAssgStmt::from_elements(Span::new(start, end), var, expr))
    }

    fn parse_dump_stmt(&mut self) -> Result<'i, Ast<AstDumpStmt>> {
        self.eat(TokenKind::Dump)?;
        let var = self.parse_var()?;

        Ok(AstDumpStmt::from_var(var))
    }

    fn parse_bool_expr(&mut self) -> Result<'i, Ast<AstBoolExpr>> {
        let lhs = self.parse_arith_expr()?;
        let op = self.parse_compare_op()?;
        let rhs = self.parse_arith_expr()?;
        let start = lhs.span.start;
        let end = rhs.span.end;

        Ok(AstBoolExpr::from_elements(
            Span::new(start, end),
            lhs,
            op,
            rhs,
        ))
    }

    fn parse_compare_op(&mut self) -> Result<'i, Ast<AstCompareOp>> {
        match self.next_token().map(|t| (t, t.kind))? {
            (tok, TokenKind::Lt) => Ok(Ast {
                ast: AstCompareOp::Lt,
                span: tok.span,
            }),
            (tok, TokenKind::Gt) => Ok(Ast {
                ast: AstCompareOp::Gt,
                span: tok.span,
            }),
            (tok, TokenKind::Le) => Ok(Ast {
                ast: AstCompareOp::Le,
                span: tok.span,
            }),
            (tok, TokenKind::Ge) => Ok(Ast {
                ast: AstCompareOp::Ge,
                span: tok.span,
            }),
            (tok, TokenKind::Eq) => Ok(Ast {
                ast: AstCompareOp::Eq,
                span: tok.span,
            }),
            (tok, TokenKind::Ne) => Ok(Ast {
                ast: AstCompareOp::Ne,
                span: tok.span,
            }),
            (tok, _) => Err(ParserError::unexpected_token(
                tok,
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

    fn parse_arith_expr(&mut self) -> Result<'i, Ast<AstArithExpr>> {
        let mul = self.parse_mul_expr()?;
        self.parse_arith_expr_impl(AstArithExpr::from_mul(mul))
    }

    fn parse_arith_expr_impl(&mut self, lhs: Ast<AstArithExpr>) -> Result<'i, Ast<AstArithExpr>> {
        match self.lookahead(0).map(|t| t.kind).ok() {
            Some(TokenKind::Add) => {
                let start = lhs.span.start;
                self.eat(TokenKind::Add)?;
                let rhs = self.parse_mul_expr()?;
                let end = rhs.span.end;
                self.parse_arith_expr_impl(AstArithExpr::add_from_elements(
                    Span::new(start, end),
                    lhs,
                    rhs,
                ))
            }
            Some(TokenKind::Sub) => {
                let start = lhs.span.start;
                self.eat(TokenKind::Sub)?;
                let rhs = self.parse_mul_expr()?;
                let end = rhs.span.end;
                self.parse_arith_expr_impl(AstArithExpr::sub_from_elements(
                    Span::new(start, end),
                    lhs,
                    rhs,
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn parse_mul_expr(&mut self) -> Result<'i, Ast<AstMulExpr>> {
        let unary = self.parse_unary_expr()?;
        self.parse_mul_expr_impl(AstMulExpr::from_unary(unary))
    }

    fn parse_mul_expr_impl(&mut self, lhs: Ast<AstMulExpr>) -> Result<'i, Ast<AstMulExpr>> {
        match self.lookahead(0).map(|t| t.kind).ok() {
            Some(TokenKind::Mul) => {
                let start = lhs.span.start;
                self.eat(TokenKind::Mul)?;
                let rhs = self.parse_unary_expr()?;
                let end = rhs.span.end;
                self.parse_mul_expr_impl(AstMulExpr::mul_from_elements(
                    Span::new(start, end),
                    lhs,
                    rhs,
                ))
            }
            Some(TokenKind::Div) => {
                let start = lhs.span.start;
                self.eat(TokenKind::Div)?;
                let rhs = self.parse_unary_expr()?;
                let end = rhs.span.end;
                self.parse_mul_expr_impl(AstMulExpr::div_from_elements(
                    Span::new(start, end),
                    lhs,
                    rhs,
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn parse_unary_expr(&mut self) -> Result<'i, Ast<AstUnaryExpr>> {
        match self.lookahead(0)?.kind {
            TokenKind::Sub => {
                let start = self.eat(TokenKind::Sub)?.span.start;
                let unary = self.parse_unary_expr()?;
                let end = unary.span.end;
                Ok(AstUnaryExpr::neg_from(Span::new(start, end), unary))
            }
            _ => self.parse_primary_expr().map(AstUnaryExpr::from_primary),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<'i, Ast<AstPrimaryExpr>> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (_, TokenKind::OpenPar) => {
                let start = self.eat(TokenKind::OpenPar)?.span.start;
                let expr = self.parse_arith_expr()?;
                let end = self.eat(TokenKind::ClosePar)?.span.end;
                Ok(AstPrimaryExpr::paren_from_elements(
                    Span::new(start, end),
                    expr,
                ))
            }
            (_, TokenKind::IntConst(_)) | (_, TokenKind::FloatConst(_)) => {
                let value = self.parse_const()?;
                Ok(AstPrimaryExpr::from_const(value))
            }
            (_, TokenKind::Ident(_)) => {
                // peek next identifiers to determine whether it's fncall or variable, look one
                // token ahead.
                if self.lookahead(1).map(|t| t.kind).ok() == Some(TokenKind::OpenPar) {
                    // fncall
                    let fncall = self.parse_fncall()?;
                    Ok(AstPrimaryExpr::from_fncall(fncall))
                } else {
                    let var = self.parse_var()?;
                    Ok(AstPrimaryExpr::from_var(var))
                }
            }
            (token, _) => Err(ParserError::unexpected_token(
                token,
                Some(vec![
                    TokenKind::OpenPar,
                    TokenKind::IntConst(0),
                    TokenKind::FloatConst(0.0),
                    TokenKind::Ident(""),
                ]),
            )),
        }
    }

    fn parse_fncall(&mut self) -> Result<'i, Ast<AstFnCall>> {
        let ident = self.parse_ident()?;
        self.eat(TokenKind::OpenPar)?;
        let args = self.parse_argument_list()?;
        let end = self.eat(TokenKind::ClosePar)?.span.end;
        let start = ident.span.start;

        Ok(AstFnCall::from_elements(Span::new(start, end), ident, args))
    }

    fn parse_argument_list(&mut self) -> Result<'i, Ast<AstArgumentList>> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (t, TokenKind::ClosePar) => Ok(Ast {
                ast: AstArgumentList::Empty,
                span: Span::new(t.span.start, t.span.start),
            }),
            _ => {
                let expr = self.parse_arith_expr()?;
                let next = self.lookahead(0);
                let next = if next.as_ref().map(|t| t.kind).ok() == Some(TokenKind::Comma) {
                    self.eat(TokenKind::Comma)?;
                    self.parse_argument_list()?
                } else {
                    let span = next
                        .map(|t| Span::new(t.span.start, t.span.start))
                        .unwrap_or_else(|_| self.span_eof());
                    Ast {
                        ast: AstArgumentList::Empty,
                        span,
                    }
                };
                let start = expr.span.start;
                let end = next.span.end;

                Ok(AstArgumentList::from_elements(
                    Span::new(start, end),
                    expr,
                    next,
                ))
            }
        }
    }

    fn parse_const(&mut self) -> Result<'i, Ast<AstConst>> {
        match self.next_token().map(|t| (t, t.kind))? {
            (tok, TokenKind::IntConst(value)) => Ok(Ast {
                ast: AstConst::Int(value),
                span: tok.span,
            }),
            (tok, TokenKind::FloatConst(value)) => Ok(Ast {
                ast: AstConst::Float(value),
                span: tok.span,
            }),
            (tok, _) => Err(ParserError::unexpected_token(
                tok,
                Some(vec![TokenKind::IntConst(0), TokenKind::FloatConst(0.0)]),
            )),
        }
    }

    fn parse_var(&mut self) -> Result<'i, Ast<AstVar>> {
        self.parse_ident().map(AstVar::from_ident)
    }

    fn parse_ident(&mut self) -> Result<'i, Ast<AstIdent>> {
        match self.next_token().map(|t| (t, t.kind))? {
            (tok, TokenKind::Ident(ident)) => Ok(Ast {
                ast: AstIdent(ident.to_string()),
                span: tok.span,
            }),
            (tok, _) => Err(ParserError::unexpected_token(
                tok,
                Some(vec![TokenKind::Ident("")]),
            )),
        }
    }
}
