use crate::ast::*;
use crate::lexer::{Token, TokenKind};
use crate::span::{LineColumn, Span};
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

pub fn parse<'i>(tokens: &[Token<'i>]) -> Result<'i, Ast<AstStmt>> {
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_stmt()?;
    if let Some(span_left_tokens) = parser.span_left_tokens() {
        return Err(ParserError {
            span: span_left_tokens,
            kind: ParserErrorKind::NotEntirelyConsumed,
            hints: vec![],
        });
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
            Err(ParserError {
                span: self.span_eof(),
                kind: ParserErrorKind::UnexpectedEof { expects: None },
                hints: vec![],
            })
        }
    }

    pub fn next_token(&mut self) -> Result<'i, Token<'i>> {
        let res = self.lookahead(0);
        if !self.is_finished() {
            self.ptr += 1;
        }
        res
    }

    pub fn eat(&mut self, expect: TokenKind<'i>) -> Result<'i, Token> {
        match self.next_token() {
            Ok(token) if token.kind == expect => Ok(token),
            Ok(token) => Err(ParserError {
                span: token.span,
                kind: ParserErrorKind::UnexpectedToken {
                    token_kind: token.kind,
                    expects: Some(vec![expect]),
                },
                hints: vec![],
            }),
            Err(_) => Err(ParserError {
                span: self.span_eof(),
                kind: ParserErrorKind::UnexpectedEof {
                    expects: Some(vec![expect]),
                },
                hints: vec![],
            }),
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
            (token, _) => Err(ParserError {
                span: token.span,
                kind: ParserErrorKind::UnexpectedToken {
                    token_kind: token.kind,
                    expects: Some(vec![
                        TokenKind::If,
                        TokenKind::While,
                        TokenKind::Begin,
                        TokenKind::Begin,
                        TokenKind::Dump,
                        TokenKind::Ident(""),
                    ]),
                },
                hints: vec![],
            }),
        }
    }

    fn parse_funcdef_stmt(&mut self) -> Result<'i, Ast<AstFuncdefStmt>> {
        let start = self.eat(TokenKind::Function)?.span.start;
        let name = self.parse_ident()?;
        self.eat(TokenKind::OpenPar)?;
        let params = self.parse_param_list()?;
        self.eat(TokenKind::ClosePar)?;
        self.eat(TokenKind::Colon)?;
        let ret_ty = self.parse_ty()?;
        self.eat(TokenKind::Semicolon)?;
        let body = self.parse_begin_stmt()?;
        let end = body.span.end;

        Ok(AstFuncdefStmt::from_elements(
            Span::new(start, end),
            name,
            params,
            ret_ty,
            body,
        ))
    }

    fn parse_param_list(&mut self) -> Result<'i, Ast<AstParamList>> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (t, TokenKind::ClosePar) => Ok(Ast {
                ast: AstParamList::Empty,
                span: Span::new(t.span.start, t.span.start),
            }),
            _ => self.parse_nonempty_param_list(),
        }
    }

    fn parse_nonempty_param_list(&mut self) -> Result<'i, Ast<AstParamList>> {
        let ident = self.parse_ident()?;
        self.eat(TokenKind::Colon)?;
        let ty = self.parse_ty()?;

        let next = self.lookahead(0);
        let next = if next.as_ref().map(|t| t.kind).ok() == Some(TokenKind::Comma) {
            self.eat(TokenKind::Comma)?;
            self.parse_nonempty_param_list()?
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
            ty,
            next,
        ))
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
                // if there is last statement, give a hint to add a semicolon after the last
                // statement.
                if let Some(last) = list.ast.last_stmt() {
                    err.hints.push(Hint {
                        span: Some((last.span, "try adding semicolon after this".to_string())),
                        message: "parhaps you missed a semicolon".to_string(),
                    });
                }
                return Err(err);
            }
        };

        Ok(AstBeginStmt::from_list(Span::new(start, end), list))
    }

    fn parse_stmt_list(&mut self) -> Result<'i, Ast<AstStmtList>> {
        match self.lookahead(0).map(|t| (t, t.kind))? {
            (t, TokenKind::End) => Ok(Ast {
                ast: AstStmtList::Empty,
                span: Span::new(t.span.start, t.span.start),
            }),
            _ => self.parse_nonempty_stmt_list(),
        }
    }

    fn parse_nonempty_stmt_list(&mut self) -> Result<'i, Ast<AstStmtList>> {
        let stmt = self.parse_stmt()?;
        let start = stmt.span.start;
        let token = self.lookahead(0)?;

        let (next, end) = if token.kind == TokenKind::Semicolon {
            let semi_span = self.eat(TokenKind::Semicolon)?.span;
            // peek the next token for a trailing semicolon hint
            let next_is_end = self.lookahead(0).map(|t| t.kind).ok() == Some(TokenKind::End);
            let rest = match self.parse_nonempty_stmt_list() {
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
            (rest, end)
        } else {
            (
                AstStmtList::empty(Span::new(token.span.start, token.span.start)),
                stmt.span.end,
            )
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
        let start = self.eat(TokenKind::Dump)?.span.start;
        let var = self.parse_var()?;
        let end = var.span.end;

        Ok(AstDumpStmt::from_var(Span::new(start, end), var))
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
            (token, TokenKind::Lt) => Ok(Ast {
                ast: AstCompareOp::Lt,
                span: token.span,
            }),
            (token, TokenKind::Gt) => Ok(Ast {
                ast: AstCompareOp::Gt,
                span: token.span,
            }),
            (token, TokenKind::Le) => Ok(Ast {
                ast: AstCompareOp::Le,
                span: token.span,
            }),
            (token, TokenKind::Ge) => Ok(Ast {
                ast: AstCompareOp::Ge,
                span: token.span,
            }),
            (token, TokenKind::Eq) => Ok(Ast {
                ast: AstCompareOp::Eq,
                span: token.span,
            }),
            (token, TokenKind::Ne) => Ok(Ast {
                ast: AstCompareOp::Ne,
                span: token.span,
            }),
            (token, _) => Err(ParserError {
                span: token.span,
                kind: ParserErrorKind::UnexpectedToken {
                    token_kind: token.kind,
                    expects: Some(vec![
                        TokenKind::Lt,
                        TokenKind::Gt,
                        TokenKind::Le,
                        TokenKind::Ge,
                        TokenKind::Eq,
                        TokenKind::Ne,
                    ]),
                },
                hints: vec![],
            }),
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
            (token, _) => Err(ParserError {
                span: token.span,
                kind: ParserErrorKind::UnexpectedToken {
                    token_kind: token.kind,
                    expects: Some(vec![
                        TokenKind::OpenPar,
                        TokenKind::IntConst(0),
                        TokenKind::FloatConst(0.0),
                        TokenKind::Ident(""),
                    ]),
                },
                hints: vec![],
            }),
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
            _ => self.parse_nonempty_argument_list(),
        }
    }

    fn parse_nonempty_argument_list(&mut self) -> Result<'i, Ast<AstArgumentList>> {
        let expr = self.parse_arith_expr()?;
        let next = self.lookahead(0);
        let next = if next.as_ref().map(|t| t.kind).ok() == Some(TokenKind::Comma) {
            self.eat(TokenKind::Comma)?;
            self.parse_nonempty_argument_list()?
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

    fn parse_const(&mut self) -> Result<'i, Ast<AstConst>> {
        match self.next_token().map(|t| (t, t.kind))? {
            (token, TokenKind::IntConst(value)) => Ok(Ast {
                ast: AstConst::Int(value),
                span: token.span,
            }),
            (token, TokenKind::FloatConst(value)) => Ok(Ast {
                ast: AstConst::Float(value),
                span: token.span,
            }),
            (token, _) => Err(ParserError {
                span: token.span,
                kind: ParserErrorKind::UnexpectedToken {
                    token_kind: token.kind,
                    expects: Some(vec![TokenKind::IntConst(0), TokenKind::FloatConst(0.0)]),
                },
                hints: vec![],
            }),
        }
    }

    fn parse_var(&mut self) -> Result<'i, Ast<AstVar>> {
        self.parse_ident().map(AstVar::from_ident)
    }

    fn parse_ty(&mut self) -> Result<'i, Ast<AstTy>> {
        self.parse_ident().map(AstTy::from_ident)
    }

    fn parse_ident(&mut self) -> Result<'i, Ast<AstIdent>> {
        match self.next_token().map(|t| (t, t.kind))? {
            (token, TokenKind::Ident(ident)) => Ok(Ast {
                ast: AstIdent(ident.to_string()),
                span: token.span,
            }),
            (token, _) => Err(ParserError {
                span: token.span,
                kind: ParserErrorKind::UnexpectedToken {
                    token_kind: token.kind,
                    expects: Some(vec![TokenKind::Ident("")]),
                },
                hints: vec![],
            }),
        }
    }
}
