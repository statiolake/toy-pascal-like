use crate::ast::*;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Copy)]
pub struct Parser<'a> {
    input: &'a [Token<'a>],
    ptr: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [Token<'a>]) -> Self {
        Self { input, ptr: 0 }
    }

    pub fn is_finished(&self) -> bool {
        self.ptr == self.input.len()
    }

    pub fn lookahead(&self, n: usize) -> Option<Token<'a>> {
        if self.ptr + n < self.input.len() {
            Some(self.input[self.ptr + n])
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let res = self.lookahead(0);
        if !self.is_finished() {
            self.ptr += 1;
        }

        res
    }

    #[track_caller]
    pub fn eat(&mut self, expect: TokenKind) {
        match self.next_token() {
            Some(token) if token.kind == expect => {}
            other => panic!("eat failed: expected {:?}, found {:?}", expect, other),
        };
    }
}

impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> AstStmt {
        match self
            .lookahead(0)
            .map(|t| (t, t.kind))
            .expect("unexpected EOF while reading stmt")
        {
            (_, TokenKind::If) => AstStmt::IfStmt(Box::new(self.parse_if_stmt())),
            (_, TokenKind::While) => AstStmt::WhileStmt(Box::new(self.parse_while_stmt())),
            (_, TokenKind::Begin) => AstStmt::BeginStmt(Box::new(self.parse_begin_stmt())),
            (_, TokenKind::Dump) => AstStmt::DumpStmt(Box::new(self.parse_dump_stmt())),
            (_, TokenKind::Ident(_)) => AstStmt::AssgStmt(Box::new(self.parse_assg_stmt())),
            (token, _) => panic!("unexpected token {:?} while reading stmt", token),
        }
    }

    fn parse_if_stmt(&mut self) -> AstIfStmt {
        self.eat(TokenKind::If);
        let cond = Box::new(self.parse_bool_expr());
        self.eat(TokenKind::Then);
        let then = Box::new(self.parse_stmt());
        self.eat(TokenKind::Else);
        let otherwise = Box::new(self.parse_stmt());

        AstIfStmt {
            cond,
            then,
            otherwise,
        }
    }

    fn parse_while_stmt(&mut self) -> AstWhileStmt {
        self.eat(TokenKind::While);
        let cond = Box::new(self.parse_bool_expr());
        self.eat(TokenKind::Do);
        let body = Box::new(self.parse_stmt());

        AstWhileStmt { cond, body }
    }

    fn parse_begin_stmt(&mut self) -> AstBeginStmt {
        self.eat(TokenKind::Begin);
        let list = Box::new(self.parse_stmt_list());
        self.eat(TokenKind::End);

        AstBeginStmt { list }
    }

    fn parse_stmt_list(&mut self) -> AstStmtList {
        let stmt = Box::new(self.parse_stmt());
        let next = self
            .lookahead(0)
            .filter(|t| t.kind == TokenKind::Semicolon)
            .map(|_| {
                self.eat(TokenKind::Semicolon);
                Box::new(self.parse_stmt_list())
            });

        AstStmtList { stmt, next }
    }

    fn parse_assg_stmt(&mut self) -> AstAssgStmt {
        let var = Box::new(self.parse_var());
        self.eat(TokenKind::AssgEqual);
        let expr = Box::new(self.parse_arith_expr());

        AstAssgStmt { var, expr }
    }

    fn parse_dump_stmt(&mut self) -> AstDumpStmt {
        self.eat(TokenKind::Dump);
        let var = Box::new(self.parse_var());

        AstDumpStmt { var }
    }

    fn parse_bool_expr(&mut self) -> AstBoolExpr {
        let lhs = Box::new(self.parse_arith_expr());
        let op = Box::new(self.parse_compare_op());
        let rhs = Box::new(self.parse_arith_expr());

        AstBoolExpr { lhs, op, rhs }
    }

    fn parse_compare_op(&mut self) -> AstCompareOp {
        match self
            .next_token()
            .expect("unexpected EOF while reading compare-op")
            .kind
        {
            TokenKind::Lt => AstCompareOp::Lt,
            TokenKind::Gt => AstCompareOp::Gt,
            TokenKind::Le => AstCompareOp::Le,
            TokenKind::Ge => AstCompareOp::Ge,
            TokenKind::Eq => AstCompareOp::Eq,
            TokenKind::Ne => AstCompareOp::Ne,
            other => panic!("unexpected token '{:?}' for compare-op", other),
        }
    }

    fn parse_arith_expr(&mut self) -> AstArithExpr {
        match self
            .lookahead(0)
            .map(|t| (t, t.kind))
            .expect("unexpected EOF while reading arith-expr")
        {
            (_, TokenKind::OpenPar) => {
                self.eat(TokenKind::OpenPar);
                let lhs = Box::new(self.parse_arith_expr());
                let op = Box::new(self.parse_arith_op());
                let rhs = Box::new(self.parse_arith_expr());
                self.eat(TokenKind::ClosePar);
                AstArithExpr::Op { lhs, op, rhs }
            }
            (_, TokenKind::Number(_)) => {
                let value = Box::new(self.parse_const());
                AstArithExpr::Const(value)
            }
            (_, TokenKind::Ident(_)) => {
                // peek next identifiers to determine whether it's fncall or variable, look one
                // token ahead.
                if self.lookahead(1).map(|t| t.kind) == Some(TokenKind::OpenPar) {
                    // fncall
                    let fncall = Box::new(self.parse_fncall());
                    AstArithExpr::FnCall(fncall)
                } else {
                    let var = Box::new(self.parse_var());
                    AstArithExpr::Var(var)
                }
            }
            (token, _) => panic!("unexpected token {:?} for arith-expr", token),
        }
    }

    fn parse_fncall(&mut self) -> AstFnCall {
        let ident = Box::new(self.parse_ident());
        self.eat(TokenKind::OpenPar);
        let args = Box::new(self.parse_argument_list());
        self.eat(TokenKind::ClosePar);

        AstFnCall { ident, args }
    }

    fn parse_argument_list(&mut self) -> AstArgumentList {
        match self
            .lookahead(0)
            .map(|t| t.kind)
            .expect("unexpected EOF while reading argument-list")
        {
            TokenKind::ClosePar => AstArgumentList::Empty,
            _ => {
                let expr = Box::new(self.parse_arith_expr());
                let next = if self.lookahead(0).map(|t| t.kind) == Some(TokenKind::Comma) {
                    self.eat(TokenKind::Comma);
                    Box::new(self.parse_argument_list())
                } else {
                    Box::new(AstArgumentList::Empty)
                };

                AstArgumentList::Nonempty { expr, next }
            }
        }
    }

    fn parse_arith_op(&mut self) -> AstArithOp {
        match self
            .next_token()
            .map(|t| (t, t.kind))
            .expect("unexpected EOF while reading arith-op")
        {
            (_, TokenKind::Add) => AstArithOp::Add,
            (_, TokenKind::Sub) => AstArithOp::Sub,
            (_, TokenKind::Mul) => AstArithOp::Mul,
            (_, TokenKind::Div) => AstArithOp::Div,
            (token, _) => panic!("unknown arith-op {:?}", token),
        }
    }

    fn parse_const(&mut self) -> AstConst {
        let value = match self
            .next_token()
            .map(|t| (t, t.kind))
            .expect("unexpected EOF while reading const value")
        {
            (_, TokenKind::Number(value)) => value,
            (token, _) => panic!("unexpected token {:?} for const", token),
        };

        AstConst(value)
    }

    fn parse_var(&mut self) -> AstVar {
        AstVar(self.parse_ident())
    }

    fn parse_ident(&mut self) -> AstIdent {
        let ident = match self
            .next_token()
            .map(|t| (t, t.kind))
            .expect("unexpected EOF while reading var")
        {
            (_, TokenKind::Ident(ident)) => ident,
            (token, _) => panic!("unexpected token {:?} for var", token),
        };

        AstIdent(ident.to_string())
    }
}
