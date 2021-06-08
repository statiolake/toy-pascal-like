use crate::ast::*;
use crate::lexer::Token;

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

    pub fn peek(&self) -> Option<Token<'a>> {
        if !self.is_finished() {
            Some(self.input[self.ptr])
        } else {
            None
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        let res = self.peek();
        if !self.is_finished() {
            self.ptr += 1;
        }

        res
    }

    #[track_caller]
    pub fn eat(&mut self, expect: Token) {
        assert_eq!(self.next(), Some(expect), "Eat failed");
    }
}

impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> AstStmt {
        match self.peek() {
            Some(Token::If) => AstStmt::IfStmt(Box::new(self.parse_if_stmt())),
            Some(Token::While) => AstStmt::WhileStmt(Box::new(self.parse_while_stmt())),
            Some(Token::Begin) => AstStmt::BeginStmt(Box::new(self.parse_begin_stmt())),
            Some(Token::Dump) => AstStmt::DumpStmt(Box::new(self.parse_dump_stmt())),
            _ => AstStmt::AssgStmt(Box::new(self.parse_assg_stmt())),
        }
    }

    fn parse_if_stmt(&mut self) -> AstIfStmt {
        self.eat(Token::If);
        let cond = Box::new(self.parse_bool_expr());
        self.eat(Token::Then);
        let then = Box::new(self.parse_stmt());
        self.eat(Token::Else);
        let otherwise = Box::new(self.parse_stmt());

        AstIfStmt {
            cond,
            then,
            otherwise,
        }
    }

    fn parse_while_stmt(&mut self) -> AstWhileStmt {
        self.eat(Token::While);
        let cond = Box::new(self.parse_bool_expr());
        self.eat(Token::Do);
        let body = Box::new(self.parse_stmt());

        AstWhileStmt { cond, body }
    }

    fn parse_begin_stmt(&mut self) -> AstBeginStmt {
        self.eat(Token::Begin);
        let list = Box::new(self.parse_stmt_list());
        self.eat(Token::End);

        AstBeginStmt { list }
    }

    fn parse_stmt_list(&mut self) -> AstStmtList {
        let stmt = Box::new(self.parse_stmt());
        let next = if self.peek() == Some(Token::Semicolon) {
            self.eat(Token::Semicolon);
            Some(Box::new(self.parse_stmt_list()))
        } else {
            None
        };

        AstStmtList { stmt, next }
    }

    fn parse_assg_stmt(&mut self) -> AstAssgStmt {
        let var = Box::new(self.parse_var());
        self.eat(Token::AssgEqual);
        let expr = Box::new(self.parse_arith_expr());

        AstAssgStmt { var, expr }
    }

    fn parse_dump_stmt(&mut self) -> AstDumpStmt {
        self.eat(Token::Dump);
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
            .next()
            .expect("unexpected EOF while reading compare-op")
        {
            Token::Lt => AstCompareOp::Lt,
            Token::Gt => AstCompareOp::Gt,
            Token::Le => AstCompareOp::Le,
            Token::Ge => AstCompareOp::Ge,
            Token::Eq => AstCompareOp::Eq,
            Token::Ne => AstCompareOp::Ne,
            other => panic!("unexpected token '{:?}' for compare-op", other),
        }
    }

    fn parse_arith_expr(&mut self) -> AstArithExpr {
        match self.peek() {
            Some(Token::OpenPar) => {
                self.eat(Token::OpenPar);
                let lhs = Box::new(self.parse_arith_expr());
                let op = Box::new(self.parse_arith_op());
                let rhs = Box::new(self.parse_arith_expr());
                self.eat(Token::ClosePar);
                AstArithExpr::Op { lhs, op, rhs }
            }
            Some(Token::Number(_)) => {
                let value = Box::new(self.parse_const());
                AstArithExpr::Const(value)
            }
            Some(Token::Ident(_)) => {
                let var = Box::new(self.parse_var());
                AstArithExpr::Var(var)
            }
            Some(other) => panic!("unexpected token {:?} for arith-expr", other),
            None => panic!("unexpected EOF while reading arith-expr"),
        }
    }

    fn parse_arith_op(&mut self) -> AstArithOp {
        match self.next().expect("unexpected EOF while reading arith-op") {
            Token::Add => AstArithOp::Add,
            Token::Sub => AstArithOp::Sub,
            Token::Mul => AstArithOp::Mul,
            Token::Div => AstArithOp::Div,
            other => panic!("unknown arith-op {:?}", other),
        }
    }

    fn parse_const(&mut self) -> AstConst {
        let token = self
            .next()
            .expect("unexpected EOF while reading const value");
        let value = match token {
            Token::Number(value) => value,
            other => panic!("unexpected token {:?} for const", other),
        };

        AstConst(value)
    }

    fn parse_var(&mut self) -> AstVar {
        let token = self.next().expect("unexpected EOF while reading var");
        let ident = match token {
            Token::Ident(ident) => ident,
            other => panic!("unexpected token {:?} for var", other),
        };

        AstVar(ident.to_string())
    }
}
