use double_peekable::DoublePeekable;
use ast::{Program, Function, Block, Statement, Expression, SCBinOpKind, BinOpKind, UnOpKind};
use source::Span;
use token::Token;

pub struct Parser<L: IntoIterator<Item = (Span, Token)>> {
    lexer: DoublePeekable<L::IntoIter>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Unexpected(String, Option<Span>),
}

macro_rules! expect { // we return the span for propagation
    ($lexer:expr, $p:pat, $expected:tt) => {
        match $lexer.next() {
            Some((span, $p)) => span,
            other => return make_unexpected!($expected, other)
        }
    };
}

macro_rules! match_peek_token {
    ($lexer:expr => $($p:pat)|+) => {
        {
            match $lexer.peek() {
                $(Some(&(_, $p)) => true),+,
                _ => false
            }
        }
    };
    ($lexer:expr => $p1:pat, $p2:pat) => {
        {
            match $lexer.peek_double() {
                Some((&(_, $p1), &(_, $p2))) => true,
                _ => false
            }
        }
    };
}

macro_rules! make_unexpected {
    ($expected:tt, $other:expr) => {
        Err(ParseError::Unexpected($expected.to_string(), $other.map(|(span, _)| span)))
    }
}

impl<L: IntoIterator<Item = (Span, Token)>> Parser<L> {
    pub fn new(lexer: L) -> Parser<L> {
        Parser { lexer: DoublePeekable::new(lexer.into_iter()) }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        // program = function_def* block

        let mut funcs = Vec::new();
        while self.lexer.peek().is_some() && match_peek_token!(self.lexer => Token::FnKw) {
            funcs.push(try!(self.parse_function_def()));
        }

        let block = try!(self.parse_block());
        let span = Span::merge(funcs.first().map(|func| func.span).unwrap_or(block.span),
                               block.span);

        let main_func = Function {
            name: String::from("main"),
            params: Vec::new(),
            span: block.span,
            block: block,
        };

        Ok(Program {
            functions: funcs,
            main_func: main_func,
            span: span,
        })
    }

    pub fn parse_function_def(&mut self) -> Result<Function, ParseError> {
        // function_def = "fn" IDENTIFIER '(' param_list ')' block
        let func_span = expect!(self.lexer, Token::FnKw, "fn");

        let func_name = match self.lexer.next() {
            Some((_, Token::Identifier(func_name))) => func_name,
            other => return make_unexpected!("identifier", other),
        };

        expect!(self.lexer, Token::LParen, "(");
        let mut param_names = Vec::new();
        if self.lexer.peek().is_some() && !match_peek_token!(self.lexer => Token::RParen) {
            param_names.push(match self.lexer.next() {
                Some((_, Token::Identifier(func_name))) => func_name,
                other => return make_unexpected!("identifier", other),
            });
            while self.lexer.peek().is_some() && !match_peek_token!(self.lexer => Token::RParen) {
                expect!(self.lexer, Token::Comma, ",");
                param_names.push(match self.lexer.next() {
                    Some((_, Token::Identifier(func_name))) => func_name,
                    other => return make_unexpected!("identifier", other),
                });
            }
        }
        expect!(self.lexer, Token::RParen, ")");
        let block = try!(self.parse_block());
        let span = Span::merge(func_span, block.span);

        Ok(Function {
            name: func_name,
            params: param_names,
            block: block,
            span: span,
        })
    }

    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        // block = '{' statement* '}'

        let begin_span = expect!(self.lexer, Token::LBrace, "{");

        let mut stmts = Vec::new();
        while self.lexer.peek().is_some() && !match_peek_token!(self.lexer => Token::RBrace) {
            stmts.push(try!(self.parse_statement()));
        }
        let end_span = expect!(self.lexer, Token::RBrace, "}");

        Ok(Block {
            stmts: stmts,
            span: Span::merge(begin_span, end_span),
        })
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // statement = print-statement
        //           | read-statement
        //           | if-statement
        //           | loop-statement
        //           | break-statement
        //           | assign-statement

        match self.lexer.peek() {
            Some(&(_, Token::IfKw)) => self.parse_if_statement(),
            Some(&(_, Token::LoopKw)) => self.parse_loop_statement(),
            Some(&(_, Token::WhileKw)) => self.parse_while_statement(),
            Some(&(_, Token::BreakKw)) => self.parse_break_statement(),
            Some(&(_, Token::ReturnKw)) => self.parse_return_statement(),
            Some(&(_, Token::LBrace)) => self.parse_block_statement(),
            _ => self.parse_statement_expression(),
        }
    }

    pub fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        // if-statement = "if" expression statement [ "else" statement ]

        let kw_span = expect!(self.lexer, Token::IfKw, "if");

        let condition = try!(self.parse_expression());
        let stmt = try!(self.parse_statement());
        let else_stmt = {
            if match_peek_token!(self.lexer => Token::ElseKw) {
                self.lexer.next();
                Some(try!(self.parse_statement()))
            } else {
                None
            }
        };
        let end_span = if let Some(ref else_stmt) = else_stmt {
            else_stmt.span()
        } else {
            stmt.span()
        };
        Ok(Statement::If {
            cond: condition,
            if_stmt: Box::new(stmt),
            else_stmt: else_stmt.map(Box::new),
            span: Span::merge(kw_span, end_span),
        })
    }

    pub fn parse_loop_statement(&mut self) -> Result<Statement, ParseError> {
        // loop-statement = "loop" statement

        let kw_span = expect!(self.lexer, Token::LoopKw, "loop");
        let stmt = try!(self.parse_statement());
        let span = Span::merge(kw_span, stmt.span());
        Ok(Statement::Loop {
            stmt: Box::new(stmt),
            span: span,
        })
    }

    pub fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        // while-statement = "while" expression statement

        let kw_span = expect!(self.lexer, Token::WhileKw, "while");
        let condition = try!(self.parse_expression());
        let stmt = try!(self.parse_statement());
        let span = Span::merge(kw_span, stmt.span());
        Ok(Statement::While {
            cond: condition,
            stmt: Box::new(stmt),
            span: span,
        })
    }

    pub fn parse_break_statement(&mut self) -> Result<Statement, ParseError> {
        // break-statement = "break" ";"

        let kw_span = expect!(self.lexer, Token::BreakKw, "break");
        let end_span = expect!(self.lexer, Token::SemiColon, ";");
        let span = Span::merge(kw_span, end_span);
        Ok(Statement::Break { span: span })
    }

    pub fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        // return-statement = "return" expression ";"

        let kw_span = expect!(self.lexer, Token::ReturnKw, "return");
        let expr = try!(self.parse_expression());
        let end_span = expect!(self.lexer, Token::SemiColon, ";");
        let span = Span::merge(kw_span, end_span);
        Ok(Statement::Return {
            expr: expr,
            span: span,
        })
    }

    pub fn parse_block_statement(&mut self) -> Result<Statement, ParseError> {
        let block = try!(self.parse_block());
        Ok(Statement::Block {
            span: block.span,
            block: block,
        })
    }

    pub fn parse_statement_expression(&mut self) -> Result<Statement, ParseError> {
        let expr = try!(self.parse_expression());
        let end_span = expect!(self.lexer, Token::SemiColon, ";");
        let span = Span::merge(expr.span(), end_span);
        Ok(Statement::Expression {
            expr: expr,
            span: span,
        })
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // expression = IDENTIFIER '=' expression
        //            | logical_or

        if self.lexer.peek().is_some() &&
           match_peek_token!(self.lexer => Token::Identifier(_), Token::AssignOp) {
            let (begin_span, id) = match self.lexer.next() {
                Some((span, Token::Identifier(id))) => (span, id),
                _ => unreachable!(),
            };
            expect!(self.lexer, Token::AssignOp, "=");
            let value = try!(self.parse_expression());
            let span = Span::merge(begin_span, value.span());
            Ok(Expression::Assign {
                id: id,
                value: Box::new(value),
                span: span,
            })
        } else {
            self.parse_logical_or()
        }
    }

    pub fn parse_logical_or(&mut self) -> Result<Expression, ParseError> {
        // logical_or = logical_and { "||" logical_and }
        let mut lhs = try!(self.parse_logical_and());
        while self.lexer.peek().is_some() && match_peek_token!(self.lexer => Token::LogOrOp) {
            self.lexer.next();
            let rhs = try!(self.parse_logical_and());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = Expression::SCBinOp {
                kind: SCBinOpKind::LogicalOr,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
            };
        }
        Ok(lhs)
    }

    pub fn parse_logical_and(&mut self) -> Result<Expression, ParseError> {
        // logical_and = eq_comp { "&&" eq_comp }
        let mut lhs = try!(self.parse_eq_comp());
        while self.lexer.peek().is_some() && match_peek_token!(self.lexer => Token::LogAndOp) {
            self.lexer.next();
            let rhs = try!(self.parse_eq_comp());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = Expression::SCBinOp {
                kind: SCBinOpKind::LogicalAnd,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
            };
        }
        Ok(lhs)
    }

    pub fn parse_eq_comp(&mut self) -> Result<Expression, ParseError> {
        // eq_comp = ord_comp [ (==|!=) ord_comp ]

        let mut lhs = try!(self.parse_ord_comp());
        if self.lexer.peek().is_some() &&
           match_peek_token!(self.lexer => Token::EqualOp | Token::NotEqualOp) {
            let binop_kind = match self.lexer.next() {
                Some((_, Token::EqualOp)) => BinOpKind::Equal,
                Some((_, Token::NotEqualOp)) => BinOpKind::NotEqual,
                _ => unreachable!(),
            };
            let rhs = try!(self.parse_ord_comp());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = Expression::BinOp {
                kind: binop_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
            };
        }
        Ok(lhs)
    }

    pub fn parse_ord_comp(&mut self) -> Result<Expression, ParseError> {
        // ord_comp = sum [ (<|<=|>|>=) sum ]

        let mut lhs = try!(self.parse_sum());
        if self.lexer.peek().is_some() &&
           match_peek_token!(self.lexer =>
                             Token::LessOp |
                             Token::LessEqualOp |
                             Token::GreaterOp |
                             Token::GreaterEqualOp) {
            let binop_kind = match self.lexer.next() {
                Some((_, Token::LessOp)) => BinOpKind::Less,
                Some((_, Token::LessEqualOp)) => BinOpKind::LessEq,
                Some((_, Token::GreaterOp)) => BinOpKind::Greater,
                Some((_, Token::GreaterEqualOp)) => BinOpKind::GreaterEq,
                _ => unreachable!(),
            };
            let rhs = try!(self.parse_sum());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = Expression::BinOp {
                kind: binop_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
            };
        }
        Ok(lhs)
    }

    pub fn parse_sum(&mut self) -> Result<Expression, ParseError> {
        // sum = factor { [+|-] factor }

        let mut lhs = try!(self.parse_factor());
        while self.lexer.peek().is_some() &&
              match_peek_token!(self.lexer => Token::PlusOp | Token::MinusOp) {
            let binop_kind = match self.lexer.next() {
                Some((_, Token::PlusOp)) => BinOpKind::Add,
                Some((_, Token::MinusOp)) => BinOpKind::Sub,
                _ => unreachable!(),
            };

            let rhs = try!(self.parse_factor());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = Expression::BinOp {
                kind: binop_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
            };
        }
        Ok(lhs)
    }

    pub fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        // factor = term { [*|/|%] term }

        let mut lhs = try!(self.parse_term());
        while self.lexer.peek().is_some() &&
              match_peek_token!(self.lexer => Token::TimesOp | Token::DivideOp | Token::ModOp) {
            let binop_kind = match self.lexer.next() {
                Some((_, Token::TimesOp)) => BinOpKind::Mul,
                Some((_, Token::DivideOp)) => BinOpKind::Div,
                Some((_, Token::ModOp)) => BinOpKind::Mod,
                _ => unreachable!(),
            };

            let rhs = try!(self.parse_term());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = Expression::BinOp {
                kind: binop_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
            };
        }
        Ok(lhs)
    }

    pub fn parse_term(&mut self) -> Result<Expression, ParseError> {
        // term = IDENTIFIER
        //      | NUMBER
        //      | [+|-] term
        //      | '(' expression ')'

        let term = match self.lexer.next() {
            Some((span, Token::Identifier(id))) => {
                if match_peek_token!(self.lexer => Token::LParen) {
                    self.lexer.next();
                    let mut args = Vec::new();
                    if self.lexer.peek().is_some() &&
                       !match_peek_token!(self.lexer => Token::RParen) {
                        args.push(try!(self.parse_expression()));
                        while self.lexer.peek().is_some() &&
                              !match_peek_token!(self.lexer => Token::RParen) {
                            expect!(self.lexer, Token::Comma, ",");
                            args.push(try!(self.parse_expression()));
                        }
                    }
                    let end_span = expect!(self.lexer, Token::RParen, ")");
                    Expression::FuncCall {
                        func_name: id,
                        args: args,
                        span: Span::merge(span, end_span),
                    }
                } else {
                    Expression::Identifier {
                        id: id,
                        span: span,
                    }
                }
            }
            Some((span, Token::Number(value))) => {
                Expression::Number {
                    value: value,
                    span: span,
                }
            }
            Some((span, Token::LParen)) => {
                let expr = try!(self.parse_expression());
                let end_span = expect!(self.lexer, Token::RParen, ")");
                Expression::Paren {
                    expr: Box::new(expr),
                    span: Span::merge(span, end_span),
                }
            }
            Some((span, Token::LogNotOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                Expression::UnOp {
                    kind: UnOpKind::LogNot,
                    expr: Box::new(expr),
                    span: span,
                }
            }
            Some((span, Token::PlusOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                Expression::UnOp {
                    kind: UnOpKind::Plus,
                    expr: Box::new(expr),
                    span: span,
                }
            }
            Some((span, Token::MinusOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                Expression::UnOp {
                    kind: UnOpKind::Minus,
                    expr: Box::new(expr),
                    span: span,
                }
            }
            other => return make_unexpected!("identifier, number, (, +, -", other),
        };
        Ok(term)
    }
}
