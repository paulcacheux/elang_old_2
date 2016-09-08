use std::iter::Peekable;
use std::fmt;

use ast::{Program, Statement, Expression, BinOpKind, UnOpKind};
use source::Span;
use token::Token;

pub struct Parser<L: IntoIterator<Item = (Span, Token)>> {
    lexer: Peekable<L::IntoIter>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Unexpected(String, Option<Span>),
}

macro_rules! expect { // we return the span for propagation
    ($lexer:expr, $p:pat, $expected:tt) => {
        match $lexer.next() {
            Some((span, $p)) => span,
            other => return_unexpected!($expected, other)
        }
    };
}

macro_rules! match_peek_token {
    ($lexer:expr, $($p:pat),+) => {
        {
            match $lexer.peek() {
                $(Some(&(_, $p)) => true),+,
                _ => false
            }
        }
    }
}

macro_rules! return_unexpected {
    ($expected:tt, $other:expr) => {
        return Err(ParseError::Unexpected($expected.to_string(), $other.map(|(span, _)| span)))
    }
}

impl<L: IntoIterator<Item = (Span, Token)>> Parser<L> {
    pub fn new(lexer: L) -> Parser<L> {
        Parser { lexer: lexer.into_iter().peekable() }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        // program = BEGIN statement* END

        let begin_span = expect!(self.lexer, Token::BeginKw, "BEGIN");

        let mut statements = Vec::new();
        while self.lexer.peek().is_some() && !match_peek_token!(self.lexer, Token::EndKw) {
            statements.push(try!(self.parse_statement()));
        }

        let end_span = expect!(self.lexer, Token::EndKw, "END");

        Ok(Program {
            stmts: statements,
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

        match self.lexer.peek().cloned() {
            Some((_, Token::PrintKw)) => self.parse_print_statement(),
            Some((_, Token::ReadKw)) => self.parse_read_statement(),
            Some((_, Token::IfKw(_))) => self.parse_if_statement(),
            Some((_, Token::LoopKw)) => self.parse_loop_statement(),
            Some((_, Token::BreakKw)) => self.parse_break_statement(),
            Some((_, Token::Identifier(_))) => self.parse_assign_statement(),
            other => {
                return_unexpected!("PRINT, READ, IFN, IFP, IFZ, LOOP, BREAK, identifier", other)
            }
        }
    }

    pub fn parse_print_statement(&mut self) -> Result<Statement, ParseError> {
        // print-statement = PRINT expression

        let kw_span = expect!(self.lexer, Token::PrintKw, "PRINT");
        let expr = try!(self.parse_expression());
        let expr_span = expr.span();
        Ok(Statement::Print {
            expr: expr,
            span: Span::merge(kw_span, expr_span),
        })
    }

    pub fn parse_read_statement(&mut self) -> Result<Statement, ParseError> {
        // read-statement = READ IDENTIFIER

        let kw_span = expect!(self.lexer, Token::ReadKw, "READ");
        let (span, id) = match self.lexer.next() {
            Some((span, Token::Identifier(id))) => (span, id),
            other => return_unexpected!("identifier", other),
        };
        Ok(Statement::Read {
            target_id: id,
            span: Span::merge(kw_span, span),
        })
    }

    pub fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        // if-statement = ifkw expression statement* [ ELSE statement* ] END

        let (kw_span, if_kind) = match self.lexer.next() {
            Some((span, Token::IfKw(kind))) => (span, kind),
            other => return_unexpected!("IFN, IFP or IFZ", other),
        };

        let condition = try!(self.parse_expression());
        let mut if_statements = Vec::new();
        while self.lexer.peek().is_some() &&
              !match_peek_token!(self.lexer, Token::ElseKw, Token::EndKw) {
            if_statements.push(try!(self.parse_statement()));
        }
        let else_statements = {
            if match_peek_token!(self.lexer, Token::ElseKw) {
                self.lexer.next();
                let mut stmts = Vec::new();
                while self.lexer.peek().is_some() && !match_peek_token!(self.lexer, Token::EndKw) {
                    stmts.push(try!(self.parse_statement()));
                }
                Some(stmts)
            } else {
                None
            }
        };
        let end_span = expect!(self.lexer, Token::EndKw, "END");
        Ok(Statement::If {
            kind: if_kind,
            cond: condition,
            if_stmts: if_statements,
            else_stmts: else_statements,
            span: Span::merge(kw_span, end_span),
        })
    }

    pub fn parse_loop_statement(&mut self) -> Result<Statement, ParseError> {
        // loop-statement = LOOP statement* END

        let kw_span = expect!(self.lexer, Token::LoopKw, "LOOP");
        let mut statements = Vec::new();
        while self.lexer.peek().is_some() && !match_peek_token!(self.lexer, Token::EndKw) {
            statements.push(try!(self.parse_statement()));
        }
        let end_span = expect!(self.lexer, Token::EndKw, "END");
        Ok(Statement::Loop {
            stmts: statements,
            span: Span::merge(kw_span, end_span),
        })
    }

    pub fn parse_break_statement(&mut self) -> Result<Statement, ParseError> {
        // break-statement = BREAK

        let kw_span = expect!(self.lexer, Token::BreakKw, "BREAK");
        Ok(Statement::Break { span: kw_span })
    }

    pub fn parse_assign_statement(&mut self) -> Result<Statement, ParseError> {
        // assign-statement = IDENTIFIER '=' expression

        let (id_span, id) = match self.lexer.next() {
            Some((span, Token::Identifier(id))) => (span, id),
            other => return_unexpected!("identifier", other),
        };
        expect!(self.lexer, Token::AssignOp, "=");
        let value = try!(self.parse_expression());
        let expr_span = value.span();
        Ok(Statement::Assign {
            target_id: id,
            value: value,
            span: Span::merge(id_span, expr_span),
        })
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // expression = factor { [+|-] factor }

        let mut lhs = try!(self.parse_factor());
        while self.lexer.peek().is_some() &&
              match_peek_token!(self.lexer, Token::PlusOp, Token::MinusOp) {
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
              match_peek_token!(self.lexer, Token::TimesOp, Token::DivideOp, Token::ModOp) {
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
                Expression::Identifier {
                    id: id,
                    span: span,
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
            other => return_unexpected!("identifier, number, (, +, -", other),
        };
        Ok(term)
    }
}
