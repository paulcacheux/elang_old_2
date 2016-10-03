use double_peekable::DoublePeekable;
use source::Span;
use token::Token;
use ast::{Program, Type, AssignOpKind, BinOpKind, UnOpKind, Function, Param, Block, Statement, Expression};
use ast::sema::Sema;

pub struct Parser<'a, L: IntoIterator<Item = (Span, Token)>> {
    lexer: DoublePeekable<L::IntoIter>,
    sema: Sema<'a>,
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

impl<'a, L: IntoIterator<Item = (Span, Token)>> Parser<'a, L> {
    pub fn new(lexer: L, sema: Sema<'a>) -> Parser<'a, L> {
        let mut parser = Parser {
            lexer: DoublePeekable::new(lexer.into_iter()),
            sema: sema,
        };
        parser.sema.symbol_table.begin_scope();
        parser.sema.add_io_funcs();
        parser
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        // program = function_def*

        let mut funcs = Vec::new();
        while self.lexer.peek().is_some() && match_peek_token!(self.lexer => Token::FnKw) {
            funcs.push(try!(self.parse_function_def()));
        }
        let span = if funcs.is_empty() {
            Span::new_with_len(0, 1)
        } else {
            Span::merge(funcs.first().map(|func| func.span).unwrap(),
                        funcs.last().map(|func| func.span).unwrap())
        };

        Ok(Program {
            functions: funcs,
            span: span,
        })
    }

    pub fn parse_type(&mut self) -> Result<Type, ParseError> {
        // type = IDENTIFIER
        //      | '*' type
        //      | "()" # unit type

        match self.lexer.next() {
            Some((_, Token::TimesOp)) => {
                let sub_ty = try!(self.parse_type());
                Ok(Type::Ptr(Box::new(sub_ty)))
            }
            Some((_, Token::LParen)) => {
                expect!(self.lexer, Token::RParen, ")");
                Ok(Type::Unit)
            }
            Some((span, Token::Identifier(id))) => Ok(self.sema.sema_id_type(id, span)),
            other => return make_unexpected!("identifier, &, (", other),
        }
    }

    pub fn parse_function_def(&mut self) -> Result<Function, ParseError> {
        // function_def = "fn" IDENTIFIER '(' param_list ')' "->" type block

        let func_span = expect!(self.lexer, Token::FnKw, "fn");

        let (name_span, func_name) = match self.lexer.next() {
            Some((span, Token::Identifier(func_name))) => (span, func_name),
            other => return make_unexpected!("identifier", other),
        };

        expect!(self.lexer, Token::LParen, "(");
        let params = try!(self.parse_param_list());
        expect!(self.lexer, Token::RParen, ")");

        let ret_ty = if match_peek_token!(self.lexer => Token::Arrow) {
            self.lexer.next();
            try!(self.parse_type())
        } else {
            Type::Unit
        };

        let func_info = self.sema.sema_function_def_begin(func_name, params, ret_ty, name_span);

        let block = try!(self.parse_block());
        let span = Span::merge(func_span, block.span);

        Ok(self.sema.sema_function_def_end(func_info, block, span))
    }

    pub fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
        // param_list = [ param { ',' param } ]
        let mut params = Vec::new();

        // Be carefull to the dependecy on Token::RParen !!
        if self.lexer.peek().is_some() && !match_peek_token!(self.lexer => Token::RParen) {
            params.push(try!(self.parse_param()));
            while self.lexer.peek().is_some() && !match_peek_token!(self.lexer => Token::RParen) {
                expect!(self.lexer, Token::Comma, ",");
                params.push(try!(self.parse_param()));
            }
        }
        Ok(params)
    }

    pub fn parse_param(&mut self) -> Result<Param, ParseError> {
        // param = IDENTIFIER ':' type
        let (span, name) = match self.lexer.next() {
            Some((span, Token::Identifier(name))) => (span, name),
            other => return make_unexpected!("identifier", other),
        };
        expect!(self.lexer, Token::Colon, ":");
        let ty = try!(self.parse_type());
        Ok(Param {
            name: name,
            ty: ty,
            span: span,
        })
    }

    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        // block = '{' statement* '}'

        let begin_span = expect!(self.lexer, Token::LBrace, "{");
        self.sema.symbol_table.begin_scope();

        let mut stmts = Vec::new();
        while self.lexer.peek().is_some() && !match_peek_token!(self.lexer => Token::RBrace) {
            stmts.push(try!(self.parse_statement()));
        }

        let end_span = expect!(self.lexer, Token::RBrace, "}");
        self.sema.symbol_table.end_scope();

        Ok(Block {
            statements: stmts,
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
            Some(&(_, Token::LetKw)) => self.parse_let_statement(),
            Some(&(_, Token::IfKw)) => self.parse_if_statement(),
            Some(&(_, Token::LoopKw)) => self.parse_loop_statement(),
            Some(&(_, Token::WhileKw)) => self.parse_while_statement(),
            Some(&(_, Token::BreakKw)) => self.parse_break_statement(),
            Some(&(_, Token::ContinueKw)) => self.parse_continue_statement(),
            Some(&(_, Token::ReturnKw)) => self.parse_return_statement(),
            Some(&(_, Token::LBrace)) => self.parse_block_statement(),
            _ => self.parse_statement_expression(),
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        // let-statement = "let" IDENTIFIER "=" expression ";"

        let kw_span = expect!(self.lexer, Token::LetKw, "let");

        let id = match self.lexer.next() {
            Some((_, Token::Identifier(id))) => id,
            other => return make_unexpected!("identifier", other),
        };

        let ty = if match_peek_token!(self.lexer => Token::Colon) {
            self.lexer.next();
            Some(try!(self.parse_type()))
        } else {
            None
        };

        expect!(self.lexer, Token::AssignOp, "=");
        let expr = try!(self.parse_expression());
        let end_span = expect!(self.lexer, Token::SemiColon, ";");


        Ok(self.sema.sema_let_stmt(id, ty, expr, Span::merge(kw_span, end_span)))
    }

    pub fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        // if-statement = "if" expression statement [ "else" statement ]

        let kw_span = expect!(self.lexer, Token::IfKw, "if");
        let condition = try!(self.parse_expression());

        // if stmt
        self.sema.symbol_table.begin_scope();
        let if_stmt = try!(self.parse_statement());
        self.sema.symbol_table.end_scope();


        // else stmt
        self.sema.symbol_table.begin_scope();
        let else_stmt = {
            if match_peek_token!(self.lexer => Token::ElseKw) {
                self.lexer.next();
                Some(try!(self.parse_statement()))
            } else {
                None
            }
        };
        self.sema.symbol_table.end_scope();


        let end_span = if let Some(ref else_stmt) = else_stmt {
            else_stmt.span()
        } else {
            if_stmt.span()
        };

        Ok(self.sema.sema_if_stmt(condition,
                                  if_stmt,
                                  else_stmt,
                                  Span::merge(kw_span, end_span)))
    }

    pub fn parse_loop_statement(&mut self) -> Result<Statement, ParseError> {
        // loop-statement = "loop" statement

        let kw_span = expect!(self.lexer, Token::LoopKw, "loop");

        // loop stmt
        self.sema.symbol_table.begin_scope();
        self.sema.loop_level += 1;
        let stmt = try!(self.parse_statement());
        self.sema.symbol_table.end_scope();
        self.sema.loop_level -= 1;


        let span = Span::merge(kw_span, stmt.span());
        Ok(Statement::Loop {
            statement: Box::new(stmt),
            span: span,
        })
    }

    pub fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        // while-statement = "while" expression statement

        let kw_span = expect!(self.lexer, Token::WhileKw, "while");
        let condition = try!(self.parse_expression());

        // while stmt
        self.sema.symbol_table.begin_scope();
        self.sema.loop_level += 1;
        let stmt = try!(self.parse_statement());
        self.sema.symbol_table.end_scope();
        self.sema.loop_level -= 1;

        let span = Span::merge(kw_span, stmt.span());
        Ok(self.sema.sema_while_stmt(condition, stmt, span))
    }

    pub fn parse_break_statement(&mut self) -> Result<Statement, ParseError> {
        // break-statement = "break" ";"

        let kw_span = expect!(self.lexer, Token::BreakKw, "break");
        let end_span = expect!(self.lexer, Token::SemiColon, ";");
        let span = Span::merge(kw_span, end_span);
        Ok(self.sema.sema_break_stmt(span))
    }

    pub fn parse_continue_statement(&mut self) -> Result<Statement, ParseError> {
        // continue-statement = "continue" ";"

        let kw_span = expect!(self.lexer, Token::ContinueKw, "continue");
        let end_span = expect!(self.lexer, Token::SemiColon, ";");
        let span = Span::merge(kw_span, end_span);
        Ok(self.sema.sema_continue_stmt(span))
    }

    pub fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        // return-statement = "return" [ expression ] ";"

        let kw_span = expect!(self.lexer, Token::ReturnKw, "return");
        let expr = if match_peek_token!(self.lexer => Token::SemiColon) {
            Expression::UnitLit { span: kw_span }
        } else {
            try!(self.parse_expression())
        };

        let end_span = expect!(self.lexer, Token::SemiColon, ";");
        let span = Span::merge(kw_span, end_span);

        Ok(self.sema.sema_return_stmt(expr, span))
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
            expression: expr,
            span: span,
        })
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // expression = logical_or [ assign-op expression ]

        let mut lhs = try!(self.parse_logical_or());
        if self.lexer.peek().is_some() && match_peek_token!(self.lexer =>
            Token::AssignOp |
            Token::AssignPlusOp |
            Token::AssignMinusOp |
            Token::AssignTimesOp |
            Token::AssignDivOp |
            Token::AssignModOp
        ) {
            let assignop_kind = match self.lexer.next() {
                Some((_, Token::AssignOp)) => AssignOpKind::Normal,
                Some((_, Token::AssignPlusOp)) => AssignOpKind::Add,
                Some((_, Token::AssignMinusOp)) => AssignOpKind::Sub,
                Some((_, Token::AssignTimesOp)) => AssignOpKind::Mul,
                Some((_, Token::AssignDivOp)) => AssignOpKind::Div,
                Some((_, Token::AssignModOp)) => AssignOpKind::Mod,
                _ => unreachable!(),
            };
            let rhs = try!(self.parse_expression());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = self.sema.sema_assignop_expr(assignop_kind, lhs, rhs, span);
        }
        Ok(lhs)
    }

    pub fn parse_logical_or(&mut self) -> Result<Expression, ParseError> {
        // logical_or = logical_and { "||" logical_and }
        let mut lhs = try!(self.parse_logical_and());
        while self.lexer.peek().is_some() && match_peek_token!(self.lexer => Token::LogOrOp) {
            self.lexer.next();
            let rhs = try!(self.parse_logical_and());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = self.sema.sema_binop_expr(BinOpKind::LogicalOr, lhs, rhs, span);
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
            lhs = self.sema.sema_binop_expr(BinOpKind::LogicalAnd, lhs, rhs, span);
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
            lhs = self.sema.sema_binop_expr(binop_kind, lhs, rhs, span);
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
            lhs = self.sema.sema_binop_expr(binop_kind, lhs, rhs, span);
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
            lhs = self.sema.sema_binop_expr(binop_kind, lhs, rhs, span);
        }
        Ok(lhs)
    }

    pub fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        // factor = cast { [*|/|%] cast }

        let mut lhs = try!(self.parse_cast());
        while self.lexer.peek().is_some() &&
              match_peek_token!(self.lexer => Token::TimesOp | Token::DivOp | Token::ModOp) {
            let binop_kind = match self.lexer.next() {
                Some((_, Token::TimesOp)) => BinOpKind::Mul,
                Some((_, Token::DivOp)) => BinOpKind::Div,
                Some((_, Token::ModOp)) => BinOpKind::Mod,
                _ => unreachable!(),
            };

            let rhs = try!(self.parse_cast());
            let span = Span::merge(lhs.span(), rhs.span());
            lhs = self.sema.sema_binop_expr(binop_kind, lhs, rhs, span);
        }
        Ok(lhs)
    }

    pub fn parse_cast(&mut self) -> Result<Expression, ParseError> {
        // cast = term [ "as" type ]

        let mut lhs = try!(self.parse_term());
        if match_peek_token!(self.lexer => Token::AsKw) {
            let as_span = expect!(self.lexer, Token::AsKw, "as");
            let ty = try!(self.parse_type());

            let span = Span::merge(lhs.span(), as_span);
            lhs = self.sema.sema_cast_expr(lhs, ty, span);
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
                    self.sema.sema_func_call(id, args, Span::merge(span, end_span))
                } else {
                    self.sema.sema_identifier(id, span)
                }
            }
            Some((span, Token::IntLit(value))) => {
                Expression::IntLit {
                    value: value,
                    span: span,
                }
            }
            Some((span, Token::UIntLit(value))) => {
                Expression::UIntLit {
                    value: value,
                    span: span,
                }
            }
            Some((span, Token::DoubleLit(value))) => {
                Expression::DoubleLit {
                    value: value,
                    span: span,
                }
            }
            Some((span, Token::CharLit(value))) => {
                Expression::CharLit {
                    value: value,
                    span: span,
                }
            }
            Some((span, Token::BoolLit(value))) => {
                Expression::BoolLit {
                    value: value,
                    span: span,
                }
            }
            Some((span, Token::LParen)) => {
                let expr = try!(self.parse_expression());
                let expr_ty = expr.ty();
                let end_span = expect!(self.lexer, Token::RParen, ")");
                Expression::Paren {
                    expression: Box::new(expr),
                    span: Span::merge(span, end_span),
                    ty: expr_ty,
                }
            }
            Some((span, Token::LogNotOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                self.sema.sema_unop_expr(UnOpKind::LogicalNot, expr, span)
            }
            Some((span, Token::PlusOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                self.sema.sema_unop_expr(UnOpKind::Plus, expr, span)
            }
            Some((span, Token::MinusOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                self.sema.sema_unop_expr(UnOpKind::Minus, expr, span)
            }
            Some((span, Token::AmpOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                self.sema.sema_unop_expr(UnOpKind::AddressOf, expr, span)
            }
            Some((span, Token::TimesOp)) => {
                let expr = try!(self.parse_term());
                let span = Span::merge(span, expr.span());
                self.sema.sema_unop_expr(UnOpKind::Deref, expr, span)
            }
            other => return make_unexpected!("identifier, literal, (, +, -, *, &", other),
        };
        Ok(term)
    }
}
