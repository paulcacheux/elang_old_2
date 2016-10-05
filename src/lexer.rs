use std::str::FromStr;

use double_peekable::DoublePeekable;
use token::Token;
use source::{Span, Reader};
use error::{LexError, LexErrorKind, CodeError};

fn identifier_or_keyword(raw: String, bytepos: usize) -> (Span, Token) {
    let span = Span::new_with_len(bytepos, raw.len());
    let token = match raw.as_str() {
        "as" => Token::AsKw,
        "fn" => Token::FnKw,
        "let" => Token::LetKw,
        "loop" => Token::LoopKw,
        "if" => Token::IfKw,
        "break" => Token::BreakKw,
        "continue" => Token::ContinueKw,
        "return" => Token::ReturnKw,
        "else" => Token::ElseKw,
        "while" => Token::WhileKw,
        "true" => Token::BoolLit(true),
        "false" => Token::BoolLit(false),
        _ => Token::Identifier(raw),
    };
    (span, token)
}

fn is_identifier_start(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '_' => true,
        _ => false,
    }
}

fn is_identifier_continue(c: char) -> bool {
    match c {
        '0'...'9' => true,
        c if is_identifier_start(c) => true,
        _ => false,
    }
}

pub struct Lexer<'a> {
    input: DoublePeekable<Reader<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: Reader<'a>) -> Lexer<'a> {
        Lexer {
            input: DoublePeekable::new(input),
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(&(_, c)) = self.input.peek() {
                if c.is_whitespace() {
                    self.input.next();
                    continue;
                }
            }

            if let Some((&(_, c1), &(_, c2))) = self.input.peek_double() {
                match (c1, c2) {
                    ('/', '/') => {
                        self.input.next();
                        self.input.next();
                        self.skip_to_endline();
                    }
                    ('/', '*') => {
                        self.input.next();
                        self.input.next();
                        self.skip_multicomment(1);
                    }
                    _ => return,
                }
            } else {
                return;
            }
        }
    }

    fn skip_to_endline(&mut self) {
        loop {
            match self.input.peek() {
                Some(&(_, '\n')) => {
                    self.input.next();
                    break;
                }
                Some(_) => {
                    self.input.next();
                    continue;
                }
                None => break,
            }
        }
    }

    fn skip_multicomment(&mut self, deep: usize) {
        if deep == 0 {
            return;
        }
        loop {
            match self.input.peek_double() {
                Some((&(_, '*'), &(_, '/'))) => {
                    self.skip_multicomment(deep - 1);
                }
                _ => {
                    self.input.next();
                }
            }
        }
    }

    fn if_next(&mut self,
               c: char,
               tok_true: Token,
               tok_false: Token,
               bytepos: usize)
               -> (Span, Token) {
        match self.input.peek() {
            Some(&(_, p)) if p == c => {
                self.input.next();
                (Span::new_with_len(bytepos, 2), tok_true)
            }
            _ => (Span::new_one(bytepos), tok_false),
        }
    }

    fn take_while<P>(&mut self, first: Option<char>, predicate: P) -> String
        where P: Fn(char) -> bool
    {
        let mut res = String::new();
        if let Some(first) = first {
            res.push(first);
        }

        loop {
            match self.input.peek() {
                Some(&(_, c)) if predicate(c) => res.push(c),
                _ => break,
            }
            self.input.next();
        }
        res
    }

    fn lex_digits(&mut self, first: Option<char>, base: u32) -> String {
        self.take_while(first, |c| c.is_digit(base))
    }

    fn lex_number_lit(&mut self, first: char, bytepos: usize) -> (Span, Token) {
        #[derive(Debug, Clone, PartialEq, Eq)]
        enum Kind {
            Int,
            UInt,
            Double
        }

        let mut number = self.lex_digits(Some(first), 10);
        let mut kind = Kind::Int;
        if let Some(&(_, '.')) = self.input.peek() {
            self.input.next();
            kind = Kind::Double;

            number.push('.');
            number.push_str(&self.lex_digits(None, 10));
        }
        if let Some(&(_, c)) = self.input.peek() {
            if c == 'e' || c == 'E' {
                self.input.next();
                kind = Kind::Double;

                number.push(c);
                number.push_str(&self.lex_digits(None, 10));
            }
        }
        if kind == Kind::Int {
            if let Some(&(_, 'u')) = self.input.peek() {
                self.input.next();
                kind = Kind::UInt;
            }
        }

        match kind {
            Kind::Double => {
                let value = f64::from_str(&number).unwrap();
                (Span::new_with_len(bytepos, number.len()), Token::DoubleLit(value))
            }
            Kind::Int => {
                let value = i64::from_str(&number).unwrap();
                (Span::new_with_len(bytepos, number.len()), Token::IntLit(value))
            }
            Kind::UInt => {
                let value = u64::from_str(&number).unwrap();
                (Span::new_with_len(bytepos, number.len()), Token::UIntLit(value))
            }
        }
    }

    fn lex_char(&mut self, start_pos: usize) -> Result<u8, CodeError> {
        if let Some((bytepos, c)) = self.input.next() {
            if c == '\\' {
                match self.input.next() {
                    Some((_, 'n')) => Ok(b'\n'),
                    Some((_, 'r')) => Ok(b'\r'),
                    Some((_, 't')) => Ok(b'\t'),
                    Some((_, '\\')) => Ok(b'\\'),
                    Some((_, '\'')) => Ok(b'\''),
                    Some((_, '0')) => Ok(b'\0'),
                    Some((epos, a)) => new_lex_error(LexErrorKind::WrongEscapeChar(a), epos),
                    None => new_lex_error(LexErrorKind::NotClosedCharLiteral, bytepos),
                }
            } else {
                Ok(c as u8)
            }
        } else {
            new_lex_error(LexErrorKind::NotClosedCharLiteral, start_pos)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(Span, Token), CodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((bytepos, c)) = self.input.next() {
            Some(match c {
                c if is_identifier_start(c) => {
                    Ok(identifier_or_keyword(self.take_while(Some(c), is_identifier_continue), bytepos))
                }
                c if c.is_digit(10) => Ok(self.lex_number_lit(c, bytepos)),
                '\'' => {
                    let c = match self.lex_char(bytepos) {
                        Ok(c) => c,
                        Err(err) => return Some(Err(err))
                    };
                    match self.input.peek() {
                        Some(&(end, '\'')) => {
                            self.input.next();
                            Ok((Span::new(bytepos, end + 1), Token::CharLit(c)))
                        }
                        _ => new_lex_error(LexErrorKind::Expected('\''), bytepos)
                    }
                }
                '<' => Ok(self.if_next('=', Token::LessEqualOp, Token::LessOp, bytepos)),
                '>' => Ok(self.if_next('=', Token::GreaterEqualOp, Token::GreaterOp, bytepos)),
                '=' => Ok(self.if_next('=', Token::EqualOp, Token::AssignOp, bytepos)),
                '!' => Ok(self.if_next('=', Token::NotEqualOp, Token::LogNotOp, bytepos)),
                '&' => Ok(self.if_next('&', Token::LogAndOp, Token::AmpOp, bytepos)),
                '|' => {
                    match self.input.peek() {
                        Some(&(_, '|')) => {
                            self.input.next();
                            Ok((Span::new_with_len(bytepos, 2), Token::LogOrOp))
                        }
                        _ => new_lex_error(LexErrorKind::Expected('|'), bytepos),
                    }
                }
                '(' => Ok((Span::new_one(bytepos), Token::LParen)),
                ')' => Ok((Span::new_one(bytepos), Token::RParen)),
                '+' => Ok(self.if_next('=', Token::AssignPlusOp, Token::PlusOp, bytepos)),
                '-' => {
                    match self.input.peek() {
                        Some(&(_, '>')) => {
                            self.input.next();
                            Ok((Span::new_with_len(bytepos, 2), Token::Arrow))
                        }
                        Some(&(_, '=')) => {
                            self.input.next();
                            Ok((Span::new_with_len(bytepos, 2), Token::AssignMinusOp))
                        }
                        _ => Ok((Span::new_one(bytepos), Token::MinusOp)),
                    }
                }
                '*' => Ok(self.if_next('=', Token::AssignTimesOp, Token::TimesOp, bytepos)),
                '/' => Ok(self.if_next('=', Token::AssignDivOp, Token::DivOp, bytepos)),
                '%' => Ok(self.if_next('=', Token::AssignModOp, Token::ModOp, bytepos)),
                ',' => Ok((Span::new_one(bytepos), Token::Comma)),
                '{' => Ok((Span::new_one(bytepos), Token::LBrace)),
                '}' => Ok((Span::new_one(bytepos), Token::RBrace)),
                ':' => Ok((Span::new_one(bytepos), Token::Colon)),
                ';' => Ok((Span::new_one(bytepos), Token::SemiColon)),
                c => new_lex_error(LexErrorKind::Expected(c), bytepos),
            })
        } else {
            None
        }
    }
}

fn new_lex_error<T>(kind: LexErrorKind, bytepos: usize) -> Result<T, CodeError> {
    Err(CodeError::LexError(LexError(kind, bytepos)))
}
