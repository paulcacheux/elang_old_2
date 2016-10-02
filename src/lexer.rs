use std::str::FromStr;

use double_peekable::DoublePeekable;
use token::Token;
use source::Span;
use diagnostic::DiagnosticEngine;

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

fn is_identifier_char(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '_' => true,
        _ => false,
    }
}

pub struct Lexer<'a, R: Iterator<Item = (usize, char)>> {
    input: DoublePeekable<R>,
    diagnostic: &'a DiagnosticEngine<'a>,
}

impl<'a, R: Iterator<Item = (usize, char)>> Lexer<'a, R> {
    pub fn new(input: R, diag: &'a DiagnosticEngine<'a>) -> Lexer<'a, R> {
        Lexer {
            input: DoublePeekable::new(input),
            diagnostic: diag,
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
                Some(&(_, '\n')) => break,
                Some(_) => continue,
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
            _ => (Span::new_with_len(bytepos, 1), tok_false),
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

    fn lex_char(&mut self, start_pos: usize) -> u8 {
        if let Some((bytepos, c)) = self.input.next() {
            if c == '\\' {
                match self.input.next() {
                    Some((_, 'n')) => b'\n',
                    Some((_, 'r')) => b'\r',
                    Some((_, 't')) => b'\t',
                    Some((_, '\\')) => b'\\',
                    Some((_, '\'')) => b'\'',
                    Some((_, '0')) => b'\0',
                    Some((epos, a)) => {
                        self.diagnostic.report_lex_error(
                            format!("{} is not a valid escape char", a),
                            epos
                        )
                    }
                    None => {
                        self.diagnostic.report_lex_error(
                            "Unclosed char literal".to_string(), bytepos
                        )
                    }
                }
            } else {
                c as u8
            }
        } else {
            self.diagnostic.report_lex_error(
                "Unclosed char literal".to_string(), start_pos
            )
        }
    }
}

impl<'a, R: Iterator<Item = (usize, char)>> Iterator for Lexer<'a, R> {
    type Item = (Span, Token);

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((bytepos, c)) = self.input.next() {
            Some(match c {
                c if is_identifier_char(c) => {
                    identifier_or_keyword(self.take_while(Some(c), is_identifier_char), bytepos)
                }
                c if c.is_digit(10) => self.lex_number_lit(c, bytepos),
                '\'' => {
                    let c = self.lex_char(bytepos);
                    match self.input.peek() {
                        Some(&(end, '\'')) => {
                            self.input.next();
                            (Span::new(bytepos, end + 1), Token::CharLit(c))
                        }
                        _ => self.diagnostic.report_lex_error("Expected \'".to_string(), bytepos),
                    }
                }
                '<' => self.if_next('=', Token::LessEqualOp, Token::LessOp, bytepos),
                '>' => self.if_next('=', Token::GreaterEqualOp, Token::GreaterOp, bytepos),
                '=' => self.if_next('=', Token::EqualOp, Token::AssignOp, bytepos),
                '!' => self.if_next('=', Token::NotEqualOp, Token::LogNotOp, bytepos),
                '&' => self.if_next('&', Token::LogAndOp, Token::AmpOp, bytepos),
                '|' => {
                    match self.input.peek() {
                        Some(&(_, '|')) => {
                            self.input.next();
                            (Span::new_with_len(bytepos, 2), Token::LogOrOp)
                        }
                        _ => self.diagnostic.report_lex_error("Expected |".to_string(), bytepos),
                    }
                }
                '(' => (Span::new_with_len(bytepos, 1), Token::LParen),
                ')' => (Span::new_with_len(bytepos, 1), Token::RParen),
                '+' => (Span::new_with_len(bytepos, 1), Token::PlusOp),
                '-' => self.if_next('>', Token::Arrow, Token::MinusOp, bytepos),
                '*' => (Span::new_with_len(bytepos, 1), Token::TimesOp),
                '/' => (Span::new_with_len(bytepos, 1), Token::DivideOp),
                '%' => (Span::new_with_len(bytepos, 1), Token::ModOp),
                ',' => (Span::new_with_len(bytepos, 1), Token::Comma),
                '{' => (Span::new_with_len(bytepos, 1), Token::LBrace),
                '}' => (Span::new_with_len(bytepos, 1), Token::RBrace),
                ':' => (Span::new_with_len(bytepos, 1), Token::Colon),
                ';' => (Span::new_with_len(bytepos, 1), Token::SemiColon),
                c => self.diagnostic.report_lex_error(format!("Unexpected char {}", c), bytepos),
            })
        } else {
            None
        }
    }
}
