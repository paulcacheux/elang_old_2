use std::iter::Peekable;
use std::str::FromStr;

use token::Token;
use source::Span;
use diagnostic::DiagnosticEngine;

fn identifier_or_keyword(raw: String, bytepos: usize) -> (Span, Token) {
    let span = Span::new_with_len(bytepos, raw.len());
    let token = match raw.as_str() {
        "FN" => Token::FnKw,
        "BEGIN" => Token::BeginKw,
        "END" => Token::EndKw,
        "READ" => Token::ReadKw,
        "PRINT" => Token::PrintKw,
        "LOOP" => Token::LoopKw,
        "IF" => Token::IfKw,
        "BREAK" => Token::BreakKw,
        "RETURN" => Token::ReturnKw,
        "ELSE" => Token::ElseKw,
        "WHILE" => Token::WhileKw,
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
    input: Peekable<R>,
    diagnostic: &'a DiagnosticEngine<'a>,
}

impl<'a, R: Iterator<Item = (usize, char)>> Lexer<'a, R> {
    pub fn new(input: R, diag: &'a DiagnosticEngine<'a>) -> Lexer<'a, R> {
        Lexer {
            input: input.peekable(),
            diagnostic: diag,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.input.peek() {
                Some(&(_, c)) if c.is_whitespace() => {
                    self.input.next();
                }
                _ => break,
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

    fn take_while<P>(&mut self, first: char, predicate: P) -> String
        where P: Fn(char) -> bool
    {
        let mut res = String::new();
        res.push(first);

        loop {
            match self.input.peek() {
                Some(&(_, c)) if predicate(c) => res.push(c),
                _ => break,
            }
            self.input.next();
        }
        res
    }
}

impl<'a, R: Iterator<Item = (usize, char)>> Iterator for Lexer<'a, R> {
    type Item = (Span, Token);

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((bytepos, c)) = self.input.next() {
            Some(match c {
                c if is_identifier_char(c) => {
                    identifier_or_keyword(self.take_while(c, is_identifier_char), bytepos)
                }
                c if c.is_digit(10) => {
                    let number = self.take_while(c, |c| c.is_digit(10));
                    let value = i64::from_str(&number).unwrap();
                    (Span::new_with_len(bytepos, number.len()), Token::Number(value))
                }
                '<' => self.if_next('=', Token::LessEqualOp, Token::LessOp, bytepos),
                '>' => self.if_next('=', Token::GreaterEqualOp, Token::GreaterOp, bytepos),
                '=' => self.if_next('=', Token::EqualOp, Token::AssignOp, bytepos),
                '!' => self.if_next('=', Token::NotEqualOp, Token::LogNotOp, bytepos),
                '(' => (Span::new_with_len(bytepos, 1), Token::LParen),
                ')' => (Span::new_with_len(bytepos, 1), Token::RParen),
                '+' => (Span::new_with_len(bytepos, 1), Token::PlusOp),
                '-' => (Span::new_with_len(bytepos, 1), Token::MinusOp),
                '*' => (Span::new_with_len(bytepos, 1), Token::TimesOp),
                '/' => (Span::new_with_len(bytepos, 1), Token::DivideOp),
                '%' => (Span::new_with_len(bytepos, 1), Token::ModOp),
                ',' => (Span::new_with_len(bytepos, 1), Token::Comma),
                c => self.diagnostic.report_lex_error(format!("Unexpected char {}", c), bytepos),
            })
        } else {
            None
        }
    }
}
