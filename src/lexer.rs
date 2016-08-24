use std::iter::Peekable;
use std::str::FromStr;

use ast::IfKind;
use token::Token;
use reader::Span;

fn identifier_or_keyword(raw: String, bytepos: usize) -> (Span, Token) {
    let span = Span::new_with_len(bytepos, raw.len());
    let token = match raw.as_str() {
        "BEGIN" => Token::BeginKw,
        "END" => Token::EndKw,
        "READ" => Token::ReadKw,
        "PRINT" => Token::PrintKw,
        "LOOP" => Token::LoopKw,
        "IFP" => Token::IfKw(IfKind::Positive),
        "IFN" => Token::IfKw(IfKind::Negative),
        "IFZ" => Token::IfKw(IfKind::Zero),
        "BREAK" => Token::BreakKw,
        "ELSE" => Token::ElseKw,
        _ => Token::Identifier(raw)
    };
    (span, token)
}

fn is_identifier_char(c: char) -> bool {
    match c {
        'a' ... 'z' | 'A' ... 'Z' | '_' => true,
        _ => false
    }
}

pub struct Lexer<R: Iterator<Item=(usize, char)>> {
    input: Peekable<R>
}

impl<R: Iterator<Item=(usize, char)>> Lexer<R> {
    pub fn new(input: R) -> Lexer<R> {
        Lexer {
            input: input.peekable()
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.input.peek() {
                Some(&(_, c)) if c.is_whitespace() => { self.input.next(); },
                _ => break
            }
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
                _ => break
            }
            self.input.next();
        }
        res
    }
}

impl<R: Iterator<Item=(usize, char)>> Iterator for Lexer<R> {
    type Item = (Span, Token);

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((bytepos, c)) = self.input.next() {
            Some(match c {
                c if is_identifier_char(c) => {
                    identifier_or_keyword(self.take_while(c, |c| is_identifier_char(c)), bytepos)
                },
                c if c.is_digit(10) => {
                    let number = self.take_while(c, |c| c.is_digit(10));
                    let value = i64::from_str(&number).unwrap();
                    (Span::new_with_len(bytepos, number.len()), Token::Number(value))
                }
                '(' => (Span::new_with_len(bytepos, 1), Token::LParen),
                ')' => (Span::new_with_len(bytepos, 1), Token::RParen),
                '=' => (Span::new_with_len(bytepos, 1), Token::AssignOp),
                '+' => (Span::new_with_len(bytepos, 1), Token::PlusOp),
                '-' => (Span::new_with_len(bytepos, 1), Token::MinusOp),
                '*' => (Span::new_with_len(bytepos, 1), Token::TimesOp),
                '/' => (Span::new_with_len(bytepos, 1), Token::DivideOp),
                '%' => (Span::new_with_len(bytepos, 1), Token::ModOp),
                _ => panic!("Unknown character")
            })
        } else {
            None
        }
    }
}
