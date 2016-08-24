use std::fmt;

use ast::IfKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    BeginKw,
    EndKw,
    ReadKw,
    PrintKw,
    LoopKw,
    BreakKw,
    IfKw(IfKind),
    ElseKw,
    LParen,
    RParen,
    AssignOp,
    PlusOp,
    MinusOp,
    TimesOp,
    DivideOp,
    ModOp,
    Number(i64),
    Identifier(String)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::BeginKw => write!(f, "BEGIN"),
            Token::EndKw => write!(f, "END"),
            Token::ReadKw => write!(f, "READ"),
            Token::PrintKw => write!(f, "PRINT"),
            Token::LoopKw => write!(f, "LOOP"),
            Token::BreakKw => write!(f, "BREAK"),
            Token::IfKw(if_kind) => match if_kind {
                IfKind::Negative => write!(f, "IFN"),
                IfKind::Positive => write!(f, "IFP"),
                IfKind::Zero => write!(f, "IFZ")
            },
            Token::ElseKw => write!(f, "ELSE"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::AssignOp => write!(f, "="),
            Token::PlusOp => write!(f, "+"),
            Token::MinusOp => write!(f, "-"),
            Token::TimesOp => write!(f, "*"),
            Token::DivideOp => write!(f, "/"),
            Token::ModOp => write!(f, "%"),
            Token::Number(n) => write!(f, "{}", n),
            Token::Identifier(ref id) => write!(f, "{}", id)
        }
    }
}
