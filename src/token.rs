use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    BeginKw,
    EndKw,
    ReadKw,
    PrintKw,
    LoopKw,
    BreakKw,
    IfKw,
    ElseKw,
    WhileKw,
    LParen,
    RParen,
    AssignOp,
    PlusOp,
    MinusOp,
    TimesOp,
    DivideOp,
    ModOp,
    LessOp,
    LessEqualOp,
    GreaterOp,
    GreaterEqualOp,
    EqualOp,
    NotEqualOp,
    LogNotOp,
    Number(i64),
    Identifier(String),
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
            Token::IfKw => write!(f, "IF"),
            Token::ElseKw => write!(f, "ELSE"),
            Token::WhileKw => write!(f, "WHILE"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::AssignOp => write!(f, "="),
            Token::PlusOp => write!(f, "+"),
            Token::MinusOp => write!(f, "-"),
            Token::TimesOp => write!(f, "*"),
            Token::DivideOp => write!(f, "/"),
            Token::ModOp => write!(f, "%"),
            Token::LessOp => write!(f, "<"),
            Token::LessEqualOp => write!(f, "<="),
            Token::GreaterOp => write!(f, ">"),
            Token::GreaterEqualOp => write!(f, ">="),
            Token::EqualOp => write!(f, "=="),
            Token::NotEqualOp => write!(f, "!="),
            Token::LogNotOp => write!(f, "!"),
            Token::Number(n) => write!(f, "{}", n),
            Token::Identifier(ref id) => write!(f, "{}", id),
        }
    }
}
