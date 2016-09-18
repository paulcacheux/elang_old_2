use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    FnKw,
    LoopKw,
    BreakKw,
    ReturnKw,
    IfKw,
    ElseKw,
    WhileKw,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    SemiColon,
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
    LogAndOp,
    LogOrOp,
    LogNotOp,
    Number(i64),
    Identifier(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::FnKw => write!(f, "fn"),
            Token::LoopKw => write!(f, "loop"),
            Token::BreakKw => write!(f, "break"),
            Token::ReturnKw => write!(f, "return"),
            Token::IfKw => write!(f, "if"),
            Token::ElseKw => write!(f, "else"),
            Token::WhileKw => write!(f, "while"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::SemiColon => write!(f, ";"),
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
            Token::LogAndOp => write!(f, "&&"),
            Token::LogOrOp => write!(f, "||"),
            Token::LogNotOp => write!(f, "!"),
            Token::Number(n) => write!(f, "{}", n),
            Token::Identifier(ref id) => write!(f, "{}", id),
        }
    }
}
