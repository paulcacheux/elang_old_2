use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    FnKw,
    LetKw,
    LoopKw,
    BreakKw,
    ContinueKw,
    ReturnKw,
    IfKw,
    ElseKw,
    WhileKw,
    AsKw,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    SemiColon,
    Arrow,
    AssignOp,
    AssignPlusOp,
    AssignMinusOp,
    AssignTimesOp,
    AssignDivOp,
    AssignModOp,
    PlusOp,
    MinusOp,
    TimesOp,
    DivOp,
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
    AmpOp,
    IntLit(i64),
    UIntLit(u64),
    DoubleLit(f64),
    BoolLit(bool),
    CharLit(u8),
    Identifier(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::FnKw => write!(f, "fn"),
            Token::LetKw => write!(f, "let"),
            Token::LoopKw => write!(f, "loop"),
            Token::BreakKw => write!(f, "break"),
            Token::ContinueKw => write!(f, "continue"),
            Token::ReturnKw => write!(f, "return"),
            Token::IfKw => write!(f, "if"),
            Token::ElseKw => write!(f, "else"),
            Token::WhileKw => write!(f, "while"),
            Token::AsKw => write!(f, "as"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Arrow => write!(f, "->"),
            Token::AssignOp => write!(f, "="),
            Token::AssignPlusOp => write!(f, "+="),
            Token::AssignMinusOp => write!(f, "-="),
            Token::AssignTimesOp => write!(f, "*="),
            Token::AssignDivOp => write!(f, "/="),
            Token::AssignModOp => write!(f, "%="),
            Token::PlusOp => write!(f, "+"),
            Token::MinusOp => write!(f, "-"),
            Token::TimesOp => write!(f, "*"),
            Token::DivOp => write!(f, "/"),
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
            Token::AmpOp => write!(f, "&"),
            Token::IntLit(n) => write!(f, "{}", n),
            Token::UIntLit(n) => write!(f, "{}", n),
            Token::DoubleLit(n) => write!(f, "{}", n),
            Token::BoolLit(n) => write!(f, "{:?}", n),
            Token::CharLit(n) => write!(f, "{:?}", n),
            Token::Identifier(ref id) => write!(f, "{}", id),
        }
    }
}
