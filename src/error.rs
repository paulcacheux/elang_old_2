use source::Span;
use ast::Type;
use ast::{BinOpKind, UnOpKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodeError {
    LexError(LexError),
    ParseError(ParseError),
    SemaError(SemaError),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError(pub LexErrorKind, pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
    WrongEscapeChar(char),
    NotClosedCharLiteral,
    Unexpected(char),
    Expected(char),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub expected: Vec<String>,
    pub span: Option<Span>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaError(pub SemaErrorKind, pub Span);

// always in the order (expected, given)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemaErrorKind {
    FunctionAlreadyDefined(String),
    ParameterAlreadyDefined(String),
    LetMismatchingTypes(Type, Type),
    LetAlreadyDefined(String),
    IfConditionNotBool(Type),
    WhileConditionNotBool(Type),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    ReturnMismatchingTypes(Type, Type),
    UndefinedIdentifier(String),
    AssignMismatchingTypes(Type, Type),
    AssignLHSNotRef,
    BinOpUndefined(BinOpKind, Type, Type),
    UnOpUndefined(UnOpKind, Type),
    CastUndefined(Type, Type),
    FuncCallMismatchingLen(usize, usize),
    FuncCallMismatchingTypes(Type, Type),
    FuncCallNotCallable(String, Type),
    TypeUndefined(String),
}
