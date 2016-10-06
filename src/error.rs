use std::fmt;

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

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LexErrorKind::*;
        match *self {
            WrongEscapeChar(c) => write!(f, "\'{}\' is not a valid escape char", c),
            NotClosedCharLiteral => write!(f, "Unclosed char literal"),
            Unexpected(c) => write!(f, "Unexpected char \'{}\'", c),
            Expected(c) => write!(f, "Expected \'{}\'", c),
        }
    }
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
    MismatchedTypes(Type, Type),
    LetAlreadyDefined(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    UndefinedIdentifier(String),
    AssignLHSNotRef,
    BinOpUndefined(BinOpKind, Type, Type),
    UnOpUndefined(UnOpKind, Type),
    CastUndefined(Type, Type),
    FuncCallMismatchingLen(usize, usize),
    FuncCallNotCallable(String, Type),
    TypeUndefined(String),
}

impl fmt::Display for SemaErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SemaErrorKind::*;
        match *self {
            FunctionAlreadyDefined(ref name) => write!(f, "Function \'{}\' is already defined", name),
            ParameterAlreadyDefined(ref name) => write!(f, "Parameter \'{}\' is already declared", name),
            MismatchedTypes(ref expected, ref given) => {
                write!(
                    f,
                    "Mismatched types. Expected type \'{}\' given type \'{}\'.",
                    expected,
                    given
                )
            }
            LetAlreadyDefined(ref name) => write!(f, "Let binding \'{}\' is already defined.", name),
            BreakOutsideLoop => write!(f, "Break outside a loop."),
            ContinueOutsideLoop => write!(f, "Continue outside a loop."),
            UndefinedIdentifier(ref name) => write!(f, "\'{}\' is not defined.", name),
            AssignLHSNotRef => write!(f, "Left hand side of the assignment is not assignable."),
            BinOpUndefined(ref kind, ref lhs_ty, ref rhs_ty) => {
                write!(
                    f,
                    "\'{}\' is not defined between \'{}\' and \'{}\'.",
                    kind,
                    lhs_ty,
                    rhs_ty
                )
            }
            UnOpUndefined(ref kind, ref expr_ty) => {
                write!(
                    f,
                    "'\'{}\' is not defined for \'{}\'.",
                    kind,
                    expr_ty
                )
            }
            CastUndefined(ref expr_ty, ref target_ty) => {
                write!(
                    f,
                    "Cast undefined between \'{}\' and \'{}\'.",
                    expr_ty,
                    target_ty
                )
            }
            FuncCallMismatchingLen(ref exp, ref giv) => {
                write!(
                    f,
                    "Mismatched parameters lenght. Expected {} given {}.",
                    exp,
                    giv
                )
            }
            FuncCallNotCallable(ref name, ref ty) => {
                write!(
                    f,
                    "\'{}\' of type \'{}\' is not callable.",
                    name,
                    ty
                )
            }
            TypeUndefined(ref ty) => {
                write!(
                    f,
                    "\'{}\' is not a valid type.",
                    ty
                )
            }
        }
    }
}
