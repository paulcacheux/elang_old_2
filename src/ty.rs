use itertools::Itertools;

use fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SCBinOpKind {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
    Plus,
    Minus,
    LogNot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Unit,
    Int,
    UInt,
    Bool,
    Char,
    Double,
    Ref(Box<Type>),
    Function(Vec<Type>, Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "int"),
            Type::UInt => write!(f, "uint"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Double => write!(f, "double"),
            Type::Ref(ref sub_ty) => write!(f, "&{}", sub_ty),
            Type::Function(ref params, ref ret) => {
                write!(f, "({}) -> {}", params.iter().join(", "), ret)
            }
        }
    }
}
