use itertools::Itertools;

pub mod pretty_printer;
pub mod sema;
pub mod cgen;

use std::fmt;

use source::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Param>,
    pub return_ty: Type,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        identifier: String,
        ty: Type,
        expression: Expression,
        span: Span,
    },
    If {
        condition: Expression,
        if_statement: Box<Statement>,
        else_statement: Option<Box<Statement>>,
        span: Span,
    },
    Loop {
        statement: Box<Statement>,
        span: Span,
    },
    While {
        condition: Expression,
        statement: Box<Statement>,
        span: Span,
    },
    Break { span: Span },
    Continue { span: Span },
    Return { expression: Expression, span: Span },
    Expression { expression: Expression, span: Span },
    Block { block: Block, span: Span },
}

impl Statement {
    pub fn span(&self) -> Span {
        match *self {
            Statement::Let { span, .. } |
            Statement::If { span, .. } |
            Statement::Loop { span, .. } |
            Statement::While { span, .. } |
            Statement::Break { span, .. } |
            Statement::Continue { span, .. } |
            Statement::Return { span, .. } |
            Statement::Expression { span, .. } |
            Statement::Block { span, .. } => span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOpKind {
    Normal,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl fmt::Display for AssignOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            AssignOpKind::Normal => "=",
            AssignOpKind::Add => "+=",
            AssignOpKind::Sub => "-=",
            AssignOpKind::Mul => "*=",
            AssignOpKind::Div => "/=",
            AssignOpKind::Mod => "%=",
        })
    }
}

impl AssignOpKind {
    pub fn to_binop(self) -> Option<BinOpKind> {
        match self {
            AssignOpKind::Normal => None,
            AssignOpKind::Add => Some(BinOpKind::Add),
            AssignOpKind::Sub => Some(BinOpKind::Sub),
            AssignOpKind::Mul => Some(BinOpKind::Mul),
            AssignOpKind::Div => Some(BinOpKind::Div),
            AssignOpKind::Mod => Some(BinOpKind::Mod),
        }
    }
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
    LogicalAnd,
    LogicalOr,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Mod => "%",
            BinOpKind::Less => "<",
            BinOpKind::LessEq => "<=",
            BinOpKind::Greater => ">",
            BinOpKind::GreaterEq => ">=",
            BinOpKind::Equal => "==",
            BinOpKind::NotEqual => "!=",
            BinOpKind::LogicalAnd => "&&",
            BinOpKind::LogicalOr => "||",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
    Plus,
    Minus,
    LogicalNot,
    Deref,
    AddressOf,
}

impl fmt::Display for UnOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            UnOpKind::Plus => "+",
            UnOpKind::Minus => "-",
            UnOpKind::LogicalNot => "!",
            UnOpKind::Deref => "*",
            UnOpKind::AddressOf => "&",
        })
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    AssignOp {
        kind: AssignOpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span,
        ty: Type,
    },
    BinOp {
        kind: BinOpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span,
        ty: Type,
    },
    UnOp {
        kind: UnOpKind,
        expression: Box<Expression>,
        span: Span,
        ty: Type,
    },
    Paren {
        expression: Box<Expression>,
        span: Span,
        ty: Type,
    },
    FuncCall {
        function_name: String,
        arguments: Vec<Expression>,
        span: Span,
        ty: Type,
    },
    L2R {
        expression: Box<Expression>,
        ty: Type,
        span: Span,
    },
    Cast {
        expression: Box<Expression>,
        ty: Type,
        span: Span,
    },
    Identifier {
        identifier: String,
        span: Span,
        ty: Type,
    },
    IntLit { value: i64, span: Span },
    UIntLit { value: u64, span: Span },
    DoubleLit { value: f64, span: Span },
    CharLit { value: u8, span: Span },
    BoolLit { value: bool, span: Span },
    UnitLit { span: Span },
}

impl Expression {
    pub fn span(&self) -> Span {
        match *self {
            Expression::AssignOp { span, .. } |
            Expression::BinOp { span, .. } |
            Expression::UnOp { span, .. } |
            Expression::Paren { span, .. } |
            Expression::FuncCall { span, .. } |
            Expression::L2R { span, .. } |
            Expression::Cast { span, .. } |
            Expression::Identifier { span, .. } |
            Expression::IntLit { span, .. } |
            Expression::UIntLit { span, .. } |
            Expression::CharLit { span, .. } |
            Expression::BoolLit { span, .. } |
            Expression::DoubleLit { span, .. } |
            Expression::UnitLit { span } => span,
        }
    }

    pub fn ty(&self) -> Type {
        match *self {
            Expression::AssignOp { ref ty, .. } |
            Expression::BinOp { ref ty, .. } |
            Expression::UnOp { ref ty, .. } |
            Expression::Paren { ref ty, .. } |
            Expression::FuncCall { ref ty, .. } |
            Expression::L2R { ref ty, .. } |
            Expression::Cast { ref ty, .. } |
            Expression::Identifier { ref ty, .. } => ty.clone(),
            Expression::IntLit { .. } => Type::Int,
            Expression::UIntLit { .. } => Type::UInt,
            Expression::CharLit { .. } => Type::Char,
            Expression::BoolLit { .. } => Type::Bool,
            Expression::DoubleLit { .. } => Type::Double,
            Expression::UnitLit { .. } => Type::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Int,
    UInt,
    Bool,
    Char,
    Double,
    Ptr(Box<Type>),
    LVal(Box<Type>),
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
            Type::LVal(ref sub_ty) => write!(f, "lval({})", sub_ty),
            Type::Ptr(ref sub_ty) => write!(f, "*{}", sub_ty),
            Type::Function(ref params, ref ret) => {
                write!(f, "({}) -> {}", params.iter().join(", "), ret)
            }
        }
    }
}

impl Type {
    pub fn function_type(params: Vec<Param>, ret: Type) -> Type {
        Type::Function(params.into_iter().map(|param| param.ty).collect(),
                       Box::new(ret))
    }
}
