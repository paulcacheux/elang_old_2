use source::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub main_func: Function,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    If {
        cond: Expression,
        if_stmts: Vec<Statement>,
        else_stmts: Option<Vec<Statement>>,
        span: Span,
    },
    Loop { stmts: Vec<Statement>, span: Span },
    While {
        cond: Expression,
        stmts: Vec<Statement>,
        span: Span,
    },
    Break { span: Span },
    Return { expr: Expression, span: Span },
    Expression { expr: Expression, span: Span },
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

#[derive(Debug, Clone)]
pub enum Expression {
    Assign {
        id: String,
        value: Box<Expression>,
        span: Span,
    },
    BinOp {
        kind: BinOpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span,
    },
    UnOp {
        kind: UnOpKind,
        expr: Box<Expression>,
        span: Span,
    },
    Paren { expr: Box<Expression>, span: Span },
    FuncCall {
        func_name: String,
        args: Vec<Expression>,
        span: Span,
    },
    Identifier { id: String, span: Span },
    Number { value: i64, span: Span },
}

impl Expression {
    pub fn span(&self) -> Span {
        match *self {
            Expression::Assign { span, .. } |
            Expression::BinOp { span, .. } |
            Expression::UnOp { span, .. } |
            Expression::Paren { span, .. } |
            Expression::FuncCall { span, .. } |
            Expression::Identifier { span, .. } |
            Expression::Number { span, .. } => span,
        }
    }
}
