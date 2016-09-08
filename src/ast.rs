use source::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IfKind {
    Negative,
    Positive,
    Zero,
}

#[derive(Debug, Clone)]
pub enum Statement {
    // TODO: span is a bit repetitive
    Print { expr: Expression, span: Span },
    Read { target_id: String, span: Span },
    If {
        kind: IfKind,
        cond: Expression,
        if_stmts: Vec<Statement>,
        else_stmts: Option<Vec<Statement>>,
        span: Span,
    },
    Loop { stmts: Vec<Statement>, span: Span },
    Break { span: Span },
    Assign {
        target_id: String,
        value: Expression,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum Expression {
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
    Identifier { id: String, span: Span },
    Number { value: i64, span: Span },
}

impl Expression {
    pub fn span(&self) -> Span {
        match *self {
            Expression::BinOp { span, .. } |
            Expression::UnOp { span, .. } |
            Expression::Paren { span, .. } |
            Expression::Identifier { span, .. } |
            Expression::Number { span, .. } => span,
        }
    }
}
