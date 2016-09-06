use source::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Statement>,
    pub span: Span
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IfKind {
    Negative,
    Positive,
    Zero
}

#[derive(Debug, Clone)]
pub enum Statement { //TODO: span is a bit repetitive
    Print {
        expr: Expression,
        span: Span
    },
    Read {
        target_id: String,
        span: Span
    },
    If {
        kind: IfKind,
        cond: Expression,
        if_stmts: Vec<Statement>,
        else_stmts: Option<Vec<Statement>>,
        span: Span
    },
    Loop {
        stmts: Vec<Statement>,
        span: Span
    },
    Break {
        span: Span
    },
    Assign {
        target_id: String,
        value: Expression,
        span: Span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

impl BinOpKind {
    pub fn apply(self, a: i64, b: i64) -> i64 {
        match self {
            BinOpKind::Add => a + b,
            BinOpKind::Sub => a - b,
            BinOpKind::Mul => a * b,
            BinOpKind::Div => a / b,
            BinOpKind::Mod => a % b
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
    Plus,
    Minus
}

impl UnOpKind {
    pub fn apply(self, val: i64) -> i64 {
        match self {
            UnOpKind::Plus => val, // noop
            UnOpKind::Minus => -val
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinOp {
        kind: BinOpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span
    },
    UnOp {
        kind: UnOpKind,
        expr: Box<Expression>,
        span: Span
    },
    Paren {
        expr: Box<Expression>,
        span: Span
    },
    Identifier {
        id: String,
        span: Span
    },
    Number {
        value: i64,
        span: Span
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match *self {
            Expression::BinOp { span, .. } => span,
            Expression::UnOp { span, .. } => span,
            Expression::Paren { span, .. } => span,
            Expression::Identifier { span, .. } => span,
            Expression::Number { span, .. } => span
        }
    }
}
