use ty::{SCBinOpKind, BinOpKind, UnOpKind};

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub ty: Type,
    pub scope: Scope,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        id: String,
        ty: Type,
        comp: Computation,
    },
    If {
        cond: Value,
        if_scope: Scope,
        else_scope: Option<Scope>,
    },
    Loop { scope: Scope },
    While { cond: Value, scope: Scope },
    Break,
    Continue,
    Return { val: Value },
    Compute { comp: Computation },
    Scope { scope: Scope },
}

#[derive(Debug, Clone)]
pub enum Computation {
    Value(Value),
    SCBinOp {
        kind: SCBinOpKind,
        lhs: Value,
        rhs: Value,
    },
    BinOp {
        kind: BinOpKind,
        lhs: Value,
        rhs: Value,
    },
    UnOp { kind: UnOpKind, expr: Value },
    FuncCall { name: String, args: Vec<Value> },
}

#[derive(Debug, Clone)]
pub enum Value {
    IntLitteral(i64),
    UIntLitteral(u64),
    CharLitteral(u8),
    BoolLitteral(bool),
    Var(String),
    Argument(usize),
}
