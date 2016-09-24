pub mod opt;
pub mod gen;

use itertools::Itertools;
use std::collections::HashSet;

use std::fmt;

// TODO: Delete all the clones (Maybe Cow<str> ??)

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub vars: HashSet<String>,
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub usize);

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub branch: Branch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Branch {
    Jmp(BasicBlockId),
    JmpT(Value, BasicBlockId, BasicBlockId),
    Ret(Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Const(i64),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Computation {
    Value(Value),
    FuncCall(String, Vec<Value>),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Mod(Value, Value),
    CmpLess(Value, Value),
    CmpLessEq(Value, Value),
    CmpGreater(Value, Value),
    CmpGreaterEq(Value, Value),
    CmpEq(Value, Value),
    CmpNotEq(Value, Value),
    LogNot(Value),
    Negate(Value),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Assign(String, Computation),
    Compute(Computation),
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "-----> Module:\n"));
        for func in &self.functions {
            try!(write!(f, "{}\n", func));
        }
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}() {{\n", self.name));
        try!(write!(f, "params: {}\n", self.params.iter().join(", ")));
        try!(write!(f, "vars: {}\n", self.vars.iter().join(", ")));
        for block in &self.blocks {
            try!(write!(f, "{}\n", block));
        }
        write!(f, "}}\n")
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}:\n", self.id));
        for inst in &self.instructions {
            try!(write!(f, "\t{}\n", inst));
        }
        write!(f, "\t{}\n", self.branch)
    }
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Branch::Jmp(ref dest) => write!(f, "jmp {}", dest),
            Branch::JmpT(ref cond, ref true_label, ref false_label) => {
                write!(f, "jmpT {}, {}, {}", cond, true_label, false_label)
            }
            Branch::Ret(ref val) => write!(f, "return {}", val),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(value) => write!(f, "{}", value),
            Value::Var(ref name) => write!(f, "{}", name),
        }
    }
}

impl fmt::Display for Computation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Computation::Value(ref src) => write!(f, "{}", src),
            Computation::FuncCall(ref func, ref params) => {
                write!(f, "call {}({})", func, params.iter().join(", "))
            }
            Computation::Add(ref lhs, ref rhs) => write!(f, "add {} {}", lhs, rhs),
            Computation::Sub(ref lhs, ref rhs) => write!(f, "sub {} {}", lhs, rhs),
            Computation::Mul(ref lhs, ref rhs) => write!(f, "mul {} {}", lhs, rhs),
            Computation::Div(ref lhs, ref rhs) => write!(f, "div {} {}", lhs, rhs),
            Computation::Mod(ref lhs, ref rhs) => write!(f, "mod {} {}", lhs, rhs),
            Computation::CmpLess(ref lhs, ref rhs) => write!(f, "< {} {}", lhs, rhs),
            Computation::CmpLessEq(ref lhs, ref rhs) => write!(f, "<= {} {}", lhs, rhs),
            Computation::CmpGreater(ref lhs, ref rhs) => write!(f, "> {} {}", lhs, rhs),
            Computation::CmpGreaterEq(ref lhs, ref rhs) => write!(f, ">= {} {}", lhs, rhs),
            Computation::CmpEq(ref lhs, ref rhs) => write!(f, "== {} {}", lhs, rhs),
            Computation::CmpNotEq(ref lhs, ref rhs) => write!(f, "!= {} {}", lhs, rhs),
            Computation::LogNot(ref value) => write!(f, "! {}", value),
            Computation::Negate(ref value) => write!(f, "neg {}", value),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Assign(ref dest, ref comp) => write!(f, "{} = {}", dest, comp),
            Instruction::Compute(ref comp) => write!(f, "{}", comp),
        }
    }
}