use itertools::Itertools;
use std::collections::HashSet;
use std::fmt;

use ast;
use ast::{Program, Statement, Expression, BinOpKind, UnOpKind};

// TODO: Delete all the clones (Maybe Cow<str> ??)

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
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

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub vars: HashSet<String>,
    pub blocks: Vec<BasicBlock>,
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

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub branch: Branch,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}:\n", self.name));
        for inst in &self.instructions {
            try!(write!(f, "\t{}\n", inst));
        }
        write!(f, "\t{}\n", self.branch)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Branch {
    Jmp(String),
    JmpT(Value, String, String),
    Ret(Value),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Const(i64),
    Var(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(value) => write!(f, "{}", value),
            Value::Var(ref name) => write!(f, "{}", name),
        }
    }
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

impl fmt::Display for Computation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Computation::Value(ref src) => write!(f, "{}", src),
            Computation::FuncCall(ref func, ref params) => {
                write!(f, "{}({})", func, params.iter().join(", "))
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Assign(String, Computation),
    Print(Computation),
    Read(String),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Assign(ref dest, ref comp) => write!(f, "{} = {}", dest, comp),
            Instruction::Print(ref comp) => write!(f, "print {}", comp),
            Instruction::Read(ref dest) => write!(f, "{} = read()", dest),
        }
    }
}

pub fn generate(program: &Program) -> Module {
    let mut functions = Vec::new();
    for function in &program.functions {
        functions.push(generate_function(&function))
    }
    functions.push(generate_function(&program.main_func));

    Module { functions: functions }
}

fn generate_function(function: &ast::Function) -> Function {
    let mut builder = Builder::new();

    let mut blocks = Vec::new();

    for stmt in &function.block.stmts {
        blocks.extend(builder.generate_statement(stmt));
    }

    let end_block = BasicBlock {
        name: builder.new_label(),
        instructions: Vec::new(),
        branch: Branch::Ret(Value::Const(0)),
    };
    blocks.push(end_block);

    Function {
        name: function.name.clone(),
        params: function.params.clone(),
        vars: builder.vars,
        blocks: blocks,
    }
}

pub struct Builder {
    vars: HashSet<String>,
    current_break_label: Vec<String>,
    temp_counter: usize,
    label_counter: usize,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            vars: HashSet::new(),
            current_break_label: vec![String::from("exit")],
            temp_counter: 0,
            label_counter: 0,
        }
    }

    fn new_temp(&mut self) -> String {
        let temp = format!("_0temp{}", self.temp_counter);
        self.temp_counter += 1;
        self.vars.insert(temp.clone());
        temp
    }

    fn peek_label(&self) -> String {
        format!("label{}", self.label_counter)
    }

    fn new_label(&mut self) -> String {
        let label = self.peek_label();
        self.label_counter += 1;
        label
    }

    fn generate_statement(&mut self, statement: &Statement) -> Vec<BasicBlock> {
        match *statement {
            Statement::Print { ref expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(expr, expr_name.clone());
                instructions.push(Instruction::Print(Computation::Value(Value::Var(expr_name))));

                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: instructions,
                         branch: Branch::Jmp(self.peek_label()),
                     }]
            }
            Statement::Read { ref target_id, .. } => {
                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: vec![Instruction::Read(target_id.clone())],
                         branch: Branch::Jmp(self.peek_label()),
                     }]
            }
            Statement::If { ref cond, ref if_stmts, ref else_stmts, .. } => {
                let cond_name = self.new_temp();
                let cond_inst = self.generate_expression(cond, cond_name.clone());
                let cond_label = self.new_label();
                let true_label = self.new_label();
                let skip_false_label = self.new_label();
                let false_label = self.new_label();
                let end_label = self.new_label();

                let cond_branch = Branch::JmpT(Value::Var(cond_name.clone()),
                                               true_label.clone(),
                                               false_label.clone());

                let mut blocks = vec![BasicBlock {
                                          name: cond_label,
                                          instructions: cond_inst,
                                          branch: cond_branch,
                                      },
                                      BasicBlock {
                                          name: true_label,
                                          instructions: Vec::new(),
                                          branch: Branch::Jmp(self.peek_label()),
                                      }];

                for stmt in if_stmts {
                    blocks.extend(self.generate_statement(stmt));
                }

                // linker
                blocks.push(BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(skip_false_label.clone()),
                });

                blocks.push(BasicBlock {
                    name: skip_false_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(end_label.clone()),
                });

                blocks.push(BasicBlock {
                    name: false_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label()),
                });

                if let Some(ref else_stmts) = *else_stmts {
                    for stmt in else_stmts {
                        blocks.extend(self.generate_statement(stmt));
                    }
                }

                // to link the "peek label" of the last statement with the end_label
                blocks.push(BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(end_label.clone()),
                });

                blocks.push(BasicBlock {
                    name: end_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label()),
                });
                blocks
            }
            Statement::Loop { ref stmts, .. } => {
                let loop_start_label = self.new_label();
                let loop_end_label = self.new_label();

                let mut blocks = vec![BasicBlock {
                                          name: loop_start_label.clone(),
                                          instructions: Vec::new(),
                                          branch: Branch::Jmp(self.peek_label()),
                                      }];

                self.current_break_label.push(loop_end_label.clone());
                for stmt in stmts {
                    blocks.extend(self.generate_statement(stmt));
                }
                self.current_break_label.pop();

                // loop reloader
                blocks.push(BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(loop_start_label.clone()),
                });

                blocks.push(BasicBlock {
                    name: loop_end_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label()),
                });
                blocks
            }
            Statement::While { ref cond, ref stmts, .. } => {
                let cond_name = self.new_temp();
                let cond_inst = self.generate_expression(cond, cond_name.clone());
                let cond_label = self.new_label();
                let content_label = self.new_label();
                let end_label = self.new_label();

                let cond_branch = Branch::JmpT(Value::Var(cond_name.clone()),
                                               content_label.clone(),
                                               end_label.clone());

                let mut blocks = vec![BasicBlock {
                                          name: cond_label.clone(),
                                          instructions: cond_inst,
                                          branch: cond_branch,
                                      },
                                      BasicBlock {
                                          name: content_label,
                                          instructions: Vec::new(),
                                          branch: Branch::Jmp(self.peek_label()),
                                      }];

                for stmt in stmts {
                    blocks.extend(self.generate_statement(stmt));
                }

                blocks.push(BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(cond_label.clone()),
                });
                blocks.push(BasicBlock {
                    name: end_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label()),
                });
                blocks
            }
            Statement::Break { .. } => {
                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: Vec::new(),
                         branch: Branch::Jmp(self.current_break_label.last().unwrap().clone()),
                     }]
            }
            Statement::Assign { ref target_id, ref value, .. } => {
                let value_name = self.new_temp();
                let mut instructions = self.generate_expression(value, value_name.clone());
                instructions.push(Instruction::Assign(target_id.clone(),
                                                      Computation::Value(Value::Var(value_name))));

                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: instructions,
                         branch: Branch::Jmp(self.peek_label()),
                     }]
            }
        }
    }

    fn generate_expression(&mut self, expression: &Expression, name: String) -> Vec<Instruction> {
        match *expression {
            Expression::BinOp { kind, ref lhs, ref rhs, .. } => {
                let lhs_name = self.new_temp();
                let rhs_name = self.new_temp();
                let mut instructions = self.generate_expression(lhs, lhs_name.clone());
                instructions.extend(self.generate_expression(rhs, rhs_name.clone()));
                instructions.push(Instruction::Assign(name,
                                                      match kind {
                                                          BinOpKind::Add => {
                                                              Computation::Add(Value::Var(lhs_name),
                                                                               Value::Var(rhs_name))
                                                          }
                                                          BinOpKind::Sub => {
                                                              Computation::Sub(Value::Var(lhs_name),
                                                                               Value::Var(rhs_name))
                                                          }
                                                          BinOpKind::Mul => {
                                                              Computation::Mul(Value::Var(lhs_name),
                                                                               Value::Var(rhs_name))
                                                          }
                                                          BinOpKind::Div => {
                                                              Computation::Div(Value::Var(lhs_name),
                                                                               Value::Var(rhs_name))
                                                          }
                                                          BinOpKind::Mod => {
                                                              Computation::Mod(Value::Var(lhs_name),
                                                                               Value::Var(rhs_name))
                                                          }
                                                          BinOpKind::Less => {
                        Computation::CmpLess(Value::Var(lhs_name), Value::Var(rhs_name))
                    }
                                                          BinOpKind::LessEq => {
                        Computation::CmpLessEq(Value::Var(lhs_name), Value::Var(rhs_name))
                    }
                                                          BinOpKind::Greater => {
                        Computation::CmpGreater(Value::Var(lhs_name), Value::Var(rhs_name))
                    }
                                                          BinOpKind::GreaterEq => {
                        Computation::CmpGreaterEq(Value::Var(lhs_name), Value::Var(rhs_name))
                    }
                                                          BinOpKind::Equal => {
                        Computation::CmpEq(Value::Var(lhs_name), Value::Var(rhs_name))
                    }
                                                          BinOpKind::NotEqual => {
                        Computation::CmpNotEq(Value::Var(lhs_name), Value::Var(rhs_name))
                    }
                                                      }));
                instructions
            }
            Expression::UnOp { kind, ref expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(expr, expr_name.clone());
                instructions.push(Instruction::Assign(name, match kind {
                    UnOpKind::Plus => Computation::Value(Value::Var(expr_name)),
                    UnOpKind::Minus => Computation::Negate(Value::Var(expr_name)),
                    UnOpKind::LogNot => Computation::LogNot(Value::Var(expr_name)),
                }));
                instructions
            }
            Expression::Paren { ref expr, .. } => self.generate_expression(expr, name),
            Expression::FuncCall { ref func_name, ref params, .. } => {
                let mut instructions = Vec::new();
                let mut param_values = Vec::new();
                for param in params {
                    let param_name = self.new_temp();
                    instructions.extend(self.generate_expression(param, param_name.clone()));
                    param_values.push(Value::Var(param_name));
                }
                instructions.push(Instruction::Assign(name,
                                                      Computation::FuncCall(func_name.clone(),
                                                                            param_values)));
                instructions
            }
            Expression::Identifier { ref id, .. } => {
                self.vars.insert(id.clone());
                vec![Instruction::Assign(name, Computation::Value(Value::Var(id.clone())))]
            }
            Expression::Number { value, .. } => {
                vec![Instruction::Assign(name, Computation::Value(Value::Const(value)))]
            }
        }
    }
}
