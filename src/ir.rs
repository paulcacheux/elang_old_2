use std::collections::HashSet;
use std::fmt;

use ast;
use ast::{Program, IfKind, Statement, Expression, BinOpKind, UnOpKind};

// TODO: Delete all the clones (Maybe Cow<str> ??)

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub branch: Branch
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
    JmpP(String, String, String),
    JmpZ(String, String, String),
    JmpN(String, String, String),
    Ret
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Branch::Jmp(ref dest) => write!(f, "jmp {}", dest),
            Branch::JmpP(ref cond, ref true_label, ref false_label) =>
                write!(f, "jmpP {}, {}, {}", cond, true_label, false_label),
            Branch::JmpZ(ref cond, ref true_label, ref false_label) =>
                write!(f, "jmpZ {}, {}, {}", cond, true_label, false_label),
            Branch::JmpN(ref cond, ref true_label, ref false_label) =>
                write!(f, "jmpN {}, {}, {}", cond, true_label, false_label),
            Branch::Ret => write!(f, "return")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    SetConst(String, i64),
    Assign(String, String),
    Add(String, String, String), // target = a + b
    Sub(String, String, String),
    Mul(String, String, String),
    Div(String, String, String),
    Mod(String, String, String),
    Negate(String, String),
    Print(String),
    Read(String)
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::SetConst(ref dest, value) =>
                write!(f, "{} = {}", dest, value),
            Instruction::Assign(ref dest, ref src) =>
                write!(f, "{} = {}", dest, src),
            Instruction::Add(ref dest, ref lhs, ref rhs) =>
                write!(f, "{} = add {} {}", dest, lhs , rhs),
            Instruction::Sub(ref dest, ref lhs, ref rhs) =>
                write!(f, "{} = sub {} {}", dest, lhs , rhs),
            Instruction::Mul(ref dest, ref lhs, ref rhs) =>
                write!(f, "{} = mul {} {}", dest, lhs , rhs),
            Instruction::Div(ref dest, ref lhs, ref rhs) =>
                write!(f, "{} = div {} {}", dest, lhs , rhs),
            Instruction::Mod(ref dest, ref lhs, ref rhs) =>
                write!(f, "{} = mod {} {}", dest, lhs , rhs),
            Instruction::Negate(ref dest, ref value) =>
                write!(f, "{} = neg {}", dest, value),
            Instruction::Print(ref value) =>
                write!(f, "print {}", value),
            Instruction::Read(ref dest) =>
                write!(f, "{} = read", dest)
        }
    }
}

pub fn generate(program: &Program) -> Vec<BasicBlock> {
    let mut builder = Builder::new();
    let first_real_label = builder.peek_label();
    let content_blocks = builder.generate_program(program);
    let start_block = BasicBlock {
        name: String::from("start"),
        instructions: builder
            .vars
            .into_iter()
            .map(|name| Instruction::SetConst(name, 0))
            .collect(),
        branch: Branch::Jmp(first_real_label)
    };

    let mut blocks = vec![start_block];
    blocks.extend(content_blocks);
    blocks
}

pub struct Builder {
    vars: HashSet<String>,
    current_break_label: Vec<String>,
    temp_counter: usize,
    label_counter: usize
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            vars: HashSet::new(),
            current_break_label: vec![String::from("exit")],
            temp_counter: 0,
            label_counter: 0
        }
    }

    fn new_temp(&mut self) -> String {
        let temp = format!("_0temp{}", self.temp_counter);
        self.temp_counter += 1;
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

    fn generate_program(&mut self, program: &Program) -> Vec<BasicBlock> {
        let mut blocks = Vec::new();

        for stmt in &program.stmts {
            blocks.extend(self.generate_statement(stmt));
        }

        let end_block = BasicBlock {
            name: self.new_label(),
            instructions: Vec::new(),
            branch: Branch::Ret
        };
        blocks.push(end_block);

        blocks
    }

    fn generate_statement(&mut self, statement: &Statement) -> Vec<BasicBlock> {
        match *statement {
            Statement::Print { ref expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(expr, expr_name.clone());
                instructions.push(Instruction::Print(expr_name));

                vec![BasicBlock {
                    name: self.new_label(),
                    instructions: instructions,
                    branch: Branch::Jmp(self.peek_label())
                }]
            },
            Statement::Read { ref target_id, .. } => {
                vec![BasicBlock {
                    name: self.new_label(),
                    instructions: vec![Instruction::Read(target_id.clone())],
                    branch: Branch::Jmp(self.peek_label())
                }]
            },
            Statement::If { kind, ref cond, ref if_stmts, ref else_stmts, .. } => {
                let cond_name = self.new_temp();
                let cond_inst = self.generate_expression(cond, cond_name.clone());
                let cond_label = self.new_label();
                let true_label = self.new_label();
                let skip_false_label = self.new_label();
                let false_label = self.new_label();
                let end_label = self.new_label();

                let cond_branch = match kind {
                    IfKind::Positive => Branch::JmpP(
                        cond_name.clone(),
                        true_label.clone(),
                        false_label.clone()
                    ),
                    IfKind::Negative => Branch::JmpN(
                        cond_name.clone(),
                        true_label.clone(),
                        false_label.clone()
                    ),
                    IfKind::Zero => Branch::JmpZ(
                        cond_name.clone(),
                        true_label.clone(),
                        false_label.clone()
                    )
                };

                let mut blocks = vec![
                    BasicBlock {
                        name: cond_label,
                        instructions: cond_inst,
                        branch: cond_branch
                    },
                    BasicBlock{
                        name: true_label,
                        instructions: Vec::new(),
                        branch: Branch::Jmp(self.peek_label())
                    }
                ];

                for stmt in if_stmts {
                    blocks.extend(self.generate_statement(stmt));
                }

                // linker
                blocks.push(BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(skip_false_label.clone())
                });

                blocks.push(BasicBlock {
                    name: skip_false_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(end_label.clone())
                });

                blocks.push(BasicBlock {
                    name: false_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label())
                });

                if let &Some(ref else_stmts) = else_stmts {
                    for stmt in else_stmts {
                        blocks.extend(self.generate_statement(stmt));
                    }
                }

                // to link the "peek label" of the last statement with the end_label
                blocks.push(BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(end_label.clone())
                });

                blocks.push(BasicBlock {
                    name: end_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label())
                });
                blocks
            },
            Statement::Loop { ref stmts, .. } => {
                let loop_start_label = self.new_label();
                let loop_end_label = self.new_label();

                let mut blocks = vec![BasicBlock {
                    name: loop_start_label.clone(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label())
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
                    branch: Branch::Jmp(loop_start_label.clone())
                });

                blocks.push(BasicBlock {
                    name: loop_end_label,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_label())
                });
                blocks
            },
            Statement::Break { .. } => {
                vec![BasicBlock {
                    name: self.new_label(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(
                        self.current_break_label.last().unwrap().clone()
                    )
                }]
            },
            Statement::Assign { ref target_id, ref value, .. } => {
                let value_name = self.new_temp();
                let mut instructions = self.generate_expression(value, value_name.clone());
                instructions.push(Instruction::Assign(target_id.clone(), value_name));

                vec![BasicBlock {
                    name: self.new_label(),
                    instructions: instructions,
                    branch: Branch::Jmp(self.peek_label())
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
                instructions.push(match kind {
                    BinOpKind::Add => Instruction::Add(name, lhs_name, rhs_name),
                    BinOpKind::Sub => Instruction::Sub(name, lhs_name, rhs_name),
                    BinOpKind::Mul => Instruction::Mul(name, lhs_name, rhs_name),
                    BinOpKind::Div => Instruction::Div(name, lhs_name, rhs_name),
                    BinOpKind::Mod => Instruction::Mod(name, lhs_name, rhs_name)
                });
                instructions
            },
            Expression::UnOp { kind, ref expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(expr, expr_name.clone());
                instructions.push(match kind {
                    UnOpKind::Plus => Instruction::Assign(name, expr_name),
                    UnOpKind::Minus => Instruction::Negate(name, expr_name)
                });
                instructions
            },
            Expression::Paren { ref expr, .. } => {
                self.generate_expression(expr, name)
            },
            Expression::Identifier { ref id, .. } => {
                self.vars.insert(id.clone());
                vec![Instruction::Assign(name, id.clone())]
            },
            Expression::Number { value, .. } => {
                vec![Instruction::SetConst(name, value)]
            }
        }
    }
}
