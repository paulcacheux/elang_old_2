use std::io;
use std::collections::HashMap;

use ast;
use ast::{Program, IfKind, Statement, Expression, BinOpKind, UnOpKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpCode {
    PushConst(i64),
    PushVar(String),
    StoreInto(String),
    Pop,
    Dup,
    BinaryAdd,
    BinarySub,
    BinaryTimes,
    BinaryDivide,
    BinaryModulo,
    UnaryNeg,
    Print,
    Read,
    Jmp(usize),
    JmpP(usize),
    JmpN(usize),
    JmpZ(usize),
    Exit
}

pub struct Builder {
    pub opcodes: Vec<OpCode>,
    waiting_break: Vec<Vec<usize>>
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            opcodes: Vec::new(),
            waiting_break: vec![Vec::new()]
        }
    }

    fn register_break(&mut self, addr: usize) {
        self.waiting_break
            .last_mut()
            .map(|ref mut top| top.push(addr));
    }

    fn start_loop(&mut self) {
        self.waiting_break.push(Vec::new());
    }

    fn end_loop(&mut self, break_target: usize) {
        let breaks = self.waiting_break.pop().unwrap();
        for b in breaks {
            self.opcodes[b] = OpCode::Jmp(break_target);
        }
    }
}

impl ast::Visitor for Builder {
    fn visit_program(&mut self, program: &Program) {
        for stmt in &program.stmts {
            self.visit_statement(stmt);
        }
        let final_addr = self.opcodes.len();
        self.end_loop(final_addr); // break outside loop = early return
        self.opcodes.push(OpCode::Exit);
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match *statement {
            Statement::Print { ref expr, .. } => {
                self.visit_expression(expr);
                self.opcodes.push(OpCode::Print);
            },
            Statement::Read { ref target_id, .. } => {
                self.opcodes.push(OpCode::Read);
                self.opcodes.push(OpCode::StoreInto(target_id.clone()));
            },
            Statement::If { kind, ref cond, ref if_stmts, ref else_stmts, .. } => {
                self.visit_expression(cond);

                let first_target = self.opcodes.len() + 2;
                self.opcodes.push(match kind {
                    IfKind::Positive => OpCode::JmpP(first_target),
                    IfKind::Negative => OpCode::JmpN(first_target),
                    IfKind::Zero => OpCode::JmpZ(first_target)
                });

                let abs_jump = self.opcodes.len();
                self.opcodes.push(OpCode::Jmp(0));

                for stmt in if_stmts {
                    self.visit_statement(stmt);
                }

                let mut second_target = self.opcodes.len();
                if let Some(else_stmts) = else_stmts.as_ref() {
                    second_target += 1;
                    let skip_else = self.opcodes.len();
                    self.opcodes.push(OpCode::Jmp(0));

                    for stmt in else_stmts {
                        self.visit_statement(stmt);
                    }

                    let final_addr = self.opcodes.len();
                    self.opcodes[skip_else] = OpCode::Jmp(final_addr);
                }
                self.opcodes[abs_jump] = OpCode::Jmp(second_target);
            },
            Statement::Loop { ref stmts, .. } => {
                self.start_loop();
                let target = self.opcodes.len();
                for stmt in stmts {
                    self.visit_statement(stmt);
                }
                self.opcodes.push(OpCode::Jmp(target));
                let break_target_addr = self.opcodes.len();
                self.end_loop(break_target_addr);
            },
            Statement::Break { .. } => {
                let break_addr = self.opcodes.len();
                self.register_break(break_addr);
                self.opcodes.push(OpCode::Jmp(0));
            },
            Statement::Assign { ref target_id, ref value, .. } => {
                self.visit_expression(value);
                self.opcodes.push(OpCode::StoreInto(target_id.clone()));
            }
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match *expression {
            Expression::BinOp { kind, ref lhs, ref rhs, .. } => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
                self.opcodes.push(match kind {
                    BinOpKind::Add => OpCode::BinaryAdd,
                    BinOpKind::Sub => OpCode::BinarySub,
                    BinOpKind::Times => OpCode::BinaryTimes,
                    BinOpKind::Divide => OpCode::BinaryDivide,
                    BinOpKind::Modulo => OpCode::BinaryModulo
                });
            },
            Expression::UnOp { kind, ref expr, .. } => {
                self.visit_expression(expr);
                if kind == UnOpKind::Negative {
                    self.opcodes.push(OpCode::UnaryNeg);
                }
            },
            Expression::Paren { ref expr, .. } => {
                self.visit_expression(expr);
            },
            Expression::Identifier { ref id, .. } => {
                self.opcodes.push(OpCode::PushVar(id.clone()));
            },
            Expression::Number { value, .. } => {
                self.opcodes.push(OpCode::PushConst(value));
            }
        }
    }
}

pub fn run(opcodes: Vec<OpCode>) {
    let mut pc: usize = 0;
    let mut memory = HashMap::<String, i64>::new();
    let mut stack = Vec::<i64>::new();

    while pc < opcodes.len() {
        let ref opcode = opcodes[pc];
        pc += 1;
        match *opcode {
            OpCode::PushConst(val) => {
                stack.push(val);
            },
            OpCode::PushVar(ref id) => {
                stack.push(memory.get(id).cloned().unwrap_or(0));
            },
            OpCode::StoreInto(ref id) => {
                memory.insert(id.clone(), stack.pop().unwrap());
            },
            OpCode::Pop => {
                stack.pop();
            },
            OpCode::Dup => {
                let val = stack.last().cloned().unwrap();
                stack.push(val);
            },
            OpCode::BinaryAdd => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a + b);
            },
            OpCode::BinarySub => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a - b);
            },
            OpCode::BinaryTimes => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a * b);
            },
            OpCode::BinaryDivide => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a / b);
            },
            OpCode::BinaryModulo => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a % b);
            },
            OpCode::UnaryNeg => {
                let val = stack.pop().unwrap();
                stack.push(-val);
            },
            OpCode::Print => {
                println!("{}", stack.pop().unwrap());
            },
            OpCode::Read => {
                let mut line = String::new();
                io::stdin().read_line(&mut line).unwrap();
                stack.push(line.parse::<i64>().unwrap_or(0));
            },
            OpCode::Jmp(target) => {
                pc = target;
            },
            OpCode::JmpP(target) => {
                let val = stack.pop().unwrap();
                if val > 0 {
                    pc = target;
                }
            },
            OpCode::JmpN(target) => {
                let val = stack.pop().unwrap();
                if val < 0 {
                    pc = target;
                }
            },
            OpCode::JmpZ(target) => {
                let val = stack.pop().unwrap();
                if val == 0 {
                    pc = target;
                }
            },
            OpCode::Exit => {
                return;
            }
        }
    }
}
