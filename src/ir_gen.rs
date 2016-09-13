use std::collections::{HashSet, HashMap};

use diagnostic::DiagnosticEngine;
use ast;
use ir::{Module, Function, BasicBlockId, BasicBlock, Branch, Instruction, Computation, Value};

pub fn generate<'a>(program: ast::Program, diag_engine: &DiagnosticEngine<'a>) -> Module {
    let mut functions = Vec::new();
    let mut builder = Builder::new(diag_engine);
    builder.symbols.functions.insert("print".to_string(), 1);
    builder.symbols.functions.insert("read".to_string(), 0);
    for function in program.functions {
        functions.push(builder.generate_function(function))
    }
    functions.push(builder.generate_function(program.main_func));

    Module { functions: functions }
}

struct SymbolTable {
    vars: HashSet<String>,
    functions: HashMap<String, usize>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            vars: HashSet::new(),
            functions: HashMap::new(),
        }
    }

    fn clear_vars(&mut self) {
        self.vars.clear();
    }
}

struct Builder<'a> {
    diag_engine: &'a DiagnosticEngine<'a>,
    symbols: SymbolTable,
    current_break_blockid: Vec<BasicBlockId>,
    temp_counter: usize,
    blockid_counter: usize,
}

impl<'a> Builder<'a> {
    pub fn new(diag_engine: &'a DiagnosticEngine<'a>) -> Builder<'a> {
        Builder {
            diag_engine: diag_engine,
            symbols: SymbolTable::new(),
            current_break_blockid: Vec::new(),
            temp_counter: 0,
            blockid_counter: 0,
        }
    }

    fn new_temp(&mut self) -> String {
        let temp = format!("_0temp{}", self.temp_counter);
        self.temp_counter += 1;
        self.symbols.vars.insert(temp.clone());
        temp
    }

    fn peek_blockid(&self) -> BasicBlockId {
        BasicBlockId(self.blockid_counter)
    }

    fn new_blockid(&mut self) -> BasicBlockId {
        let blockid = self.peek_blockid();
        self.blockid_counter += 1;
        blockid
    }
    fn generate_function(&mut self, function: ast::Function) -> Function {
        if self.symbols.functions.contains_key(&function.name) {
            self.diag_engine
                .report_sema_error(format!("{} function is already defined", function.name),
                                   function.span);
        }

        self.symbols.functions.insert(function.name.clone(), function.params.len());
        self.symbols.vars.extend(function.params.clone());

        let mut blocks = Vec::new();

        for stmt in function.block.stmts {
            blocks.extend(self.generate_statement(stmt));
        }

        let end_block = BasicBlock {
            id: self.new_blockid(),
            instructions: Vec::new(),
            branch: Branch::Ret(Value::Const(0)),
        };
        blocks.push(end_block);

        let func = Function {
            name: function.name,
            params: function.params,
            vars: self.symbols.vars.clone(),
            blocks: blocks,
        };

        self.symbols.clear_vars();
        func
    }

    fn generate_statement(&mut self, statement: ast::Statement) -> Vec<BasicBlock> {
        use ast::Statement::*;
        match statement {
            If { cond, if_stmts, else_stmts, .. } => {
                let cond_name = self.new_temp();
                let cond_inst = self.generate_expression(cond, Some(cond_name.clone()));
                let cond_blockid = self.new_blockid();
                let true_blockid = self.new_blockid();
                let skip_false_blockid = self.new_blockid();
                let false_blockid = self.new_blockid();
                let end_blockid = self.new_blockid();

                let cond_branch =
                    Branch::JmpT(Value::Var(cond_name.clone()), true_blockid, false_blockid);

                let mut blocks = vec![BasicBlock {
                                          id: cond_blockid,
                                          instructions: cond_inst,
                                          branch: cond_branch,
                                      },
                                      BasicBlock {
                                          id: true_blockid,
                                          instructions: Vec::new(),
                                          branch: Branch::Jmp(self.peek_blockid()),
                                      }];

                for stmt in if_stmts {
                    blocks.extend(self.generate_statement(stmt));
                }

                // linker
                blocks.push(BasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(skip_false_blockid),
                });

                blocks.push(BasicBlock {
                    id: skip_false_blockid,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(end_blockid),
                });

                blocks.push(BasicBlock {
                    id: false_blockid,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_blockid()),
                });

                if let Some(else_stmts) = else_stmts {
                    for stmt in else_stmts {
                        blocks.extend(self.generate_statement(stmt));
                    }
                }

                // to link the "peek label" of the last statement with the end_label
                blocks.push(BasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(end_blockid),
                });

                blocks.push(BasicBlock {
                    id: end_blockid,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_blockid()),
                });
                blocks
            }
            Loop { stmts, .. } => {
                let loop_start_blockid = self.new_blockid();
                let loop_end_blockid = self.new_blockid();

                let mut blocks = vec![BasicBlock {
                                          id: loop_start_blockid,
                                          instructions: Vec::new(),
                                          branch: Branch::Jmp(self.peek_blockid()),
                                      }];

                self.current_break_blockid.push(loop_end_blockid);
                for stmt in stmts {
                    blocks.extend(self.generate_statement(stmt));
                }
                self.current_break_blockid.pop();

                // loop reloader
                blocks.push(BasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(loop_start_blockid),
                });

                blocks.push(BasicBlock {
                    id: loop_end_blockid,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_blockid()),
                });
                blocks
            }
            While { cond, stmts, .. } => {
                let cond_name = self.new_temp();
                let cond_inst = self.generate_expression(cond, Some(cond_name.clone()));
                let cond_blockid = self.new_blockid();
                let content_blockid = self.new_blockid();
                let end_blockid = self.new_blockid();

                let cond_branch =
                    Branch::JmpT(Value::Var(cond_name.clone()), content_blockid, end_blockid);

                let mut blocks = vec![BasicBlock {
                                          id: cond_blockid,
                                          instructions: cond_inst,
                                          branch: cond_branch,
                                      },
                                      BasicBlock {
                                          id: content_blockid,
                                          instructions: Vec::new(),
                                          branch: Branch::Jmp(self.peek_blockid()),
                                      }];

                self.current_break_blockid.push(end_blockid);
                for stmt in stmts {
                    blocks.extend(self.generate_statement(stmt));
                }
                self.current_break_blockid.pop();

                blocks.push(BasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Branch::Jmp(cond_blockid),
                });
                blocks.push(BasicBlock {
                    id: end_blockid,
                    instructions: Vec::new(),
                    branch: Branch::Jmp(self.peek_blockid()),
                });
                blocks
            }
            Break { span } => {
                if let Some(&break_dest) = self.current_break_blockid.last() {
                    vec![BasicBlock {
                             id: self.new_blockid(),
                             instructions: Vec::new(),
                             branch: Branch::Jmp(break_dest),
                         }]
                } else {
                    self.diag_engine
                        .report_sema_error("Break outside of a loop/while".to_string(), span);
                }
            }
            Return { expr, .. } => {
                let expr_name = self.new_temp();
                let instructions = self.generate_expression(expr, Some(expr_name.clone()));
                vec![BasicBlock {
                         id: self.new_blockid(),
                         instructions: instructions,
                         branch: Branch::Ret(Value::Var(expr_name)),
                     }]
            }
            Expression { expr, .. } => {
                vec![BasicBlock {
                         id: self.new_blockid(),
                         instructions: self.generate_expression(expr, None),
                         branch: Branch::Jmp(self.peek_blockid()),
                     }]
            }
        }
    }

    fn generate_expression(&mut self,
                           expression: ast::Expression,
                           name: Option<String>)
                           -> Vec<Instruction> {
        use ast::Expression::*;
        use ast::BinOpKind;
        use ast::UnOpKind;
        match expression {
            Assign { id, value, .. } => {
                let value_name = self.new_temp();
                let mut instructions = self.generate_expression(*value, Some(value_name.clone()));
                instructions.push(Instruction::Assign(id.clone(),
                                                      Computation::Value(Value::Var(value_name))));
                if let Some(name) = name {
                    instructions.push(Instruction::Assign(name, Computation::Value(Value::Var(id))))
                }
                instructions
            }
            BinOp { kind, lhs, rhs, .. } => {
                let lhs_name = self.new_temp();
                let rhs_name = self.new_temp();
                let mut instructions = self.generate_expression(*lhs, Some(lhs_name.clone()));
                instructions.extend(self.generate_expression(*rhs, Some(rhs_name.clone())));
                let comp = match kind {
                    BinOpKind::Add => Computation::Add(Value::Var(lhs_name), Value::Var(rhs_name)),
                    BinOpKind::Sub => Computation::Sub(Value::Var(lhs_name), Value::Var(rhs_name)),
                    BinOpKind::Mul => Computation::Mul(Value::Var(lhs_name), Value::Var(rhs_name)),
                    BinOpKind::Div => Computation::Div(Value::Var(lhs_name), Value::Var(rhs_name)),
                    BinOpKind::Mod => Computation::Mod(Value::Var(lhs_name), Value::Var(rhs_name)),
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
                };
                instructions.push(if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                });
                instructions
            }
            UnOp { kind, expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(*expr, Some(expr_name.clone()));
                let comp = match kind {
                    UnOpKind::Plus => Computation::Value(Value::Var(expr_name)),
                    UnOpKind::Minus => Computation::Negate(Value::Var(expr_name)),
                    UnOpKind::LogNot => Computation::LogNot(Value::Var(expr_name)),
                };
                instructions.push(if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                });
                instructions
            }
            Paren { expr, .. } => self.generate_expression(*expr, name),
            FuncCall { func_name, args, span } => {
                if !self.symbols.functions.contains_key(&func_name) {
                    self.diag_engine
                        .report_sema_error(format!("{} function is actually not defined",
                                                   func_name),
                                           span);
                }

                let expected_len = *self.symbols.functions.get(&func_name).unwrap();
                if expected_len != args.len() {
                    self.diag_engine
                        .report_sema_error(format!("{} function expect {} arguments, you gave {}",
                                                   func_name,
                                                   expected_len,
                                                   args.len()),
                                           span);
                }

                let mut instructions = Vec::new();
                let mut arg_values = Vec::new();
                for arg in args {
                    let arg_name = self.new_temp();
                    instructions.extend(self.generate_expression(arg, Some(arg_name.clone())));
                    arg_values.push(Value::Var(arg_name));
                }

                let comp = Computation::FuncCall(func_name.clone(), arg_values);
                instructions.push(if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                });
                instructions
            }
            Identifier { id, span } => {
                if !self.symbols.vars.contains(&id) {
                    self.diag_engine
                        .report_sema_warning(format!("{} is referenced before assignment, it \
                                                      will default to 0",
                                                     id),
                                             span);
                }
                self.symbols.vars.insert(id.clone());
                let comp = Computation::Value(Value::Var(id.clone()));
                vec![if let Some(name) = name {
                         Instruction::Assign(name, comp)
                     } else {
                         Instruction::Compute(comp)
                     }]
            }
            Number { value, .. } => {
                let comp = Computation::Value(Value::Const(value));
                vec![if let Some(name) = name {
                         Instruction::Assign(name, comp)
                     } else {
                         Instruction::Compute(comp)
                     }]
            }
        }
    }
}
