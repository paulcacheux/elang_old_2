use std::collections::{HashSet, HashMap};

use diagnostic::DiagnosticEngine;
use ast;
use ir::{Module, Function, BasicBlock, Branch, Instruction, Computation, Value};

pub fn generate<'a>(program: ast::Program, diag_engine: &DiagnosticEngine<'a>) -> Module {
    let mut functions = Vec::new();
    let mut builder = Builder::new(diag_engine);
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
    current_break_label: Vec<String>,
    temp_counter: usize,
    label_counter: usize,
}

impl<'a> Builder<'a> {
    pub fn new(diag_engine: &'a DiagnosticEngine<'a>) -> Builder<'a> {
        Builder {
            diag_engine: diag_engine,
            symbols: SymbolTable::new(),
            current_break_label: vec![String::from("exit")],
            temp_counter: 0,
            label_counter: 0,
        }
    }

    fn new_temp(&mut self) -> String {
        let temp = format!("_0temp{}", self.temp_counter);
        self.temp_counter += 1;
        self.symbols.vars.insert(temp.clone());
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
    fn generate_function(&mut self, function: ast::Function) -> Function {
        if self.symbols.functions.contains_key(&function.name) {
            self.diag_engine
                .report_sema_error(format!("{} function is already defined", function.name),
                                   function.span);
        }

        self.symbols.functions.insert(function.name.clone(), function.params.len());

        let mut blocks = Vec::new();

        for stmt in function.block.stmts {
            blocks.extend(self.generate_statement(stmt));
        }

        let end_block = BasicBlock {
            name: self.new_label(),
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
            Print { expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(expr, expr_name.clone());
                instructions.push(Instruction::Print(Computation::Value(Value::Var(expr_name))));

                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: instructions,
                         branch: Branch::Jmp(self.peek_label()),
                     }]
            }
            Read { target_id, .. } => {
                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: vec![Instruction::Read(target_id.clone())],
                         branch: Branch::Jmp(self.peek_label()),
                     }]
            }
            If { cond, if_stmts, else_stmts, .. } => {
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

                if let Some(else_stmts) = else_stmts {
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
            Loop { stmts, .. } => {
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
            While { cond, stmts, .. } => {
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
            Break { .. } => {
                vec![BasicBlock {
                         name: self.new_label(),
                         instructions: Vec::new(),
                         branch: Branch::Jmp(self.current_break_label.last().unwrap().clone()),
                     }]
            }
            Assign { target_id, value, .. } => {
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

    fn generate_expression(&mut self,
                           expression: ast::Expression,
                           name: String)
                           -> Vec<Instruction> {
        use ast::Expression::*;
        use ast::BinOpKind;
        use ast::UnOpKind;
        match expression {
            BinOp { kind, lhs, rhs, .. } => {
                let lhs_name = self.new_temp();
                let rhs_name = self.new_temp();
                let mut instructions = self.generate_expression(*lhs, lhs_name.clone());
                instructions.extend(self.generate_expression(*rhs, rhs_name.clone()));
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
                instructions.push(Instruction::Assign(name, comp));
                instructions
            }
            UnOp { kind, expr, .. } => {
                let expr_name = self.new_temp();
                let mut instructions = self.generate_expression(*expr, expr_name.clone());
                instructions.push(Instruction::Assign(name, match kind {
                    UnOpKind::Plus => Computation::Value(Value::Var(expr_name)),
                    UnOpKind::Minus => Computation::Negate(Value::Var(expr_name)),
                    UnOpKind::LogNot => Computation::LogNot(Value::Var(expr_name)),
                }));
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
                    instructions.extend(self.generate_expression(arg, arg_name.clone()));
                    arg_values.push(Value::Var(arg_name));
                }
                instructions.push(Instruction::Assign(name,
                                                      Computation::FuncCall(func_name.clone(),
                                                                            arg_values)));
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
                vec![Instruction::Assign(name, Computation::Value(Value::Var(id.clone())))]
            }
            Number { value, .. } => {
                vec![Instruction::Assign(name, Computation::Value(Value::Const(value)))]
            }
        }
    }
}
