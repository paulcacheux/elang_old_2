use std::collections::{HashSet, HashMap};

use diagnostic::DiagnosticEngine;
use ast;
use ir::{Module, Function, BasicBlockId, BasicBlock, Branch, Instruction, Computation, Value};

pub fn generate<'a>(program: ast::Program, diag_engine: &DiagnosticEngine<'a>) -> Module {
    let mut functions = Vec::new();
    let mut builder = Sema::new(diag_engine);
    builder.symbols.functions.insert("println".to_string(), 1);
    builder.symbols.functions.insert("print".to_string(), 1);
    builder.symbols.functions.insert("read".to_string(), 0);

    let mut main_available = false;
    for function in program.functions {
        let function = builder.generate_function(function);
        if function.name == "main" {
            main_available = true;
        }
        functions.push(function)
    }

    if !main_available {
        diag_engine.report_sema_error("main function is not defined".to_string(), program.span)
    }

    Module { functions: functions }
}

#[derive(Debug, Clone)]
struct TempBasicBlock {
    id: BasicBlockId,
    instructions: Vec<Instruction>,
    branch: Option<Branch>,
}

impl TempBasicBlock {
    fn new(id: BasicBlockId, insts: Vec<Instruction>) -> TempBasicBlock {
        TempBasicBlock {
            id: id,
            instructions: insts,
            branch: None,
        }
    }

    fn new_from_inst(id: BasicBlockId, inst: Instruction) -> TempBasicBlock {
        TempBasicBlock {
            id: id,
            instructions: vec![inst],
            branch: None,
        }
    }
}

fn finalize(temp_blocks: Vec<TempBasicBlock>) -> Vec<BasicBlock> {
    let mut last_blockid = BasicBlockId(0);

    let mut blocks: Vec<_> = temp_blocks.into_iter()
        .rev()
        .map(|block| {
            let bb = BasicBlock {
                id: block.id,
                instructions: block.instructions,
                branch: match block.branch {
                    Some(br) => br,
                    None => Branch::Jmp(last_blockid),
                },
            };
            last_blockid = block.id;
            bb
        })
        .collect();
    blocks.reverse();
    blocks
}

struct SymbolTable {
    all_vars: HashSet<String>,
    scopes: Vec<HashSet<String>>,
    functions: HashMap<String, usize>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            all_vars: HashSet::new(),
            scopes: Vec::new(),
            functions: HashMap::new(),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_var(&mut self, name: String) {
        self.scopes.last_mut().unwrap().insert(name.clone());
        self.all_vars.insert(name);
    }

    fn is_var_available(&self, name: &String) -> bool {
        self.scopes.iter().rev().any(|ref scope| scope.contains(name))
    }
}

struct Sema<'a> {
    diag_engine: &'a DiagnosticEngine<'a>,
    symbols: SymbolTable,
    current_break_blockid: Vec<BasicBlockId>,
    current_continue_blockid: Vec<BasicBlockId>,
    temp_counter: usize,
    blockid_counter: usize,
}

impl<'a> Sema<'a> {
    pub fn new(diag_engine: &'a DiagnosticEngine<'a>) -> Sema<'a> {
        Sema {
            diag_engine: diag_engine,
            symbols: SymbolTable::new(),
            current_break_blockid: Vec::new(),
            current_continue_blockid: Vec::new(),
            temp_counter: 0,
            blockid_counter: 0,
        }
    }

    fn new_temp(&mut self) -> String {
        let temp = format!("_0temp{}", self.temp_counter);
        self.temp_counter += 1;
        self.symbols.add_var(temp.clone());
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
        self.symbols.begin_scope();
        for param in function.params.clone() {
            self.symbols.add_var(param);
        }
        self.symbols.begin_scope();

        let mut blocks = Vec::new();

        for stmt in function.block.stmts {
            blocks.extend(self.generate_statement(stmt));
        }

        let end_block = TempBasicBlock {
            id: self.new_blockid(),
            instructions: Vec::new(),
            branch: Some(Branch::Ret(Value::Const(0))),
        };
        blocks.push(end_block);

        let finalized_blocks = finalize(blocks);

        let func = Function {
            name: function.name,
            params: function.params,
            vars: self.symbols.all_vars.clone(),
            blocks: finalized_blocks,
        };

        self.symbols.end_scope();
        self.symbols.end_scope();
        func
    }

    fn generate_statement(&mut self, statement: ast::Statement) -> Vec<TempBasicBlock> {
        use ast::Statement::*;
        match statement {
            Let { id, expr, .. } => {
                let blockid = self.new_blockid();
                let blocks = self.generate_expression(expr, blockid, Some(id.clone()));
                self.symbols.add_var(id);
                blocks
            }
            If { cond, if_stmt, else_stmt, .. } => {
                self.symbols.begin_scope();
                let cond_name = self.new_temp();
                let cond_blockid = self.new_blockid();
                let true_blockid = self.new_blockid();
                let skip_false_blockid = self.new_blockid();
                let false_blockid = self.new_blockid();
                let end_blockid = self.new_blockid();

                let mut blocks =
                    self.generate_expression(cond, cond_blockid, Some(cond_name.clone()));

                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::JmpT(Value::Var(cond_name.clone()),
                                              true_blockid,
                                              false_blockid)),
                });

                blocks.push(TempBasicBlock {
                    id: true_blockid,
                    instructions: Vec::new(),
                    branch: None,
                });

                blocks.extend(self.generate_statement(*if_stmt));

                blocks.push(TempBasicBlock {
                    id: skip_false_blockid,
                    instructions: Vec::new(),
                    branch: Some(Branch::Jmp(end_blockid)),
                });

                blocks.push(TempBasicBlock {
                    id: false_blockid,
                    instructions: Vec::new(),
                    branch: None,
                });

                if let Some(else_stmt) = else_stmt {
                    blocks.extend(self.generate_statement(*else_stmt));
                }

                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::Jmp(end_blockid)),
                });

                blocks.push(TempBasicBlock {
                    id: end_blockid,
                    instructions: Vec::new(),
                    branch: None,
                });
                self.symbols.end_scope();
                blocks
            }
            Loop { stmt, .. } => {
                self.symbols.begin_scope();
                let loop_start_blockid = self.new_blockid();
                let loop_end_blockid = self.new_blockid();

                let mut blocks = vec![TempBasicBlock {
                                          id: loop_start_blockid,
                                          instructions: Vec::new(),
                                          branch: None,
                                      }];

                self.current_break_blockid.push(loop_end_blockid);
                self.current_continue_blockid.push(loop_start_blockid);
                blocks.extend(self.generate_statement(*stmt));
                self.current_break_blockid.pop();
                self.current_continue_blockid.pop();

                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::Jmp(loop_start_blockid)),
                });

                blocks.push(TempBasicBlock {
                    id: loop_end_blockid,
                    instructions: Vec::new(),
                    branch: None,
                });
                self.symbols.end_scope();
                blocks
            }
            While { cond, stmt, .. } => {
                self.symbols.begin_scope();
                let cond_name = self.new_temp();
                let cond_blockid = self.new_blockid();
                let content_blockid = self.new_blockid();
                let end_blockid = self.new_blockid();

                let mut blocks =
                    self.generate_expression(cond, cond_blockid, Some(cond_name.clone()));

                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::JmpT(Value::Var(cond_name.clone()),
                                              content_blockid,
                                              end_blockid)),
                });
                blocks.push(TempBasicBlock {
                    id: content_blockid,
                    instructions: Vec::new(),
                    branch: None,
                });

                self.current_break_blockid.push(end_blockid);
                self.current_continue_blockid.push(cond_blockid);
                blocks.extend(self.generate_statement(*stmt));
                self.current_break_blockid.pop();
                self.current_continue_blockid.pop();

                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::Jmp(cond_blockid)),
                });
                blocks.push(TempBasicBlock {
                    id: end_blockid,
                    instructions: Vec::new(),
                    branch: None,
                });
                self.symbols.end_scope();
                blocks
            }
            Break { span } => {
                if let Some(&break_dest) = self.current_break_blockid.last() {
                    vec![TempBasicBlock {
                             id: self.new_blockid(),
                             instructions: Vec::new(),
                             branch: Some(Branch::Jmp(break_dest)),
                         }]
                } else {
                    self.diag_engine
                        .report_sema_error("Break outside of a loop/while".to_string(), span);
                }
            }
            Continue { span } => {
                if let Some(&continue_dest) = self.current_continue_blockid.last() {
                    vec![TempBasicBlock {
                             id: self.new_blockid(),
                             instructions: Vec::new(),
                             branch: Some(Branch::Jmp(continue_dest)),
                         }]
                } else {
                    self.diag_engine
                        .report_sema_error("Continue outside of a loop/while".to_string(), span);
                }
            }

            Return { expr, .. } => {
                let expr_name = self.new_temp();
                let expr_id = self.new_blockid();
                let mut blocks = self.generate_expression(expr, expr_id, Some(expr_name.clone()));
                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::Ret(Value::Var(expr_name))),
                });
                blocks
            }
            Expression { expr, .. } => {
                let expr_id = self.new_blockid();
                self.generate_expression(expr, expr_id, None)
            }
            Block { block, .. } => self.generate_block(block),
        }
    }

    fn generate_block(&mut self, block: ast::Block) -> Vec<TempBasicBlock> {
        self.symbols.begin_scope();
        let mut blocks = Vec::new();
        for stmt in block.stmts {
            blocks.extend(self.generate_statement(stmt));
        }
        self.symbols.end_scope();
        blocks
    }

    fn generate_expression(&mut self,
                           expression: ast::Expression,
                           block_id: BasicBlockId,
                           name: Option<String>)
                           -> Vec<TempBasicBlock> {
        use ast::Expression::*;
        use ast::SCBinOpKind;
        use ast::BinOpKind;
        use ast::UnOpKind;
        match expression {
            Assign { id, value, span } => {
                if !self.symbols.is_var_available(&id) {
                    self.diag_engine
                        .report_sema_error(format!("{} is assigned before declaration, maybe \
                                                    use a let statement",
                                                   id),
                                           span);
                }
                let mut blocks = self.generate_expression(*value, block_id, Some(id.clone()));
                if let Some(name) = name {
                    blocks.push(
                        TempBasicBlock::new_from_inst(
                            self.new_blockid(),
                            Instruction::Assign(name, Computation::Value(Value::Var(id)))
                        )
                    );
                }
                blocks
            }
            SCBinOp { kind, lhs, rhs, .. } => {
                let lhs_name = self.new_temp();
                let mut blocks = self.generate_expression(*lhs, block_id, Some(lhs_name.clone()));
                let true_id = self.new_blockid();
                let false_id = self.new_blockid();
                let cont_id = self.new_blockid();

                blocks.push(TempBasicBlock {
                    id: self.new_blockid(),
                    instructions: Vec::new(),
                    branch: Some(Branch::JmpT(Value::Var(lhs_name.clone()), true_id, false_id)),
                });
                match kind {
                    SCBinOpKind::LogicalAnd => {
                        blocks.extend(self.generate_expression(*rhs, true_id, name.clone()));
                        blocks.push(TempBasicBlock {
                            id: self.new_blockid(),
                            instructions: Vec::new(),
                            branch: Some(Branch::Jmp(cont_id)),
                        });

                        let false_comp = Computation::Value(Value::Const(0));

                        blocks.push(TempBasicBlock {
                            id: false_id,
                            instructions: vec![if let Some(name) = name {
                                                   Instruction::Assign(name, false_comp)
                                               } else {
                                                   Instruction::Compute(false_comp)
                                               }],
                            branch: Some(Branch::Jmp(cont_id)),
                        });
                    }
                    SCBinOpKind::LogicalOr => {
                        let true_comp = Computation::Value(Value::Const(1));

                        blocks.push(TempBasicBlock {
                            id: true_id,
                            instructions: vec![if let Some(name) = name.clone() {
                                                   Instruction::Assign(name, true_comp)
                                               } else {
                                                   Instruction::Compute(true_comp)
                                               }],
                            branch: Some(Branch::Jmp(cont_id)),
                        });

                        blocks.extend(self.generate_expression(*rhs, false_id, name));
                        blocks.push(TempBasicBlock {
                            id: self.new_blockid(),
                            instructions: Vec::new(),
                            branch: Some(Branch::Jmp(cont_id)),
                        });
                    }
                }
                blocks.push(TempBasicBlock {
                    id: cont_id,
                    instructions: Vec::new(),
                    branch: None,
                });

                blocks
            }
            BinOp { kind, lhs, rhs, .. } => {
                let lhs_name = self.new_temp();
                let rhs_name = self.new_temp();
                let rhs_id = self.new_blockid();
                let mut blocks = self.generate_expression(*lhs, block_id, Some(lhs_name.clone()));
                blocks.extend(self.generate_expression(*rhs, rhs_id, Some(rhs_name.clone())));
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
                let inst = if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                };
                blocks.push(TempBasicBlock::new_from_inst(self.new_blockid(), inst));
                blocks
            }
            UnOp { kind, expr, .. } => {
                let expr_name = self.new_temp();
                let mut blocks = self.generate_expression(*expr, block_id, Some(expr_name.clone()));
                let comp = match kind {
                    UnOpKind::Plus => Computation::Value(Value::Var(expr_name)),
                    UnOpKind::Minus => Computation::Negate(Value::Var(expr_name)),
                    UnOpKind::LogNot => Computation::LogNot(Value::Var(expr_name)),
                };
                let inst = if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                };
                blocks.push(TempBasicBlock::new_from_inst(self.new_blockid(), inst));
                blocks
            }
            Paren { expr, .. } => self.generate_expression(*expr, block_id, name),
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

                let mut blocks = vec![TempBasicBlock::new(block_id, Vec::new())];
                let mut arg_values = Vec::new();
                for arg in args {
                    let arg_name = self.new_temp();
                    let arg_id = self.new_blockid();
                    blocks.extend(self.generate_expression(arg, arg_id, Some(arg_name.clone())));
                    arg_values.push(Value::Var(arg_name));
                }

                let comp = Computation::FuncCall(func_name.clone(), arg_values);
                let inst = if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                };

                blocks.push(TempBasicBlock::new_from_inst(self.new_blockid(), inst));
                blocks
            }
            Identifier { id, span } => {
                if !self.symbols.is_var_available(&id) {
                    self.diag_engine
                        .report_sema_error(format!("{} is referenced before assignment", id), span);
                }
                let comp = Computation::Value(Value::Var(id.clone()));
                let inst = if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                };
                vec![TempBasicBlock::new_from_inst(block_id, inst)]
            }
            Number { value, .. } => {
                let comp = Computation::Value(Value::Const(value));
                let inst = if let Some(name) = name {
                    Instruction::Assign(name, comp)
                } else {
                    Instruction::Compute(comp)
                };
                vec![TempBasicBlock::new_from_inst(block_id, inst)]
            }
        }
    }
}
