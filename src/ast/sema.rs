use std::collections::HashMap;

use diagnostic::DiagnosticEngine;
use ast;
use parse_tree as pt;
use ty::Type;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { scopes: Vec::new() }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn insert(&mut self, name: String, ty: Type) -> Option<Type> {
        // return Some(old_ty) if name is already defined in this scope, else None

        scopes.last_mut().unwrap().insert(name, ty)
    }

    pub fn get(&self, name: &String) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ref ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
struct TyppedName {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone)]
pub struct Sema<'a> {
    symbol_table: SymbolTable,
    diag_engine: DiagnosticEngine<'a>,

    current_ret_ty: Type,
    loop_level: usize,
}

impl Sema<'a> {
    pub fn sema_program(program: pt::Program,
                        diag_engine: &'a DiagnosticEngine<'a>)
                        -> ast::Program {
        let sema = Sema {
            symbol_table: SymbolTable::new(),
            diag_engine: diag_engine,
            current_ret_ty: Type::Unit,
            loop_level: 0,
        };

        sema.symbol_table.begin_scope();
        let functions = program.functions.into_iter().map(sema.sema_function).collect();
        sema.symbol_table.end_scope();

        ast::Program { functions: functions }
    }

    fn sema_function(&mut self, function: pt::Function) -> ast::Function {
        let name = function.name;
        let param_types = function.params.iter().map(|param| param.ty.clone()).collect();
        let ty = Type::Function(Box::new(function.ret_ty), params_ty);

        if let Some(old_ty) = self.symbol_table.insert(name.clone(), ty.clone()) {
            self.diag_engine
                .report_sema_error(format!("\'{}\' function is already defined (with type \
                                            \'{}\')",
                                           name,
                                           old_ty),
                                   function.span);
        }

        let mut param_stmts = Vec::with_capacity(function.params.len());
        for (index, param) in function.params
            .into_iter()
            .enumerate() {
            if let Some(old_ty) = self.symbol_table.insert(param.name.clone(), param.ty.clone()) {
                diag_engine.report_sema_error(format!("\'{}\' param is already defined (with \
                                                       type \'{}\')",
                                                      param.name,
                                                      old_ty),
                                              param.span);
            }

            ast::Statement::Let {
                id: param.name,
                ty: param.ty,
                comp: Computation::Value(Value::Argument(index)),
            }
        }

        let mut func_scope = ast::Scope { stmts: param_stmts };

        let cont_scope = self.sema_block(function.block);
        func_scope.stmts.push(ast::Statement::Scope { scope: cont_scope });

        ast::Function {
            name: name,
            ty: ty,
            scope: func_scope,
        }
    }

    fn sema_block(&mut self, block: pt::Block) -> ast::Scope {
        self.symbol_table.begin_scope();
        let mut scope_stmts = Vec::new();
        for stmt in blocks.stmts {
            scope_stmts.append(self.sema_statement(stmt));
        }
        self.symbol_table.end_scope();
    }

    fn sema_statement(&mut self, statement: pt::Statement) -> Vec<ast::Statement> {
        use pt::Statement;
        match statement {
            Statement::Let { id, ty, expr, span } => {
                let (mut stmts, expr_tn) = self.sema_expression(expr);
                let ty = if let Some(ty) = ty.map(self.sema_type) {
                    if ty != expr_tn.ty {
                        self.diag_engine
                            .report_sema_error(format!("Definition of {}({}) with an \'{}\' \
                                                        value",
                                                       id,
                                                       ty,
                                                       expr_tn.ty),
                                               span);
                    }
                } else {
                    expr_tn.ty
                };

                if let Some(old_ty) = self.symbol_table.insert(id.clone(), ty.clone()) {
                    self.diag_engine
                        .report_sema_error(format!("\'{}\' is already defined (with type \'{}\')",
                                                   id,
                                                   old_ty),
                                           span: Span);
                }

                stmts.push(ast::Statement::Let {
                    id: id,
                    ty: ty,
                    comp: Computation::Value(Value::Var(expr_tn.name)),
                });
                stmts
            }
            Statement::If { cond, if_stmt, else_stmt, .. } => {
                let cond_span = cond.span();
                let (mut stmts, expr_tn) = self.sema_expression(cond);
                if expr_tn.ty != Type::Bool {
                    self.diag_engine
                        .report_sema_error(format!("Mismatching condition type (given: {}, \
                                                    expected: {})",
                                                   expr_tn.ty,
                                                   Type::Bool),
                                           cond_span);
                }

                self.symbol_table.begin_scope();
                let if_scope = ast::Scope { stmts: self.sema_statement(if_stmt) };
                self.symbol_table.end_scope();

                let else_scope = else_stmt.map(|stmt| {
                    self.symbol_table.begin_scope();
                    let scope = ast::Scope { stmts: self.sema_statement(stmt) };
                    self.symbol_table.end_scope();
                    Some(scope)
                });
                stmts.push(ast::Statement::If {
                    cond: Value::Var(expr_tn.name),
                    if_scope: if_scope,
                    else_scope: else_scope,
                });
                stmts
            }
            Statement::Loop { stmt, .. } => {
                self.symbol_table.begin_scope();
                self.loop_level += 1;
                let scope = ast::Scope { scope: self.sema_statement(stmt) };
                self.loop_level -= 1;
                self.symbol_table.end_scope();
                vec![ast::Statement::Loop { scope: scope }]
            }
            Statement::While { cond, stmt, .. } => {
                let cond_span = cond.span();
                let (mut stmts, expr_tn) = self.sema_expression(cond);
                if expr_tn.ty != Type::Bool {
                    self.diag_engine
                        .report_sema_error(format!("Mismatching condition type (given: {}, \
                                                    expected: {})",
                                                   expr_tn.ty,
                                                   Type::Bool),
                                           cond_span);
                }

                self.symbol_table.begin_scope();
                self.loop_level += 1;
                let scope = ast::Scope { scope: self.sema_statement(stmt) };
                self.loop_level -= 1;
                self.symbol_table.end_scope();
                stmts.push(ast::Statement::While {
                    cond: Value::Var(expr_tn.name),
                    scope: scope,
                });
                stmts
            }
            Statement::Break { span } => {
                if self.loop_level == 0 {
                    self.diag_engine
                        .report_sema_error("Can't break outside of a loop/while", span);
                }
                vec![ast::Statement::Break]
            }
            Statement::Continue { .. } => {
                if self.loop_level == 0 {
                    self.diag_engine
                        .report_sema_error("Can't continue outside of a loop/while", span);
                }
                vec![ast::Statement::Continue]
            }
            Statement::Return { expr, span } => {
                let expr_span = expr.span();
                let (mut stmts, expr_tn) = self.sema_expression(expr);
                if expr_tn.ty != self.current_ret_ty {
                    self.diag_engine
                        .report_sema_error(format!("Mismatching return type (given: {}, \
                                                    expected: {})",
                                                   expr_tn.ty,
                                                   self.current_ret_ty),
                                           expr_span)
                }

                stmts.push(ast::Statement::Return { val: Value::Var(expr_tn.name) });
                stmts
            }
            Statement::Expression { expr, span } => {
                let (stmts, _) = self.sema_expression(expr);
                stmts
            }
            Statement::Block { block } => {
                vec![ast::Statement::Scope { scope: self.sema_block(block) }]
            }
        }
    }

    fn sema_expression(&mut self, expression: pt::Expression) -> (Vec<ast::Statement>, TyppedName) {

    }
}
