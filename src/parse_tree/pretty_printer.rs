use std::iter;
use parse_tree::{Program, Type, Function, Param, Block, Statement, Expression, SCBinOpKind,
                 BinOpKind, UnOpKind};

pub fn print(program: &Program) -> String {
    let mut pp = PrettyPrinter {
        current_tab: 0,
        output: String::new(),
    };
    pp.print_program(program);
    pp.output
}

pub struct PrettyPrinter {
    current_tab: usize,
    output: String,
}

impl PrettyPrinter {
    fn print_tab(&mut self) {
        self.output.push_str(&iter::repeat('\t').take(self.current_tab).collect::<String>());
    }

    pub fn print_program(&mut self, program: &Program) {
        for function in &program.functions {
            self.print_function(&function);
            self.output.push('\n');
        }
    }

    pub fn print_type(&mut self, ty: &Type) {
        match *ty {
            Type::Unit { .. } => self.output.push_str("()"),
            Type::Id { ref id, .. } => self.output.push_str(id),
            Type::Ref { ref sub_ty, .. } => {
                self.output.push('&');
                self.print_type(sub_ty);
            }
        }
    }

    pub fn print_function(&mut self, function: &Function) {
        self.output.push_str("fn ");
        self.output.push_str(&function.name);
        self.output.push('(');

        if !function.params.is_empty() {
            for param in &function.params {
                self.print_param(param);
                self.output.push_str(", ");
            }
            self.output.pop();
            self.output.pop();
        }

        self.output.push_str(") -> ");
        self.print_type(&function.ret_ty);
        self.output.push('\n');
        self.print_block(&function.block);
        self.output.push('\n');
    }

    pub fn print_param(&mut self, param: &Param) {
        self.output.push_str(&param.name);
        self.output.push_str(": ");
        self.print_type(&param.ty);
    }

    pub fn print_block(&mut self, block: &Block) {
        self.print_tab();
        self.output.push_str("{\n");
        self.current_tab += 1;
        for stmt in &block.stmts {
            self.print_statement(&stmt);
        }
        self.current_tab -= 1;
        self.print_tab();
        self.output.push_str("}\n");
    }

    pub fn print_statement(&mut self, stmt: &Statement) {
        match *stmt {
            Statement::Let { ref id, ref ty, ref expr, .. } => {
                self.print_tab();
                self.output.push_str(&format!("let {}", id));
                if let Some(ref ty) = *ty {
                    self.output.push_str(": ");
                    self.print_type(ty);
                }
                self.output.push_str(" = ");
                self.print_expression(expr);
                self.output.push_str(";\n");
            }
            Statement::If { ref cond, ref if_stmt, ref else_stmt, .. } => {
                self.print_tab();
                self.output.push_str("if ");
                self.print_expression(cond);
                self.output.push('\n');
                self.current_tab += 1;
                self.print_statement(if_stmt);
                self.current_tab -= 1;
                if let &Some(ref else_stmt) = else_stmt {
                    self.print_tab();
                    self.output.push_str("else\n");
                    self.current_tab += 1;
                    self.print_statement(else_stmt);
                    self.current_tab -= 1;
                }
            }
            Statement::Loop { ref stmt, .. } => {
                self.print_tab();
                self.output.push_str("loop\n");
                self.current_tab += 1;
                self.print_statement(stmt);
                self.current_tab -= 1;
            }
            Statement::While { ref cond, ref stmt, .. } => {
                self.print_tab();
                self.output.push_str("while ");
                self.print_expression(cond);
                self.output.push('\n');
                self.current_tab += 1;
                self.print_statement(stmt);
                self.current_tab -= 1;
            }
            Statement::Break { .. } => {
                self.print_tab();
                self.output.push_str("break;\n");
            }
            Statement::Continue { .. } => {
                self.print_tab();
                self.output.push_str("continue;\n");
            }
            Statement::Return { ref expr, .. } => {
                self.print_tab();
                self.output.push_str("return ");
                self.print_expression(expr);
                self.output.push_str(";\n");
            }
            Statement::Expression { ref expr, .. } => {
                self.print_tab();
                self.print_expression(expr);
                self.output.push_str(";\n");
            }
            Statement::Block { ref block, .. } => {
                self.current_tab -= 1;
                self.print_block(block);
                self.current_tab += 1;
            }
        }
    }

    pub fn print_expression(&mut self, expression: &Expression) {
        match *expression {
            Expression::Assign { ref id, ref value, .. } => {
                self.output.push_str(&format!("{} = ", id));
                self.print_expression(value);
            }
            Expression::SCBinOp { kind, ref lhs, ref rhs, .. } => {
                self.print_expression(lhs);
                self.output.push_str(match kind {
                    SCBinOpKind::LogicalAnd => " && ",
                    SCBinOpKind::LogicalOr => " || ",
                });
                self.print_expression(rhs);
            }
            Expression::BinOp { kind, ref lhs, ref rhs, .. } => {
                self.print_expression(lhs);
                self.output.push_str(match kind {
                    BinOpKind::Add => " + ",
                    BinOpKind::Sub => " - ",
                    BinOpKind::Mul => " * ",
                    BinOpKind::Div => " / ",
                    BinOpKind::Mod => " % ",
                    BinOpKind::Less => " < ",
                    BinOpKind::LessEq => " <= ",
                    BinOpKind::Greater => " > ",
                    BinOpKind::GreaterEq => " >= ",
                    BinOpKind::Equal => " == ",
                    BinOpKind::NotEqual => " != ",
                });
                self.print_expression(rhs);
            }
            Expression::UnOp { kind, ref expr, .. } => {
                self.output.push_str(match kind {
                    UnOpKind::Plus => "+",
                    UnOpKind::Minus => "-",
                    UnOpKind::LogNot => "!",
                });
                self.print_expression(expr);
            }
            Expression::Paren { ref expr, .. } => {
                self.output.push('(');
                self.print_expression(expr);
                self.output.push(')');
            }
            Expression::FuncCall { ref func_name, ref args, .. } => {
                self.output.push_str(&func_name);
                self.output.push('(');

                if !args.is_empty() {
                    for arg in args {
                        self.print_expression(arg);
                        self.output.push_str(", ");
                    }
                    self.output.pop();
                    self.output.pop();
                }

                self.output.push(')');
            }
            Expression::Identifier { ref id, .. } => {
                self.output.push_str(&id);
            }
            Expression::Number { value, .. } => {
                self.output.push_str(&format!("{}", value));
            }
        }
    }
}
