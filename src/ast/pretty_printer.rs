use std::iter;
use ast::{Program, Type, Function, Param, Block, Statement, Expression, BinOpKind,
          UnOpKind};

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
        self.output.push_str(&ty.to_string());
    }

    pub fn print_function(&mut self, function: &Function) {
        self.output.push_str("fn ");
        self.output.push_str(&function.name);
        self.output.push('(');

        if !function.parameters.is_empty() {
            for param in &function.parameters {
                self.print_param(param);
                self.output.push_str(", ");
            }
            self.output.pop();
            self.output.pop();
        }

        self.output.push_str(") -> ");
        self.print_type(&function.return_ty);
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
        for stmt in &block.statements {
            self.print_statement(&stmt);
        }
        self.current_tab -= 1;
        self.print_tab();
        self.output.push_str("}\n");
    }

    pub fn print_statement(&mut self, stmt: &Statement) {
        match *stmt {
            Statement::Let { ref identifier, ref ty, ref expression, .. } => {
                self.print_tab();
                self.output.push_str(&format!("let {}", identifier));
                self.output.push_str(": ");
                self.print_type(ty);
                self.output.push_str(" = ");
                self.print_expression(expression);
                self.output.push_str(";\n");
            }
            Statement::If {
                ref condition,
                ref if_statement,
                ref else_statement,
                ..
            } => {
                self.print_tab();
                self.output.push_str("if ");
                self.print_expression(condition);
                self.output.push('\n');
                self.current_tab += 1;
                self.print_statement(if_statement);
                self.current_tab -= 1;
                if let &Some(ref else_statement) = else_statement {
                    self.print_tab();
                    self.output.push_str("else\n");
                    self.current_tab += 1;
                    self.print_statement(else_statement);
                    self.current_tab -= 1;
                }
            }
            Statement::Loop { ref statement, .. } => {
                self.print_tab();
                self.output.push_str("loop\n");
                self.current_tab += 1;
                self.print_statement(statement);
                self.current_tab -= 1;
            }
            Statement::While { ref condition, ref statement, .. } => {
                self.print_tab();
                self.output.push_str("while ");
                self.print_expression(condition);
                self.output.push('\n');
                self.current_tab += 1;
                self.print_statement(statement);
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
            Statement::Return { ref expression, .. } => {
                self.print_tab();
                self.output.push_str("return ");
                self.print_expression(expression);
                self.output.push_str(";\n");
            }
            Statement::Expression { ref expression, .. } => {
                self.print_tab();
                self.print_expression(expression);
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
            Expression::BinOp { kind, ref lhs, ref rhs, .. } => {
                self.print_expression(lhs);
                self.output.push_str(match kind {
                    BinOpKind::Assign => " = ",
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
                    BinOpKind::LogicalAnd => " && ",
                    BinOpKind::LogicalOr => " || ",
                });
                self.print_expression(rhs);
            }
            Expression::UnOp { kind, ref expression, .. } => {
                self.output.push_str(match kind {
                    UnOpKind::Plus => "+",
                    UnOpKind::Minus => "-",
                    UnOpKind::LogicalNot => "!",
                });
                self.print_expression(expression);
            }
            Expression::Paren { ref expression, .. } => {
                self.output.push('(');
                self.print_expression(expression);
                self.output.push(')');
            }
            Expression::FuncCall { ref function_name, ref arguments, .. } => {
                self.output.push_str(&function_name);
                self.output.push('(');

                if !arguments.is_empty() {
                    for arg in arguments {
                        self.print_expression(arg);
                        self.output.push_str(", ");
                    }
                    self.output.pop();
                    self.output.pop();
                }

                self.output.push(')');
            }
            Expression::L2R { ref expression, .. } => {
                self.output.push_str("l2r(");
                self.print_expression(expression);
                self.output.push_str(")");
            }
            Expression::Cast { ref expression, ref ty, .. } => {
                self.print_expression(expression);
                self.output.push_str(" as ");
                self.print_type(ty);
            }
            Expression::Identifier { ref identifier, .. } => {
                self.output.push_str(&identifier);
            }
            Expression::IntLit { value, .. } => {
                self.output.push_str(&format!("{}", value));
            }
            Expression::UIntLit { value, .. } => {
                self.output.push_str(&format!("{}", value));
            }
            Expression::DoubleLit { value, .. } => {
                self.output.push_str(&format!("{}", value));
            }
            Expression::BoolLit { value, .. } => {
                self.output.push_str(&format!("{:?}", value));
            }
            Expression::CharLit { value, .. } => {
                self.output.push_str(&format!("{:?}", value));
            }
            Expression::UnitLit { .. } => {
                self.output.push_str("()");
            }
        }
    }
}
