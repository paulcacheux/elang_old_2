use std::collections::HashSet;

use ast;
use ast::{Program, IfKind, Statement, Expression, BinOpKind, UnOpKind};

pub fn generate(program: &Program) -> String {
    let header = "#include <stdlib.h>\n#include <stdio.h>\n\nint main() {\n";
    let footer = "return 0;\n}\n";

    let mut builder = Builder {
        vars: HashSet::new(),
        output: String::new()
    };

    let content = builder.generate_program(program);

    let init: String = builder.vars.into_iter().map(|var| format!("int {} = 0;\n", var)).collect();

    format!("{}{}{}{}", header, init, content, footer)
}

struct Builder {
    vars: HashSet<String>,
    output: String
}

impl Builder {
    fn generate_program(&mut self, program: &Program) -> String {
        program.stmts.iter().map(|stmt| self.generate_statement(stmt)).collect()
    }

    fn generate_statement(&mut self, statement: &Statement) -> String {
        match *statement {
            Statement::Print { ref expr, .. } => {
                format!("printf(\"%d\\n\", {});\n", self.generate_expression(expr))
            },
            Statement::Read { ref target_id, .. } => {
                self.vars.insert(target_id.clone());
                format!("scanf(\"%d\", &{});\n", target_id)
            },
            Statement::If { kind, ref cond, ref if_stmts, ref else_stmts, .. } => {
                let mut if_branch = format!("if ({} {}) {{\n{}}}\n",
                    self.generate_expression(cond),
                    match kind {
                        IfKind::Negative => " < 0",
                        IfKind::Positive => " > 0",
                        IfKind::Zero => " == 0"
                    },
                    if_stmts.iter().map(|stmt| self.generate_statement(stmt)).collect::<String>()
                );

                if let &Some(ref else_stmts) = else_stmts {
                    if_branch += &format!("else {{\n{}}}\n",
                        else_stmts.iter().map(|stmt| self.generate_statement(stmt)).collect::<String>()
                    );
                }
                if_branch
            },
            Statement::Loop { ref stmts, .. } => {
                format!("for(;;) {{\n{}}}\n",
                    stmts.iter().map(|stmt| self.generate_statement(stmt)).collect::<String>()
                )
            },
            Statement::Break { .. } => {
                format!("break;\n")
            },
            Statement::Assign { ref target_id, ref value, .. } => {
                self.vars.insert(target_id.clone());
                format!("{} = {};\n", target_id, self.generate_expression(value))
            }
        }
    }

    fn generate_expression(&mut self, expression: &Expression) -> String {
        match *expression {
            Expression::BinOp { kind, ref lhs, ref rhs, .. } => {
                format!("({} {} {})",
                    self.generate_expression(lhs),
                    match kind {
                        BinOpKind::Add => "+",
                        BinOpKind::Sub => "-",
                        BinOpKind::Times => "*",
                        BinOpKind::Divide => "/",
                        BinOpKind::Modulo => "%"
                    },
                    self.generate_expression(rhs)
                )
            },
            Expression::UnOp { kind, ref expr, .. } => {
                format!("({}({}))",
                    match kind {
                        UnOpKind::Positive => "+",
                        UnOpKind::Negative => "-"
                    },
                    self.generate_expression(expr)
                )
            },
            Expression::Paren { ref expr, .. } => {
                format!("({})", self.generate_expression(expr))
            },
            Expression::Identifier { ref id, .. } => {
                self.vars.insert(id.clone());
                format!("{}", id)
            },
            Expression::Number { value, .. } => {
                format!("({})", value)
            }
        }
    }
}
