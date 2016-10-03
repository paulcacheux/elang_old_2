use itertools::Itertools;

use ast::{Program, Type, Function, Param, Block, Statement, Expression, BinOpKind, UnOpKind};

pub fn generate_program(program: Program) -> String {
    let header = concat!(
        "#include <stdlib.h>\n",
        "#include <stdio.h>\n\n",
        "void print(int x) { printf(\"%d\", x); }\n\n",
        "void printchar(char x) { printf(\"%c\", x); }\n\n",
        "void println(int x) { printf(\"%d\\n\", x); }\n\n",
        "int read(void) { int x; scanf(\"%d\", &x); return x; }\n\n"
    );

    let content = program.functions.into_iter().map(generate_function).join("\n\n");

    format!("{}\n{}", header, content)
}

fn generate_function(function: Function) -> String {
    format!(
        "{} {}({})\n{}",
        generate_type(function.return_ty),
        function.name,
        function.parameters.into_iter().map(generate_param).join(", "),
        generate_block(function.block)
    )
}

fn generate_param(param: Param) -> String {
    format!(
        "{} {}",
        generate_type(param.ty),
        param.name,
    )
}

fn generate_block(block: Block) -> String {
    format!(
        "{{\n{}\n}}",
        block.statements.into_iter().map(generate_statement).join("")
    )
}

fn generate_statement(statement: Statement) -> String {
    match statement {
        Statement::Let { identifier, ty, expression, .. } => {
            format!(
                "{} {} = {};\n",
                generate_type(ty),
                identifier,
                generate_expression(expression)
            )
        }
        Statement::If { condition, if_statement, else_statement, .. } => {
            let mut content = format!(
                "if ({})\n{}\n",
                generate_expression(condition),
                generate_statement(*if_statement)
            );
            if let Some(stmt) = else_statement {
                content.push_str(&format!(
                    "else\n{}\n",
                    generate_statement(*stmt)
                ))
            }
            content
        }
        Statement::Loop { statement, .. } => {
            format!(
                "while (1) {{{}}}\n",
                generate_statement(*statement)
            )
        }
        Statement::While { condition, statement, .. } => {
            format!(
                "while ({})\n{}\n",
                generate_expression(condition),
                generate_statement(*statement),
            )
        }
        Statement::Break { .. } => {
            "break;\n".to_string()
        }
        Statement::Continue { .. } => {
            "continue;\n".to_string()
        }
        Statement::Return { expression, .. } => {
            format!(
                "return {};\n",
                generate_expression(expression)
            )
        }
        Statement::Expression { expression, .. } => {
            format!(
                "{};\n",
                generate_expression(expression)
            )
        }
        Statement::Block { block, .. } => {
            generate_block(block)
        }
    }
}

fn generate_expression(expression: Expression) -> String {
    match expression {
        Expression::BinOp { kind, lhs, rhs, .. } => {
            format!(
                "({} {} {})",
                generate_expression(*lhs),
                match kind {
                    BinOpKind::Assign => "=",
                    BinOpKind::Add => "+",
                    BinOpKind::Sub => "-",
                    BinOpKind::Mul => "*",
                    BinOpKind::Div => "/",
                    BinOpKind::Mod => "%",
                    BinOpKind::Less => "<",
                    BinOpKind::LessEq => "<=",
                    BinOpKind::Greater => ">",
                    BinOpKind::GreaterEq => ">=",
                    BinOpKind::Equal => "==",
                    BinOpKind::NotEqual => "!=",
                    BinOpKind::LogicalAnd => "&&",
                    BinOpKind::LogicalOr => "||",
                },
                generate_expression(*rhs)
            )
        }
        Expression::UnOp { kind, expression, .. } => {
            format!(
                "({}({}))",
                match kind {
                    UnOpKind::Plus => "+",
                    UnOpKind::Minus => "-",
                    UnOpKind::LogicalNot => "!",
                },
                generate_expression(*expression)
            )
        }
        Expression::Paren { expression, .. } => {
            format!("({})", generate_expression(*expression))
        }
        Expression::FuncCall { function_name, arguments, .. } => {
            format!(
                "{}({})",
                function_name,
                arguments.into_iter().map(generate_expression).join(", ")
            )
        }
        Expression::L2R { expression, .. } => {
            format!("({})", generate_expression(*expression)) // same as paren
        }
        Expression::Cast { expression, ty, .. } => {
            format!(
                "({})({})",
                generate_type(ty),
                generate_expression(*expression)
            )
        }
        Expression::Identifier { identifier, .. } => {
            format!("({})", identifier)
        }
        Expression::IntLit { value, .. } => {
            format!("{}", value)
        }
        Expression::UIntLit { value, .. } => {
            format!("{}", value)
        }
        Expression::DoubleLit { value, .. } => {
            format!("{}", value)
        }
        Expression::BoolLit { value, .. } => {
            format!("{}", if value { 1 } else { 0 })
        }
        Expression::CharLit { value, .. } => {
            format!("{:?}", value as char)
        }
        Expression::UnitLit { .. } => {
            "void".to_string()
        }
    }
}

fn generate_type(ty: Type) -> String {
    match ty {
        Type::Unit => "void".to_string(),
        Type::Int => "int".to_string(),
        Type::UInt => "unsigned".to_string(),
        Type::Double => "double".to_string(),
        Type::Char => "char".to_string(),
        Type::Bool => "bool".to_string(),
        Type::LVal(sub_ty) => generate_type(*sub_ty),
        Type::Ptr(sub_ty) => format!("*{}", generate_type(*sub_ty)),
        Type::Function(_, _) => unimplemented!()
    }
}
