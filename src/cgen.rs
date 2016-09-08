use itertools::Itertools;

use ir::{BasicBlock, Branch, Instruction, Function, Value};

pub fn generate(func: Function) -> String {
    let header = "#include <stdlib.h>\n#include <stdio.h>\n\nint main() {\n";
    let footer = "return 0;\n}\n";

    let mut content = String::new();
    for block in func.blocks {
        content.push_str(&generate_block(block));
    }

    let var_len = func.vars.len();
    let init = if var_len > 0 {
        format!("long {};\n",
                func.vars.into_iter().map(|var| format!("{} = 0", var)).join(", "))
    } else {
        String::new()
    };
    format!("{}{}{}{}", header, init, content, footer)
}

fn generate_block(block: BasicBlock) -> String {
    let mut result = format!("{}:\n", block.name);
    for instruction in block.instructions {
        result.push_str(&generate_instruction(&instruction));
    }
    result.push_str(&generate_branch(&block.branch));
    result
}

fn generate_instruction(instruction: &Instruction) -> String {
    match *instruction {
        Instruction::Assign(ref dest, ref src) => {
            format!("\t{} = {};\n", dest, generate_value(src))
        }
        Instruction::Add(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} + {};\n",
                    dest,
                    generate_value(lhs),
                    generate_value(rhs))
        }
        Instruction::Sub(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} - {};\n",
                    dest,
                    generate_value(lhs),
                    generate_value(rhs))
        }
        Instruction::Mul(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} * {};\n",
                    dest,
                    generate_value(lhs),
                    generate_value(rhs))
        }
        Instruction::Div(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} / {};\n",
                    dest,
                    generate_value(lhs),
                    generate_value(rhs))
        }
        Instruction::Mod(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} % {};\n",
                    dest,
                    generate_value(lhs),
                    generate_value(rhs))
        }
        Instruction::Negate(ref dest, ref value) => {
            format!("\t{} = -{};\n", dest, generate_value(value))
        }
        Instruction::Print(ref value) => {
            format!("\tprintf(\"%ld\\n\", {});\n", generate_value(value))
        }
        Instruction::Read(ref dest) => format!("\tscanf(\"%ld\", &{});\n", dest),
    }
}

fn generate_value(value: &Value) -> String {
    match *value {
        Value::Const(value) => format!("{}l", value),
        Value::Var(ref name) => format!("{}", name),
    }
}

fn generate_branch(branch: &Branch) -> String {
    match *branch {
        Branch::Jmp(ref dest) => format!("\tgoto {};\n", dest),
        Branch::JmpP(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({} > 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    generate_value(cond),
                    true_label,
                    false_label)
        }
        Branch::JmpZ(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({} == 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    generate_value(cond),
                    true_label,
                    false_label)
        }
        Branch::JmpN(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({} < 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    generate_value(cond),
                    true_label,
                    false_label)
        }
        Branch::Ret => format!("\treturn 0;\n"),
    }
}
