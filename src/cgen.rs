use itertools::Itertools;

use std::collections::HashSet;

use ir::{BasicBlock, Branch, Instruction, Function};

pub fn generate(func: Function) -> String {
    let header = "#include <stdlib.h>\n#include <stdio.h>\n\nint main() {\n";
    let footer = "return 0;\n}\n";

    let mut content = String::new();
    for block in func.blocks {
        content.push_str(&generate_block(block));
    }

    let init = format!("long {};\n",
                       func.vars.into_iter().map(|var| format!("{} = 0", var)).join(", "));
    format!("{}{}{}{}", header, init, content, footer)
}

fn generate_block(block: BasicBlock) -> String {
    let mut result = format!("{}:\n", block.name);
    for instruction in block.instructions {
        result.push_str(&generate_instruction(instruction));
    }
    result.push_str(&generate_branch(block.branch));
    result
}

fn generate_instruction(instruction: Instruction) -> String {
    match instruction {
        Instruction::SetConst(ref dest, value) => format!("\t{} = {};\n", dest, value),
        Instruction::Assign(ref dest, ref src) => format!("\t{} = {};\n", dest, src),
        Instruction::Add(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} + {};\n", dest, lhs, rhs)
        }
        Instruction::Sub(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} - {};\n", dest, lhs, rhs)
        }
        Instruction::Mul(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} * {};\n", dest, lhs, rhs)
        }
        Instruction::Div(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} / {};\n", dest, lhs, rhs)
        }
        Instruction::Mod(ref dest, ref lhs, ref rhs) => {
            format!("\t{} = {} % {};\n", dest, lhs, rhs)
        }
        Instruction::Negate(ref dest, ref value) => format!("\t{} = -{};\n", dest, value),
        Instruction::Print(ref value) => format!("\tprintf(\"%ld\\n\", {});\n", value),
        Instruction::Read(ref dest) => format!("\tscanf(\"%ld\", &{});\n", dest),
    }
}

fn generate_branch(branch: Branch) -> String {
    match branch {
        Branch::Jmp(ref dest) => format!("\tgoto {};\n", dest),
        Branch::JmpP(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({} > 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    cond,
                    true_label,
                    false_label)
        }
        Branch::JmpZ(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({} == 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    cond,
                    true_label,
                    false_label)
        }
        Branch::JmpN(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({} < 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    cond,
                    true_label,
                    false_label)
        }
        Branch::Ret => format!("\treturn 0;\n"),
    }
}
