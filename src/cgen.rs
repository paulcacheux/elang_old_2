use itertools::Itertools;

use ir::{BasicBlock, Branch, Instruction, Computation, Function, Value};

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
        Instruction::Assign(ref dest, ref comp) => {
            format!("\t{} = {};\n", dest, generate_computation(comp))
        }
        Instruction::Print(ref comp) => {
            format!("\tprintf(\"%ld\\n\", {});\n", generate_computation(comp))
        }
        Instruction::Read(ref dest) => format!("\tscanf(\"%ld\", &{});\n", dest),
    }
}

fn generate_computation(computation: &Computation) -> String {
    match *computation {
        Computation::Value(ref val) => format!("({})", generate_value(val)),
        Computation::Add(ref lhs, ref rhs) => {
            format!("({} + {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::Sub(ref lhs, ref rhs) => {
            format!("({} - {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::Mul(ref lhs, ref rhs) => {
            format!("({} * {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::Div(ref lhs, ref rhs) => {
            format!("({} / {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::Mod(ref lhs, ref rhs) => {
            format!("({} % {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::CmpLess(ref lhs, ref rhs) => {
            format!("({} < {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::CmpLessEq(ref lhs, ref rhs) => {
            format!("({} <= {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::CmpGreater(ref lhs, ref rhs) => {
            format!("({} > {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::CmpGreaterEq(ref lhs, ref rhs) => {
            format!("({} >= {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::CmpEq(ref lhs, ref rhs) => {
            format!("({} == {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::CmpNotEq(ref lhs, ref rhs) => {
            format!("({} != {})", generate_value(lhs), generate_value(rhs))
        }
        Computation::LogNot(ref value) => format!("(!({}))", generate_value(value)),
        Computation::Negate(ref value) => format!("(-({}))", generate_value(value)),

    }
}

fn generate_value(value: &Value) -> String {
    match *value {
        Value::Const(value) => format!("{}l", value),
        Value::Var(ref name) => name.clone(),
    }
}

fn generate_branch(branch: &Branch) -> String {
    match *branch {
        Branch::Jmp(ref dest) => format!("\tgoto {};\n", dest),
        Branch::JmpT(ref cond, ref true_label, ref false_label) => {
            format!("\tif ({}) {{ goto {}; }} else {{ goto {}; }}\n",
                    generate_value(cond),
                    true_label,
                    false_label)
        }
        Branch::Ret => String::from("\treturn 0;\n"),
    }
}
