use itertools::Itertools;

use ir::{Module, Function, BasicBlockId, BasicBlock, Branch, Instruction, Computation, Value};

pub fn generate(module: Module) -> String {
    let mut result = "#include <stdlib.h>\n#include <stdio.h>\n\n".to_string();

    result.push_str("long print(long val) {\n\tprintf(\"%ld\\n\", val);\n\treturn 0;\n}\n");
    result.push_str("long read(void) {\n\tlong res;\n\tscanf(\"%ld\", &res);\n\treturn res;\n}\n");

    for func in module.functions {
        result.push_str(&generate_function(func));
    }
    result
}

fn generate_function(func: Function) -> String {
    let Function { name, params, vars, blocks } = func;

    let mut content = String::new();
    for block in blocks {
        content.push_str(&generate_block(block));
    }

    let var_len = vars.len();
    let init = if var_len > 0 {
        format!("long {};\n",
                vars.into_iter()
                    .filter(|var| !params.contains(var))
                    .map(|var| format!("{} = 0", var))
                    .join(", "))
    } else {
        String::new()
    };

    let ret_type = if name == "main" { "int" } else { "long" };

    format!("{} {}({}) {{\n {}{} }}\n",
            ret_type,
            name,
            params.iter().map(|param| format!("long {}", param)).join(", "),
            init,
            content)
}

fn generate_block(block: BasicBlock) -> String {
    let mut result = format!("{}:\n", generate_basic_block_id(block.id));
    for instruction in block.instructions {
        result.push_str(&generate_instruction(&instruction));
    }
    result.push_str(&generate_branch(&block.branch));
    result
}

fn generate_basic_block_id(id: BasicBlockId) -> String {
    format!("label{}", id.0)
}

fn generate_instruction(instruction: &Instruction) -> String {
    match *instruction {
        Instruction::Assign(ref dest, ref comp) => {
            format!("\t{} = {};\n", dest, generate_computation(comp))
        }
        Instruction::Compute(ref comp) => format!("\t{};\n", generate_computation(comp)),
    }
}

fn generate_computation(computation: &Computation) -> String {
    match *computation {
        Computation::Value(ref val) => format!("({})", generate_value(val)),
        Computation::FuncCall(ref func_name, ref params_value) => {
            format!("{}({})",
                    func_name,
                    params_value.iter().map(|val| generate_value(val)).join(", "))
        }
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
        Branch::Jmp(dest) => format!("\tgoto {};\n", generate_basic_block_id(dest)),
        Branch::JmpT(ref cond, true_blockid, false_blockid) => {
            format!("\tif ({}) {{ goto {}; }} else {{ goto {}; }}\n",
                    generate_value(cond),
                    generate_basic_block_id(true_blockid),
                    generate_basic_block_id(false_blockid))
        }
        Branch::Ret(ref val) => format!("\treturn {};\n", generate_value(val)),
    }
}
