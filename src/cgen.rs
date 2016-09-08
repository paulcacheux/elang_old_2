use itertools::Itertools;

use std::collections::HashSet;

use ir::{BasicBlock, Branch, Instruction};

pub fn generate(blocks: Vec<BasicBlock>) -> String {
    let header = "#include <stdlib.h>\n#include <stdio.h>\n\nint main() {\n";
    let footer = "return 0;\n}\n";


    let mut builder = Builder {
        vars: HashSet::new()
    };

    let mut content = String::new();
    for block in blocks {
        content.push_str(&builder.generate_block(block));
    }

    let init = format!("long {};\n", builder.vars.into_iter().join(", "));
    format!("{}{}{}{}", header, init, content, footer)
}

struct Builder {
    vars: HashSet<String>
}

impl Builder {
    fn generate_block(&mut self, block: BasicBlock) -> String {
        let mut result = format!("{}:\n", block.name);
        for instruction in block.instructions {
            result.push_str(&self.generate_instruction(instruction));
        }
        result.push_str(&self.generate_branch(block.branch));
        result
    }

    fn generate_instruction(&mut self, instruction: Instruction) -> String {
        match instruction {
            Instruction::SetConst(ref dest, value) => {
                self.vars.insert(dest.clone());
                format!("\t{} = {};\n", dest, value)
            },
            Instruction::Assign(ref dest, ref src) => {
                self.vars.insert(dest.clone());
                self.vars.insert(src.clone());
                format!("\t{} = {};\n", dest, src)
            },
            Instruction::Add(ref dest, ref lhs, ref rhs) => {
                self.vars.insert(lhs.clone());
                self.vars.insert(rhs.clone());
                format!("\t{} = {} + {};\n", dest, lhs, rhs)
            },
            Instruction::Sub(ref dest, ref lhs, ref rhs) => {
                self.vars.insert(lhs.clone());
                self.vars.insert(rhs.clone());
                format!("\t{} = {} - {};\n", dest, lhs, rhs)
            },
            Instruction::Mul(ref dest, ref lhs, ref rhs) => {
                self.vars.insert(lhs.clone());
                self.vars.insert(rhs.clone());
                format!("\t{} = {} * {};\n", dest, lhs, rhs)
            },
            Instruction::Div(ref dest, ref lhs, ref rhs) => {
                self.vars.insert(lhs.clone());
                self.vars.insert(rhs.clone());
                format!("\t{} = {} / {};\n", dest, lhs, rhs)
            },
            Instruction::Mod(ref dest, ref lhs, ref rhs) => {
                self.vars.insert(lhs.clone());
                self.vars.insert(rhs.clone());
                format!("\t{} = {} % {};\n", dest, lhs, rhs)
            },
            Instruction::Negate(ref dest, ref value) => {
                self.vars.insert(dest.clone());
                self.vars.insert(value.clone());
                format!("\t{} = -{};\n", dest, value)
            },
            Instruction::Print(ref value) => {
                self.vars.insert(value.clone());
                format!("\tprintf(\"%ld\\n\", {});\n", value)
            },
            Instruction::Read(ref dest) => {
                self.vars.insert(dest.clone());
                format!("\tscanf(\"%ld\", &{});\n", dest)
            }
        }
    }

    fn generate_branch(&mut self, branch: Branch) -> String {
        match branch {
            Branch::Jmp(ref dest) => {
                format!("\tgoto {};\n", dest)
            },
            Branch::JmpP(ref cond, ref true_label, ref false_label) => {
                self.vars.insert(cond.clone());
                format!("\tif ({} > 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    cond,
                    true_label,
                    false_label
                )
            },
            Branch::JmpZ(ref cond, ref true_label, ref false_label) => {
                self.vars.insert(cond.clone());
                format!("\tif ({} == 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    cond,
                    true_label,
                    false_label
                )
            },
            Branch::JmpN(ref cond, ref true_label, ref false_label) => {
                self.vars.insert(cond.clone());
                format!("\tif ({} < 0) {{ goto {}; }} else {{ goto {}; }}\n",
                    cond,
                    true_label,
                    false_label
                )
            },
            Branch::Ret => format!("\treturn 0;\n"),
        }
    }
}
