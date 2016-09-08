use std::collections::{HashMap, HashSet};
use ir::{BasicBlock, Branch, Instruction, Function, Value};

pub fn optimize(func: &mut Function) {
    propagate_jumps(&mut func.blocks);
    for _ in 0..3 {
        propagate_values(&mut func.blocks);
        fold_constants(&mut func.blocks);
    }
    remove_unused_vars(func);
    simplify_jumps(&mut func.blocks);
    remove_unreachable_blocks(&mut func.blocks);
}

pub fn propagate_jumps(blocks: &mut Vec<BasicBlock>) {
    let mut directions: HashMap<String, String> = HashMap::new();
    for block in blocks.iter() {
        if block.instructions.is_empty() {
            if let Branch::Jmp(ref new_dest) = block.branch {
                let real_dest = directions.get(new_dest).cloned().unwrap_or(new_dest.clone());
                directions.insert(block.name.clone(), real_dest.clone());
                for (_, dest) in &mut directions {
                    if *dest == block.name {
                        *dest = real_dest.clone();
                    }
                }
            }
        }
    }

    for block in blocks.iter_mut() {
        match block.branch {
            Branch::Jmp(ref mut dest) => {
                if let Some(new_dest) = directions.get(dest) {
                    *dest = new_dest.clone();
                }
            }
            Branch::JmpP(_, ref mut true_dest, ref mut false_label) |
            Branch::JmpN(_, ref mut true_dest, ref mut false_label) |
            Branch::JmpZ(_, ref mut true_dest, ref mut false_label) => {
                if let Some(new_dest) = directions.get(true_dest) {
                    *true_dest = new_dest.clone();
                }
                if let Some(new_dest) = directions.get(false_label) {
                    *false_label = new_dest.clone();
                }
            }
            _ => {}
        }
    }
}

pub fn simplify_jumps(blocks: &mut Vec<BasicBlock>) {
    for block in blocks {
        let new_branch = match block.branch {
            Branch::JmpP(_, ref dest1, ref dest2) |
            Branch::JmpN(_, ref dest1, ref dest2) |
            Branch::JmpZ(_, ref dest1, ref dest2) if *dest1 == *dest2 => Branch::Jmp(dest1.clone()),
            ref other_br => other_br.clone(),
        };
        block.branch = new_branch;
    }
}

pub fn remove_unreachable_blocks(blocks: &mut Vec<BasicBlock>) {
    let map_blocks: HashMap<_, _> =
        blocks.iter().cloned().map(|block| (block.name.clone(), block)).collect();

    let first_block_label = blocks.first().unwrap().name.clone();

    let reachable_blocks = reachable_blocks(first_block_label, &map_blocks);
    blocks.retain(|block| reachable_blocks.contains(&block.name));
}

fn reachable_blocks(first_block: String, blocks: &HashMap<String, BasicBlock>) -> HashSet<String> {
    let mut visited: HashSet<String> = HashSet::new();
    let mut queue = vec![first_block];

    while let Some(block_name) = queue.pop() {
        if !visited.contains(&block_name) {
            visited.insert(block_name.clone());
            let block = blocks.get(&block_name).unwrap();
            queue.extend(can_reach(&block.branch));
        }
    }
    visited
}

fn can_reach(branch: &Branch) -> Vec<String> {
    match branch.clone() {
        Branch::Jmp(dest) => vec![dest],
        Branch::JmpP(_, dest1, dest2) => vec![dest1, dest2],
        Branch::JmpZ(_, dest1, dest2) => vec![dest1, dest2],
        Branch::JmpN(_, dest1, dest2) => vec![dest1, dest2],
        Branch::Ret => Vec::new(),
    }
}

pub fn propagate_values(blocks: &mut Vec<BasicBlock>) {
    for block in blocks.iter_mut() {
        let mut mapping: HashMap<String, Value> = HashMap::new();
        for instruction in &mut block.instructions {
            match *instruction {

                Instruction::Add(_, ref mut lhs, ref mut rhs) |
                Instruction::Sub(_, ref mut lhs, ref mut rhs) |
                Instruction::Mul(_, ref mut lhs, ref mut rhs) |
                Instruction::Div(_, ref mut lhs, ref mut rhs) |
                Instruction::Mod(_, ref mut lhs, ref mut rhs) => {
                    change_value(lhs, &mapping);
                    change_value(rhs, &mapping);
                }
                Instruction::Assign(_, ref mut value) |
                Instruction::Negate(_, ref mut value) |
                Instruction::Print(ref mut value) => change_value(value, &mapping),
                Instruction::Read(_) => {}
            }

            if let Instruction::Assign(ref dest, ref value) = *instruction {
                mapping.insert(dest.clone(), value.clone());
            }
        }

        match block.branch {
            Branch::JmpP(ref mut cond, _, _) |
            Branch::JmpN(ref mut cond, _, _) |
            Branch::JmpZ(ref mut cond, _, _) => change_value(cond, &mapping),
            _ => {}
        }
    }
}

fn change_value(value: &mut Value, mapping: &HashMap<String, Value>) {
    let old_value = value.clone();
    if let Value::Var(name) = old_value {
        if let Some(real_value) = mapping.get(&name).cloned() {
            *value = real_value;
        }
    }
}

pub fn remove_unused_vars(func: &mut Function) {
    let mut read_vars: HashSet<String> = HashSet::new();

    for block in &mut func.blocks {
        for instruction in &mut block.instructions {
            match *instruction {
                Instruction::Add(_, ref mut lhs, ref mut rhs) |
                Instruction::Sub(_, ref mut lhs, ref mut rhs) |
                Instruction::Mul(_, ref mut lhs, ref mut rhs) |
                Instruction::Div(_, ref mut lhs, ref mut rhs) |
                Instruction::Mod(_, ref mut lhs, ref mut rhs) => {
                    add_read_name(lhs, &mut read_vars);
                    add_read_name(rhs, &mut read_vars);
                }
                Instruction::Assign(_, ref mut value) |
                Instruction::Negate(_, ref mut value) |
                Instruction::Print(ref mut value) => add_read_name(value, &mut read_vars),
                Instruction::Read(_) => {}
            }
        }
        match block.branch {
            Branch::JmpP(ref mut cond, _, _) |
            Branch::JmpN(ref mut cond, _, _) |
            Branch::JmpZ(ref mut cond, _, _) => add_read_name(cond, &mut read_vars),
            _ => {}
        }
    }

    for block in &mut func.blocks {
        block.instructions.retain(|&ref instruction| {
            match *instruction {
                Instruction::Assign(ref dest, _) |
                Instruction::Add(ref dest, _, _) |
                Instruction::Sub(ref dest, _, _) |
                Instruction::Mul(ref dest, _, _) |
                Instruction::Div(ref dest, _, _) |
                Instruction::Mod(ref dest, _, _) |
                Instruction::Negate(ref dest, _) => read_vars.contains(dest),
                _ => true,
            }
        });
    }

    func.vars = read_vars;
}

fn add_read_name(value: &Value, read_vars: &mut HashSet<String>) {
    if let Value::Var(ref var) = *value {
        read_vars.insert(var.clone());
    }
}

pub fn fold_constants(blocks: &mut Vec<BasicBlock>) {
    for block in blocks.iter_mut() {
        for instruction in &mut block.instructions {
            *instruction = match instruction.clone() {
                Instruction::Add(dest, Value::Const(a), Value::Const(b)) => {
                    Instruction::Assign(dest, Value::Const(a + b))
                }
                Instruction::Sub(dest, Value::Const(a), Value::Const(b)) => {
                    Instruction::Assign(dest, Value::Const(a - b))
                }
                Instruction::Mul(dest, Value::Const(a), Value::Const(b)) => {
                    Instruction::Assign(dest, Value::Const(a * b))
                }
                Instruction::Div(dest, Value::Const(a), Value::Const(b)) => {
                    Instruction::Assign(dest, Value::Const(a / b))
                }
                Instruction::Mod(dest, Value::Const(a), Value::Const(b)) => {
                    Instruction::Assign(dest, Value::Const(a % b))
                }
                Instruction::Negate(dest, Value::Const(a)) => {
                    Instruction::Assign(dest, Value::Const(-a))
                }
                _ => instruction.clone(),
            }
        }
    }
}
