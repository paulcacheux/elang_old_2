use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use ir::{BasicBlock, Branch, Instruction, Computation, Function, Value};

pub fn optimize(func: &mut Function) {
    merge_adjacent_blocks(&mut func.blocks);

    let mut still_work = true;
    while still_work {
        for _ in 0..2 {
            propagate_values(&mut func.blocks);
            still_work = fold_constants(&mut func.blocks);
            remove_unused_vars(func);
        }
    }

    for _ in 0..2 {
        propagate_jumps(&mut func.blocks);
        simplify_jumps(&mut func.blocks);
    }
    remove_unreachable_blocks(&mut func.blocks);
    merge_adjacent_blocks(&mut func.blocks);
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
            Branch::JmpT(_, ref mut true_dest, ref mut false_label) => {
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
            Branch::JmpT(_, ref d1, ref d2) if *d1 == *d2 => Branch::Jmp(d1.clone()),
            Branch::JmpT(Value::Const(a), ref dtrue, ref dfalse) => {
                if a != 0 {
                    Branch::Jmp(dtrue.clone())
                } else {
                    Branch::Jmp(dfalse.clone())
                }
            }
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
        Branch::JmpT(_, dest1, dest2) => vec![dest1, dest2],
        Branch::Ret => Vec::new(),
    }
}

pub fn propagate_values(blocks: &mut Vec<BasicBlock>) {
    for block in blocks.iter_mut() {
        let mut mapping: HashMap<String, Value> = HashMap::new();
        for instruction in &mut block.instructions {
            match *instruction {
                Instruction::Assign(_, ref mut comp) |
                Instruction::Print(ref mut comp) => {
                    match *comp {
                        Computation::Add(ref mut lhs, ref mut rhs) |
                        Computation::Sub(ref mut lhs, ref mut rhs) |
                        Computation::Mul(ref mut lhs, ref mut rhs) |
                        Computation::Div(ref mut lhs, ref mut rhs) |
                        Computation::Mod(ref mut lhs, ref mut rhs) |
                        Computation::CmpLess(ref mut lhs, ref mut rhs) |
                        Computation::CmpLessEq(ref mut lhs, ref mut rhs) |
                        Computation::CmpGreater(ref mut lhs, ref mut rhs) |
                        Computation::CmpGreaterEq(ref mut lhs, ref mut rhs) |
                        Computation::CmpEq(ref mut lhs, ref mut rhs) |
                        Computation::CmpNotEq(ref mut lhs, ref mut rhs) => {
                            change_value(lhs, &mapping);
                            change_value(rhs, &mapping);
                        }
                        Computation::Value(ref mut value) |
                        Computation::LogNot(ref mut value) |
                        Computation::Negate(ref mut value) => change_value(value, &mapping),
                    }
                }
                _ => {}
            }

            if let Instruction::Assign(ref dest, Computation::Value(ref value)) = *instruction {
                mapping.insert(dest.clone(), value.clone());
            }
        }

        match block.branch {
            Branch::JmpT(ref mut cond, _, _) => change_value(cond, &mapping),
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
                Instruction::Assign(_, ref mut comp) |
                Instruction::Print(ref mut comp) => {
                    match *comp {
                        Computation::Add(ref mut lhs, ref mut rhs) |
                        Computation::Sub(ref mut lhs, ref mut rhs) |
                        Computation::Mul(ref mut lhs, ref mut rhs) |
                        Computation::Div(ref mut lhs, ref mut rhs) |
                        Computation::Mod(ref mut lhs, ref mut rhs) |
                        Computation::CmpLess(ref mut lhs, ref mut rhs) |
                        Computation::CmpLessEq(ref mut lhs, ref mut rhs) |
                        Computation::CmpGreater(ref mut lhs, ref mut rhs) |
                        Computation::CmpGreaterEq(ref mut lhs, ref mut rhs) |
                        Computation::CmpEq(ref mut lhs, ref mut rhs) |
                        Computation::CmpNotEq(ref mut lhs, ref mut rhs) => {
                            add_read_name(lhs, &mut read_vars);
                            add_read_name(rhs, &mut read_vars);
                        }
                        Computation::Value(ref mut value) |
                        Computation::LogNot(ref mut value) |
                        Computation::Negate(ref mut value) => add_read_name(value, &mut read_vars),
                    }
                }
                Instruction::Read(ref dest) => {
                    read_vars.insert(dest.clone());
                }
            }
        }
        match block.branch {
            Branch::JmpT(ref mut cond, _, _) => add_read_name(cond, &mut read_vars),
            _ => {}
        }
    }

    for block in &mut func.blocks {
        block.instructions.retain(|&ref instruction| {
            match *instruction {
                Instruction::Assign(ref dest, _) => read_vars.contains(dest),
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

pub fn fold_constants(blocks: &mut Vec<BasicBlock>) -> bool {
    let mut has_changed = false;
    for block in blocks.iter_mut() {
        for instruction in &mut block.instructions {
            match *instruction {
                Instruction::Assign(_, ref mut comp) |
                Instruction::Print(ref mut comp) => {
                    *comp = match comp.clone() {
                        Computation::Add(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const(a + b))
                        }
                        Computation::Sub(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const(a - b))
                        }
                        Computation::Mul(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const(a * b))
                        }
                        Computation::Div(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const(a / b))
                        }
                        Computation::Mod(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const(a % b))
                        }
                        Computation::CmpLess(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a < b) as i64))
                        }
                        Computation::CmpLessEq(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a <= b) as i64))
                        }
                        Computation::CmpGreater(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a > b) as i64))
                        }
                        Computation::CmpGreaterEq(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a >= b) as i64))
                        }
                        Computation::CmpEq(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a == b) as i64))
                        }
                        Computation::CmpNotEq(Value::Const(a), Value::Const(b)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a != b) as i64))
                        }
                        Computation::LogNot(Value::Const(a)) => {
                            has_changed = true;
                            Computation::Value(Value::Const((a == 0) as i64))
                        }
                        Computation::Negate(Value::Const(a)) => {
                            has_changed = true;
                            Computation::Value(Value::Const(-a))
                        }
                        _ => comp.clone(),
                    };
                }
                _ => {}
            }
        }
    }
    has_changed
}

pub fn merge_adjacent_blocks(blocks: &mut Vec<BasicBlock>) {
    let mut preds: HashMap<String, Vec<String>> = HashMap::new();
    for block in blocks.iter() {
        for target in can_reach(&block.branch) {
            preds.entry(target).or_insert(Vec::new()).push(block.name.clone());
        }
    }

    *blocks = blocks.iter()
        .cloned()
        .coalesce(|block1, block2| {
            if let Branch::Jmp(ref b1_target) = block1.branch {
                if *b1_target == block2.name {
                    if preds.get(&block2.name)
                        .map(|v| v.len() == 1 && v.contains(&block1.name))
                        .unwrap_or(false) {

                        for (_, block_preds) in preds.iter_mut() {
                            for pred in block_preds.iter_mut() {
                                if *pred == block2.name {
                                    *pred = block1.name.clone();
                                }
                            }
                        }

                        let mut merged_instructions = block1.instructions;
                        merged_instructions.extend(block2.instructions);
                        return Ok(BasicBlock {
                            name: block1.name,
                            instructions: merged_instructions,
                            branch: block2.branch,
                        });
                    }
                }
            }
            Err((block1, block2))
        })
        .collect();
}
