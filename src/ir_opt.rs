use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use ir::{Module, Function, BasicBlockId, BasicBlock, Branch, Instruction, Computation, Value};

pub fn optimize(module: &mut Module) {
    for func in &mut module.functions {
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
}

pub fn propagate_jumps(blocks: &mut Vec<BasicBlock>) {
    let mut directions: HashMap<BasicBlockId, BasicBlockId> = HashMap::new();
    for block in blocks.iter() {
        if block.instructions.is_empty() {
            if let Branch::Jmp(new_dest) = block.branch {
                let real_dest = directions.get(&new_dest).cloned().unwrap_or(new_dest);
                directions.insert(block.id, real_dest);
                for (_, dest) in &mut directions {
                    if *dest == block.id {
                        *dest = real_dest;
                    }
                }
            }
        }
    }

    for block in blocks.iter_mut() {
        match block.branch {
            Branch::Jmp(ref mut dest) => {
                if let Some(new_dest) = directions.get(dest) {
                    *dest = *new_dest;
                }
            }
            Branch::JmpT(_, ref mut true_dest, ref mut false_label) => {
                if let Some(new_dest) = directions.get(true_dest) {
                    *true_dest = *new_dest;
                }
                if let Some(new_dest) = directions.get(false_label) {
                    *false_label = *new_dest;
                }
            }
            _ => {}
        }
    }
}

pub fn simplify_jumps(blocks: &mut Vec<BasicBlock>) {
    for block in blocks {
        let new_branch = match block.branch {
            Branch::JmpT(_, d1, d2) if d1 == d2 => Branch::Jmp(d1),
            Branch::JmpT(Value::Const(a), dtrue, dfalse) => {
                if a != 0 {
                    Branch::Jmp(dtrue)
                } else {
                    Branch::Jmp(dfalse)
                }
            }
            ref other_br => other_br.clone(),
        };
        block.branch = new_branch;
    }
}

pub fn remove_unreachable_blocks(blocks: &mut Vec<BasicBlock>) {
    let reachable_blocks = {
        let map_blocks: HashMap<_, _> = blocks.iter().map(|block| (block.id, block)).collect();

        let first_block_id = blocks.first().unwrap().id;

        reachable_blocks(first_block_id, &map_blocks)
    };

    blocks.retain(|block| reachable_blocks.contains(&block.id));
}

fn reachable_blocks(first_block_id: BasicBlockId,
                    blocks: &HashMap<BasicBlockId, &BasicBlock>)
                    -> HashSet<BasicBlockId> {
    let mut visited: HashSet<BasicBlockId> = HashSet::new();
    let mut queue = vec![first_block_id];

    while let Some(block_name) = queue.pop() {
        if !visited.contains(&block_name) {
            visited.insert(block_name.clone());
            let block = blocks.get(&block_name).unwrap();
            queue.extend(can_reach(&block.branch));
        }
    }
    visited
}

fn can_reach(branch: &Branch) -> Vec<BasicBlockId> {
    match *branch {
        Branch::Jmp(dest) => vec![dest],
        Branch::JmpT(_, dest1, dest2) => vec![dest1, dest2],
        Branch::Ret(_) => Vec::new(),
    }
}

pub fn propagate_values(blocks: &mut Vec<BasicBlock>) {
    for block in blocks.iter_mut() {
        let mut mapping: HashMap<String, Value> = HashMap::new();
        for instruction in &mut block.instructions {
            match *instruction {
                Instruction::Assign(_, ref mut comp) |
                Instruction::Compute(ref mut comp) => {
                    match *comp {
                        Computation::FuncCall(_, ref mut param_values) => {
                            for value in param_values {
                                change_value(value, &mapping);
                            }
                        }
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
            }

            if let Instruction::Assign(ref dest, Computation::Value(ref value)) = *instruction {
                mapping.insert(dest.clone(), value.clone());
            }
        }

        match block.branch {
            Branch::JmpT(ref mut cond, _, _) => change_value(cond, &mapping),
            Branch::Ret(ref mut val) => change_value(val, &mapping),
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

    for block in &func.blocks {
        for instruction in &block.instructions {
            match *instruction {
                Instruction::Assign(_, ref comp) |
                Instruction::Compute(ref comp) => {
                    match *comp {
                        Computation::FuncCall(_, ref param_values) => {
                            for value in param_values {
                                add_read_name(value, &mut read_vars);
                            }
                        }
                        Computation::Add(ref lhs, ref rhs) |
                        Computation::Sub(ref lhs, ref rhs) |
                        Computation::Mul(ref lhs, ref rhs) |
                        Computation::Div(ref lhs, ref rhs) |
                        Computation::Mod(ref lhs, ref rhs) |
                        Computation::CmpLess(ref lhs, ref rhs) |
                        Computation::CmpLessEq(ref lhs, ref rhs) |
                        Computation::CmpGreater(ref lhs, ref rhs) |
                        Computation::CmpGreaterEq(ref lhs, ref rhs) |
                        Computation::CmpEq(ref lhs, ref rhs) |
                        Computation::CmpNotEq(ref lhs, ref rhs) => {
                            add_read_name(lhs, &mut read_vars);
                            add_read_name(rhs, &mut read_vars);
                        }
                        Computation::Value(ref value) |
                        Computation::LogNot(ref value) |
                        Computation::Negate(ref value) => add_read_name(value, &mut read_vars),
                    }
                }
            }
        }
        match block.branch {
            Branch::JmpT(ref cond, _, _) => add_read_name(cond, &mut read_vars),
            Branch::Ret(ref val) => add_read_name(val, &mut read_vars),
            _ => {}
        }
    }

    for block in &mut func.blocks {
        block.instructions.retain(|&ref instruction| {
            match *instruction {
                Instruction::Assign(ref dest, _) => read_vars.contains(dest),
                Instruction::Compute(_) => true,
                // TODO if no side effect,
                // we can delete(func call is the only side effect for now)
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
                Instruction::Compute(ref mut comp) => {
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
            }
        }
    }
    has_changed
}

pub fn merge_adjacent_blocks(blocks: &mut Vec<BasicBlock>) {
    let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    for block in blocks.iter() {
        for target in can_reach(&block.branch) {
            preds.entry(target).or_insert(Vec::new()).push(block.id);
        }
    }

    *blocks = blocks.iter()
        .cloned()
        .coalesce(|block1, block2| {
            if let Branch::Jmp(b1_target) = block1.branch {
                if b1_target == block2.id {
                    if preds.get(&block2.id)
                        .map(|v| v.len() == 1 && v.contains(&block1.id))
                        .unwrap_or(false) {

                        for (_, block_preds) in preds.iter_mut() {
                            for pred in block_preds.iter_mut() {
                                if *pred == block2.id {
                                    *pred = block1.id;
                                }
                            }
                        }

                        let mut merged_instructions = block1.instructions;
                        merged_instructions.extend(block2.instructions);
                        return Ok(BasicBlock {
                            id: block1.id,
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
