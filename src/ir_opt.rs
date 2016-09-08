use std::collections::{HashMap, HashSet};
use itertools::Itertools;
use ir::{BasicBlock, Branch, Instruction};

pub fn optimize(blocks: Vec<BasicBlock>) -> Vec<BasicBlock> {
    let blocks = propagate_jumps(blocks);
    let blocks = remove_unreachable_blocks(blocks);
    blocks
}

pub fn propagate_jumps(mut blocks: Vec<BasicBlock>) -> Vec<BasicBlock> {
    let mut directions: HashMap<String, String> = HashMap::new();
    for block in blocks.iter() {
        if block.instructions.len() == 0 {
            if let Branch::Jmp(ref new_dest) = block.branch {
                let real_dest = directions.get(new_dest).cloned().unwrap_or(new_dest.clone());
                directions.insert(block.name.clone(), real_dest.clone());
                for (_, dest) in directions.iter_mut() {
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
            },
            Branch::JmpP(_, ref mut true_dest, ref mut false_label) |
            Branch::JmpN(_, ref mut true_dest, ref mut false_label) |
            Branch::JmpZ(_, ref mut true_dest, ref mut false_label) => {
                if let Some(new_dest) = directions.get(true_dest) {
                    *true_dest = new_dest.clone();
                }
                if let Some(new_dest) = directions.get(false_label) {
                    *false_label = new_dest.clone();
                }
            },
            _ => {}
        }
    }
    blocks
}

pub fn remove_unreachable_blocks(mut blocks: Vec<BasicBlock>) -> Vec<BasicBlock> {
    let mut map_blocks: HashMap<_, _> =
        blocks.iter().cloned().map(|block| (block.name.clone(), block)).collect();

    let reachable_blocks = reachable_blocks(&mut map_blocks);
    blocks.into_iter().filter(|ref block| reachable_blocks.contains(&block.name)).collect()
}

fn can_reach(branch: &Branch) -> Vec<String> {
    match branch.clone() {
        Branch::Jmp(dest) => vec![dest],
        Branch::JmpP(_, dest1, dest2) => vec![dest1, dest2],
        Branch::JmpZ(_, dest1, dest2) => vec![dest1, dest2],
        Branch::JmpN(_, dest1, dest2) => vec![dest1, dest2],
        Branch::Ret => Vec::new()
    }
}

fn reachable_blocks(blocks: &HashMap<String, BasicBlock>) -> HashSet<String> {
    let mut visited: HashSet<String> = HashSet::new();
    let mut queue = vec![String::from("start")];

    while let Some(block_name) = queue.pop() {
        if !visited.contains(&block_name) {
            visited.insert(block_name.clone());
            let block = blocks.get(&block_name).unwrap();
            queue.extend(can_reach(&block.branch));
        }
    }
    visited
}
