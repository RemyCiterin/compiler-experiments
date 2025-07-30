use crate::ir::*;
use std::collections::{HashMap, HashSet};
use crate::identifiers::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Color(pub usize);

pub fn greedy_coloring(
    order: &Vec<Id>,
    graph: InterferenceMatrix,
    map: &mut HashMap<Id, Color>,
) {
    for &id in order.iter() {
        if map.contains_key(&id) { continue; }
        if !graph.matrix.contains_key(&id) {
            map.insert(id, Color(0));
            continue;
        }

        let edges = &graph.matrix[&id];

        // Assign the smallest possible color to `id`
        for color in 0.. {
            let no_conflict = |x| {
                !map.contains_key(x) || map[x] != Color(color)
            };

            if edges.iter().all(no_conflict) {
                map.insert(id, Color(color));
                break;
            }
        }
    }
}

pub fn get_coloring_order(cfg: &CFG) -> Vec<Id> {
    let mut vars: HashSet<Id> = HashSet::new();
    let mut order: Vec<Id> = Vec::new();

    for &block in cfg.topo_sort().iter().rev() {
        for instr in cfg[block].stmt.iter() {
            if let Some(id) = instr.destination() && !vars.contains(&id) {
                vars.insert(id);
                order.push(id);
            }

            for id in instr.operands() {
                if !vars.contains(&id) {
                    vars.insert(id);
                    order.push(id);
                }
            }
        }
    }

    order
}

pub fn spill(cfg: &mut CFG, map: &HashMap<Id, Color>, num_regs: usize, stack_size: &mut usize) {
    if map.is_empty() { return; }


    for block in 0..cfg.len() {
        let mut stmt: Vec<Instr> = vec![];

        for mut instr in cfg[block].clone().stmt.iter().cloned() {
            for r in instr.operands_mut() {
                if map[r].0 >= num_regs {
                    let new = cfg.fresh_register();
                    stmt.push(Instr::LoadStack(new, map[r].0 - num_regs + *stack_size));
                    *r = new;
                }
            }

            let mut to_push: Option<Instr> = None;
            if let Some(r) = instr.destination_mut() && map[r].0 >= num_regs {
                let new: Id = cfg.fresh_register();
                to_push = Some(Instr::StoreStack(new, map[r].0 - num_regs + *stack_size));
                *r = new;
            }

            stmt.push(instr);

            if let Some(instr) = to_push { stmt.push(instr); }
        }

        cfg.set_block_stmt(block, stmt);
    }

    let used_regs = map.iter().map(|(_, &c)| c.0).max().unwrap() + 1;
    if used_regs >= num_regs { *stack_size += used_regs - num_regs + 1; }
}

pub fn apply_coloring(cfg: &mut CFG, map: &HashMap<Id, Color>) {
    if map.is_empty() { return; }

    let max_color = map.iter().map(|(_, &c)| c).max().unwrap().0;
    let mut color2id: HashMap<Color, Id> = HashMap::new();

    for color in 0..max_color {
        color2id.insert(Color(color), cfg.fresh_register());
    }

    for block in 0..cfg.len() {
        let block = &mut cfg[block];

        for instr in block.stmt.iter_mut() {
            if let Some(r) = instr.destination_mut() {
                *r = color2id[&map[r]];
            }

            for r in instr.operands_mut() {
                *r = color2id[&map[r]];
            }
        }
    }
}
