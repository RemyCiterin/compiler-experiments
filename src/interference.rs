use crate::liveness::Liveness;
use std::collections::HashSet;
use crate::ssa::*;
use slotmap::*;

pub struct InterferenceGraph{
    matrix: SparseSecondaryMap<Var, HashSet<Var>>,
}

impl std::ops::Index<Var> for InterferenceGraph {
    type Output = HashSet<Var>;

    fn index(&self, var: Var) -> &Self::Output {
        &self.matrix[var]
    }
}

impl InterferenceGraph {
    pub fn new(cfg: &Cfg<Instr>) -> Self {
        let mut matrix = SparseSecondaryMap::new();

        for (var, _) in cfg.iter_vars() {
            matrix.insert(var, HashSet::new());
        }

        Self { matrix }
    }

    pub fn add_conflict(&mut self, x: Var, y: Var) {
        self.matrix[x].insert(y);
        self.matrix[y].insert(x);
    }

    pub fn run(&mut self, cfg: &Cfg<Instr>, liveness: &Liveness) {
        for (block, _) in cfg.iter_blocks() {
            let mut lives = liveness[block].outputs.clone();

            for instr in cfg[block].stmt.iter().rev() {
                // Loads/stores from/to a stack known offset doesn't need to be added to the
                // interference graph
                if let Instr::Load{addr: Lit::Var(addr), dest, ..} = instr
                    && cfg[*addr] == VarKind::Stack {
                    lives.remove(&dest);
                    lives.iter().for_each(|x| self.add_conflict(*x, *dest));
                    continue;
                }

                if let Instr::Store{addr: Lit::Var(addr), val, ..} = instr
                    && cfg[*addr] == VarKind::Stack {
                    if let Some(x) = val.as_var() {
                        lives.insert(x);
                    }
                    continue;
                }

                if let Some(dest) = instr.destination() {
                    lives.remove(&dest);
                    lives.iter().for_each(|x| self.add_conflict(*x, dest));
                }

                lives.extend(instr.operands());
            }
        }
    }
}
