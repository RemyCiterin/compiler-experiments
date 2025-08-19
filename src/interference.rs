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
    pub fn new<Op: Operation, Cond: Condition>(cfg: &Cfg<Op, Cond>) -> Self {
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

    pub fn contains(&self, x: Var) -> bool {
        self.matrix.contains_key(x)
    }

    pub fn merge(&mut self, new: Var, old: Var) {
        assert!(!self.matrix[new].contains(&old));
        assert!(!self.matrix[old].contains(&new));

        let added = std::mem::take(&mut self.matrix[old]);

        for &x in added.iter() {
            self.matrix[x].remove(&old);
            self.add_conflict(new, x);
        }

        self.matrix.remove(old);
    }

    pub fn run<Op: Operation, Cond: Condition>
        (&mut self, cfg: &Cfg<Op, Cond>, liveness: &Liveness) {
        for (block, _) in cfg.iter_blocks() {
            let mut lives = liveness[block].outputs.clone();

            for instr in cfg[block].stmt.iter().rev() {
                if let Some(dest) = instr.destination() {
                    lives.remove(&dest);
                    lives.iter().for_each(|x| self.add_conflict(*x, dest));
                }

                lives.extend(instr.operands());
            }
        }
    }
}
