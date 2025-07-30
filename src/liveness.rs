use crate::ssa::*;
use std::collections::BTreeSet;
use slotmap::*;

pub struct Lives {
    pub inputs: BTreeSet<Var>,
    pub outputs: BTreeSet<Var>,
}

pub struct Liveness {
    lives: SecondaryMap<Label, Lives>,
}

impl std::ops::Index<Label> for Liveness {
    type Output = Lives;

    fn index(&self, block: Label) -> &Lives {
        &self.lives[block]
    }
}

impl Liveness {
    pub fn new(cfg: &Cfg) -> Self {
        let mut this = Self {
            lives: SecondaryMap::new(),
        };

        for (block, _) in cfg.iter_blocks() {
            this.lives
                .insert(block, Lives{inputs: BTreeSet::new(), outputs: BTreeSet::new()});
        }

        this
    }

    pub fn compute_live_out(&self, cfg: &Cfg, block: Label) -> BTreeSet<Var> {
        let mut result = BTreeSet::new();

        for instr in cfg[block].stmt.iter() {
            for label in instr.labels() {
                let lives = self.lives[label].inputs.clone();
                result.extend(lives);
            }
        }

        return result;
    }

    pub fn compute_live_in(&self, cfg: &Cfg, block: Label) -> BTreeSet<Var> {
        let mut result = self.lives[block].outputs.clone();

        for instr in cfg[block].stmt.iter().rev() {
            if let Some(dest) = instr.destination() {
                result.remove(&dest);
            }

            result.extend(instr.operands());
        }

        return result;
    }

    pub fn step(&mut self, cfg: &Cfg, block: Label) -> bool {
        self.lives[block].outputs = self.compute_live_out(cfg, block);

        let new_lives_in = self.compute_live_in(cfg, block);
        let old_lives_in = &self.lives[block].inputs;

        let progress = old_lives_in != &new_lives_in;
        self.lives[block].inputs = new_lives_in;
        return progress;
    }

    pub fn run(&mut self, cfg: &Cfg) {
        let mut dirty = cfg.preorder();

        while let Some(block) = dirty.pop() {
            let progress = self.step(cfg, block);

            if progress {
                for &pred in cfg.callers(block).iter() {
                    dirty.push(pred);
                }
            }
        }
    }
}
