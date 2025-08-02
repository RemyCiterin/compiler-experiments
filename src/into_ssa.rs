//! This module is used to transform a control flow graph into a Static Single Assignment form,
//! it does it by taking as argument a CFG and returning a new equivalent CSF in SSA form.

use crate::ssa::*;
use crate::dominance::*;
use std::collections::HashMap;
use crate::liveness::*;
use slotmap::*;

/// Naive SSA form generation using liveness relation, it rename each variable at the entry of each
/// block using a phi expression
pub fn into_ssa(cfg: &mut Cfg<Instr>) {
    // Used to compute the translation as ssa of the output variables of each block
    let mut globals: HashMap<Label, HashMap<Var, Var>> = HashMap::new();

    // Phi expressions to insert in each block
    let mut phis: HashMap<Label, Vec<Instr>> = HashMap::new();

    // Liveness analysis
    let mut liveness = Liveness::new(cfg);
    liveness.run(cfg);

    let order = cfg.labels().clone();

    // Compute phis and rename variables (except in phi expressions)
    for &block in order.iter() {
        // Start by creating a fresh variable for each live variable at input of the block
        let mut map: HashMap<Var, Var> = HashMap::new();
        let mut stmt = vec![];

        for &var in liveness[block].inputs.iter() {
            let v = cfg.fresh_var();
            map.insert(var, v);
            stmt.push(
                Instr::Phi(v, cfg.preds(block).iter().map(|l| (var, *l)).collect()));
        }

        phis.insert(block, stmt);

        // Now rename each variable of the block
        let mut stmt = cfg[block].stmt.clone();

        for instr in stmt.iter_mut() {
            for x in instr.operands_mut() { *x = map[x]; }
            if let Some(x) = instr.destination_mut() {
                let y = cfg.fresh_var();
                map.insert(*x, y);
                *x = y;
            }
        }

        globals.insert(block, map);
        cfg.set_block_stmt(block, stmt);
    }

    // Rename phi variables and add phis to blocks
    for &block in order.iter() {
        let mut stmt = phis.remove(&block).unwrap();

        for phi in stmt.iter_mut() {
            match phi {
                Instr::Phi(_, vars) => {
                    for (v, l) in vars.iter_mut() {
                        *v = globals[l][v];
                    }
                }
                _ => {}
            }
        }

        // Add phis expressions to each blocks
        stmt.extend(cfg[block].stmt.iter().cloned());
        cfg.set_block_stmt(block, stmt);
    }

    // Start to compute the block of definition of each variable
    cfg.start_ssa();
}

pub struct IntoSsaTransform {
    phis: SecondaryMap<Label, SparseSecondaryMap<Var, Instr>>,
    env: SecondaryMap<Var, Var>,
    dom: Dominance,
}

impl IntoSsaTransform {
    pub fn new(cfg: &Cfg<Instr>) -> Self {
        let mut phis = SecondaryMap::new();
        let mut env = SecondaryMap::new();
        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        for var in cfg.stack.iter() {
            env.insert(*var, *var);
        }

        for var in cfg.args.iter() {
            env.insert(*var, *var);
        }

        for (block, _) in cfg.iter_blocks() {
            phis.insert(block, SparseSecondaryMap::new());
        }

        Self { env, dom, phis }
    }

    /// For each update of a variable `v` in a block `b`, insert the instruction
    /// `v := phi (v, ..., v)` at the dominance frontier of `b`
    pub fn insert_phis(&mut self, cfg: &Cfg<Instr>) {
        let mut phis =
            std::mem::take(&mut self.phis);

        let mut queue: SecondaryMap<Var, Vec<Label>> = SecondaryMap::new();
        for (var, _) in cfg.iter_vars() {
            queue.insert(var, vec![]);
        }

        for (label, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                if let Some(dest) = instr.destination() {
                    queue[dest].push(label);
                }
            }
        }

        for (var, dirty) in queue.iter_mut() {
            while let Some(block) = dirty.pop() {
                for &succ in self.dom.frontier(block) {
                    if !phis[succ].contains_key(var) {
                        let phi_args =
                            cfg.preds(succ).iter().map(|l| (var, *l)).collect();
                        phis[succ].insert(var, Instr::Phi(var, phi_args));
                        dirty.push(succ);
                    }
                }
            }
        }

        self.phis = phis;
    }

    pub fn renaming(&mut self, cfg: &mut Cfg<Instr>, block: Label) {
        let mut env = self.env.clone();

        let mut useless_phi: Vec<Var> = vec![];
        for (var, phi) in self.phis[block].iter_mut() {
            let dest = phi.destination_mut().unwrap();

            if env.contains_key(*dest) {
                let new_dest = cfg.fresh_var();
                env.insert(*dest, new_dest);
                *dest = new_dest;
            } else {
                useless_phi.push(var);
            }
        }

        for var in useless_phi {
            self.phis[block].remove(var);
        }

        let mut stmt = cfg[block].stmt.clone();

        for instr in stmt.iter_mut() {
            for src in instr.operands_mut() {
                // `src` must be a valid entry in `env`, otherwise the variable `src` doesn't
                // respect it's `use-after-def` relation
                *src = env[*src];
            }

            if let Some(dest) = instr.destination_mut() {
                let new_dest = cfg.fresh_var();
                env.insert(*dest, new_dest);
                *dest = new_dest;
            }
        }

        cfg.set_block_stmt(block, stmt);

        for succ in cfg[block].succs() {
            for (_, phi) in self.phis[succ].iter_mut() {
                if let Instr::Phi(_, vars) = phi {
                    for (src, label) in vars.iter_mut() {
                        // If env doesn't contains src, then this phi instruction will be deleted
                        // later cause all it's variable is not defined at idom(succ) (otherwise it
                        // must be definde at all it's predecessors)
                        if *label == block && env.contains_key(*src) {
                            *src = env[*src];
                        }
                    }
                }
            }
        }

        let env_save = std::mem::replace(&mut self.env, env);

        let blocks: Vec<Label> = self.dom.childrens(block).iter().cloned().collect();

        for b in blocks {
            self.renaming(cfg, b);
        }

        self.env = env_save;
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        self.insert_phis(cfg);
        self.renaming(cfg, cfg.entry());

        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for block in blocks {
            let mut stmt: Vec<Instr> =
                self.phis[block].iter().map(|(_, phi)| phi.clone()).collect();

            stmt.extend(cfg[block].stmt.iter().cloned());
            cfg.set_block_stmt(block, stmt);
        }

        cfg.start_ssa();
    }
}
