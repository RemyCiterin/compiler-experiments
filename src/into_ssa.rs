//! This module is used to transform a control flow graph into a Static Single Assignment form,
//! it does it by taking as argument a CFG and returning a new equivalent CSF in SSA form.

use crate::ssa::*;
use crate::dominance::*;
use slotmap::*;


pub struct IntoSsaTransform {
    phis: SecondaryMap<Label, SparseSecondaryMap<Var, Phi>>,
    env: SecondaryMap<Var, Var>,
    dom: Dominance,
}

impl IntoSsaTransform {
    pub fn new(cfg: &Cfg) -> Self {
        let mut phis = SecondaryMap::new();
        let mut env = SecondaryMap::new();
        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        for (var, _) in cfg.stack.iter() {
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
    pub fn insert_phis(&mut self, cfg: &Cfg) {
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
                            cfg.preds(succ)
                            .iter()
                            .map(|l| (Lit::Var(var), *l)).collect();
                        phis[succ].insert(var, Phi{dest: var, args: phi_args});
                        dirty.push(succ);
                    }
                }
            }
        }

        self.phis = phis;
    }

    pub fn renaming(&mut self, cfg: &mut Cfg, block: Label) {
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
                for (src, label) in phi.args.iter_mut() {
                    // If env doesn't contains src, then this phi instruction will be deleted
                    // later cause all it's variable is not defined at idom(succ) (otherwise it
                    // must be definde at all it's predecessors)
                    if *label == block && env.contains_key(src.as_var().unwrap()) {
                        *src = Lit::Var(env[src.as_var().unwrap()]);
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

    pub fn run(&mut self, cfg: &mut Cfg) {
        self.insert_phis(cfg);
        self.renaming(cfg, cfg.entry());

        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for block in blocks {
            let mut stmt: Vec<Instr> =
                self.phis[block].iter().map(|(_, phi)| mk_instr(phi.clone())).collect();

            stmt.extend(cfg[block].stmt.iter().cloned());
            cfg.set_block_stmt(block, stmt);
        }

        cfg.start_ssa();
    }
}
