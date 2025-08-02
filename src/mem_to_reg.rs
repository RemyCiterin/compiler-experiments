//! This module introduce a pass that search for stack slots that are safe to replace by a register
//! and introduce the necessary phi expressions and renaming to replace those stack slots by
//! registers.

use std::collections::HashSet;

use slotmap::*;
use crate::dominance::*;
use crate::ssa::*;

pub struct MemToReg {
    /// The set of stack slots to remove
    removed: HashSet<Var>,

    /// Necessary phi instruction per label/variable
    phis: SecondaryMap<Label, SparseSecondaryMap<Var, Instr>>,

    /// Current environment of variables in removed, as a mapping from stack slots to fresh
    /// variables
    env: SecondaryMap<Var, Var>,

    /// Dominance tree/dominance frontier
    dom: Dominance,
}

impl MemToReg {
    pub fn new(cfg: &Cfg<Instr>) -> Self {
        let mut removed = HashSet::new();

        for (v, kind) in cfg.iter_vars() {
            if *kind == VarKind::Stack {
                removed.insert(v);
            }
        }

        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        let env = SecondaryMap::new();
        let mut phis = SecondaryMap::new();

        for (b, _) in cfg.iter_blocks() {
            phis.insert(b, SparseSecondaryMap::new());
        }

        Self { removed, dom, env, phis }
    }

    /// Search for stack variables that are safe to remove: a stack variable is safe to remove iff
    /// we only use it to load/store from/to it in a non-volatile mode
    pub fn search(&mut self, cfg: &Cfg<Instr>) {
        for (_, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                match instr {
                    Instr::Load{volatile: false, ..} => {}
                    Instr::Store{val, volatile: false, ..} => {
                        self.removed.remove(val);
                    }
                    _ => {
                        for x in instr.operands() {
                            self.removed.remove(&x);
                        }
                    }
                }
            }
        }

        for x in self.removed.iter() {
            print!("{} ", x);
        } println!();
    }

    /// Introduce the necessary phi expressions, note that some of those expressions are not
    /// necessary because the introduce a phi in a block for a variable that is introduced in this
    /// same block, so it's necessary to detect them later
    pub fn insert_phis(&mut self, cfg: &Cfg<Instr>) {
        // Insert phis each times we store into a removed stack variable

        let mut phis =
            std::mem::take(&mut self.phis);

        let mut queue: SecondaryMap<Var, Vec<Label>> = SecondaryMap::new();
        for &var in self.removed.iter() {
            queue.insert(var, vec![]);
        }

        for (label, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                if let Instr::Store{addr, ..} = instr && self.removed.contains(addr) {
                    queue[*addr].push(label);
                }
            }
        }

        for (var, dirty) in queue.iter_mut() {
            while let Some(block) = dirty.pop() {
                for &succ in self.dom.frontier(block) {
                    if !phis[succ].contains_key(var) {
                        let phi_args =
                            cfg.preds(succ).iter().map(|l|(var,*l)).collect();

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

        // Introduce a new variable (and store it into `env) each times we store into a removed
        // stack slot. And replace each load from a removed stack slot by a move
        for instr in stmt.iter_mut() {
            if let Instr::Load{addr, dest, ..} = instr
                && self.removed.contains(addr) {
                *instr = Instr::Move(*dest, env[*addr]);
            }

            if let Instr::Store{addr, val, ..} = instr
                && self.removed.contains(addr) {
                let new_dest = cfg.fresh_var();
                env.insert(*addr, new_dest);
                *instr = Instr::Move(new_dest, *val);
            }
        }

        cfg.set_block_stmt(block, stmt);

        // Rename the arguments of the new phi expressions that refer to this block
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

        // Recursive call on the dominator tree
        let blocks: Vec<Label> = self.dom.childrens(block).iter().cloned().collect();

        for b in blocks {
            self.renaming(cfg, b);
        }

        self.env = env_save;
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        self.search(cfg);
        self.insert_phis(cfg);
        self.renaming(cfg, cfg.entry());

        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for block in blocks {
            let mut stmt: Vec<Instr> =
                self.phis[block].iter().map(|(_, phi)| phi.clone()).collect();

            stmt.extend(cfg[block].stmt.iter().cloned());
            cfg.set_block_stmt(block, stmt);
        }
    }
}
