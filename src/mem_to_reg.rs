//! This module introduce a pass that search for stack slots that are safe to replace by a register
//! and introduce the necessary phi expressions and renaming to replace those stack slots by
//! registers.

use std::collections::{HashSet, HashMap};
use crate::persistent_hash_map::PHashMap;

use slotmap::*;
use crate::dominance::*;
use crate::ssa::*;

pub struct MemToReg {
    /// The set of stack slots to remove
    removed: HashSet<Slot>,

    /// Necessary phi instruction per label/variable
    phis: SecondaryMap<Label, HashMap<Slot, (Var, Vec<(Lit, Label)>)>>,

    /// Current environment of variables in removed, as a mapping from stack slots to fresh
    /// variables
    env: PHashMap<Slot, Var>,

    /// Dominance tree/dominance frontier
    dom: Dominance,
}

impl MemToReg {
    pub fn new(cfg: &Cfg<Instr>) -> Self {
        let mut removed = HashSet::new();

        for (slot, _) in cfg.stack.iter() {
            removed.insert(slot);
        }

        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        let env = PHashMap::new();
        let mut phis = SecondaryMap::new();

        for (b, _) in cfg.iter_blocks() {
            phis.insert(b, HashMap::new());
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
                        if let Lit::Stack(x) = val {
                            self.removed.remove(x);
                        }
                    }
                    _ => {
                        for x in instr.literals() {
                            match x {
                                Lit::Stack(s) => _ = self.removed.remove(&s),
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
    }

    /// Introduce the necessary phi expressions, note that some of those expressions are not
    /// necessary because the introduce a phi in a block for a variable that is introduced in this
    /// same block, so it's necessary to detect them later
    pub fn insert_phis(&mut self, cfg: &mut Cfg<Instr>) {
        // Insert phis each times we store into a removed stack variable

        let mut phis =
            std::mem::take(&mut self.phis);

        let mut queue: HashMap<Slot, Vec<Label>> = HashMap::new();
        for &var in self.removed.iter() {
            queue.insert(var, vec![]);
        }

        for (label, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                if let Instr::Store{addr: Lit::Stack(s), ..} = instr
                    && self.removed.contains(s) {
                    queue.get_mut(s).unwrap().push(label);
                }
            }
        }

        for (var, dirty) in queue.iter_mut() {
            while let Some(block) = dirty.pop() {
                for &succ in self.dom.frontier(block) {
                    if !phis[succ].contains_key(var) {
                        let phi_args = cfg
                            .preds(succ)
                            .iter()
                            .map(|l|(Lit::Stack(*var),*l)).collect();

                        let new = cfg.fresh_var();
                        phis[succ].insert(*var, (new, phi_args));
                        dirty.push(succ);
                    }
                }
            }
        }

        self.phis = phis;
    }

    pub fn renaming(&mut self, cfg: &mut Cfg<Instr>, block: Label) {
        self.env.push();

        for (var, (dest, _)) in self.phis[block].iter_mut() {
            self.env.insert(*var, *dest);
        }

        let mut stmt = cfg[block].stmt.clone();

        // Introduce a new variable (and store it into `env) each times we store into a removed
        // stack slot. And replace each load from a removed stack slot by a move
        for instr in stmt.iter_mut() {
            if let Instr::Load{addr: Lit::Stack(s), dest, ..} = instr
                && self.removed.contains(s) {
                *instr = Instr::Move(*dest, Lit::Var(self.env[s]));
            }

            if let Instr::Store{addr: Lit::Stack(slot), val, ..} = instr
                && self.removed.contains(slot) {
                let new_dest = cfg.fresh_var();
                self.env.insert(*slot, new_dest);
                *instr = Instr::Move(new_dest, val.clone());
            }
        }

        cfg.set_block_stmt(block, stmt);

        // Rename the arguments of the new phi expressions that refer to this block
        for succ in cfg[block].succs() {
            for (_, (_, vars)) in self.phis[succ].iter_mut() {
                for (src, label) in vars.iter_mut() {
                    // If env doesn't contains src, then this phi instruction will be deleted
                    // later cause all it's variable is not defined at idom(succ) (otherwise it
                    // must be definde at all it's predecessors)
                    if *label == block {
                        if let Lit::Stack(slot) = src {
                            if self.env.contains_key(slot) {
                                *src = Lit::Var(self.env[slot]);
                            } else {
                                *src = Lit::Undef;
                            }
                        }
                    }
                }
            }
        }

        // Recursive call on the dominator tree
        let blocks: Vec<Label> = self.dom.childrens(block).iter().cloned().collect();

        for b in blocks {
            self.renaming(cfg, b);
        }

        self.env.pop();
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        self.search(cfg);
        self.insert_phis(cfg);
        self.renaming(cfg, cfg.entry());

        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for block in blocks {
            let mut stmt: Vec<Instr> =
                self.phis[block].iter()
                .map(|(_, (dest, args))| {
                    Instr::Phi(*dest, args.clone())
                }).collect();

            stmt.extend(cfg[block].stmt.iter().cloned());
            cfg.set_block_stmt(block, stmt);
        }

        for slot in self.removed.iter() {
            cfg.remove_slot(*slot);
        }
    }
}
