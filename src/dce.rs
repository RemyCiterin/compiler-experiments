use crate::ssa::*;

use std::collections::HashSet;


pub struct Dce {
    used_vars: HashSet<Var>,
    visited: HashSet<Label>,
}

impl Dce {
    pub fn new() -> Self {
        Self {
            used_vars: HashSet::new(),
            visited: HashSet::new(),
        }
    }

    pub fn search<Op: Operation, Cond: Condition>(&mut self, cfg: &Cfg<Op, Cond>) {
        let mut blocks_worklist: Vec<Label> = vec![cfg.entry()];
        let mut vars_worklist: Vec<Var> = vec![];

        while let Some(block) = blocks_worklist.pop() {
            if self.visited.contains(&block) { continue; }
            self.visited.insert(block);

            for instr in cfg[block].stmt.iter() {
                if instr.may_have_side_effect() {
                    if let Some(dest) = instr.destination() {
                        self.used_vars.insert(dest);
                    }

                    for var in instr.operands() {
                        vars_worklist.push(var);
                    }
                }
            }

            for succ in cfg[block].succs() {
                blocks_worklist.push(succ);
            }
        }

        while let Some(var) = vars_worklist.pop() {
            if self.used_vars.contains(&var) { continue; }

            match cfg[var] {
                VarKind::Arg => {},
                VarKind::Undef => panic!(),
                VarKind::Local(block, pos) => {
                    if !self.visited.contains(&block) { continue; }
                    self.used_vars.insert(var);

                    let instr = &cfg[(block, pos)];
                    assert!(instr.destination() == Some(var));

                    for x in instr.operands() {
                        vars_worklist.push(x);
                    }
                }
            }
        }
    }

    pub fn run<Op: Operation, Cond: Condition>(&mut self, cfg: &mut Cfg<Op, Cond>) {
        self.search(cfg);

        for block in cfg.labels() {
            // Remove all the unreachable blocks
            if !self.visited.contains(&block) {
                cfg.set_block_stmt(block, vec![]);
                cfg.remove_block(block);
                continue;
            }

            let preds = cfg.preds(block);
            let mut stmt: Vec<Instr<Op, Cond>> = vec![];

            for mut instr in cfg[block].stmt.iter().cloned() {
                if let Some(x) = instr.destination() && !self.used_vars.contains(&x) {
                    continue;
                }

                // Remove all the meaningless arguments of phi expressions
                if let Instr::Phi(_, args) = &mut instr {
                    let mut i: usize = 0;

                    while i < args.len() {
                        if !self.visited.contains(&args[i].1) || !preds.contains(&args[i].1) {
                            args.swap_remove(i);
                            continue;
                        }

                        i += 1;
                    }
                }

                stmt.push(instr);
            }

            cfg.set_block_stmt(block, stmt);
        }
    }
}
