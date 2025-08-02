use crate::ssa::*;
use slotmap::*;
use crate::union_find::*;

pub struct Conventionalize {
    copies: SecondaryMap<Label, Vec<Instr>>,
}

impl Conventionalize {
    pub fn new(cfg: &Cfg<Instr>) -> Self {
        let mut copies = SecondaryMap::new();

        for (b, _) in cfg.iter_blocks() {
            copies.insert(b, vec![]);
        }

        Self { copies }
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for &block in blocks.iter() {
            let mut stmt = cfg[block].stmt.clone();

            for instr in stmt.iter_mut() {
                if let Instr::Phi(_, vars) = instr {
                    for (old_var, label) in vars.iter_mut() {
                        let new_var = cfg.fresh_var();
                        self.copies[*label].push(Instr::Move(new_var, *old_var));
                        *old_var = new_var;
                    }
                }
            }

            cfg.set_block_stmt(block, stmt);
        }

        for &block in blocks.iter() {
            let mut copies = std::mem::take(&mut self.copies[block]);
            let mut moves: Vec<Instr> = vec![];

            while let Some(Instr::Move(dst, src)) = copies.pop() {
                // If any other copy read from `dst`, we need to save the current value
                // of `dst` into another fresh variable
                let found = copies
                    .iter()
                    .any(|c| c.operands().first() == Some(&dst));

                if found {
                    let tmp = cfg.fresh_var();
                    moves.push(Instr::Move(tmp, dst));
                    for copy in copies.iter_mut() {
                        if let Instr::Move(_, src2) = copy && src2 == &dst {
                            *src2 = tmp;
                        }
                    }
                }

                moves.push(Instr::Move(dst, src));
            }

            let mut body = vec![];

            for instr in cfg[block].stmt.iter() {
                if instr.exit_block() { body.extend(moves.iter().cloned()); }
                body.push(instr.clone());
            }

            cfg.set_block_stmt(block, body);
        }
    }

    pub fn out_of_ssa(&mut self, cfg: &mut Cfg<Instr>) {
        cfg.end_ssa();

        let mut uf: UnionFind<Var> = UnionFind::new();

        for (v, _) in cfg.iter_vars() {
            uf.insert(v);
        }

        for (_, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                if let Instr::Phi(dest, vars) = instr {
                    let root = uf.find(*dest);

                    for (v, _) in vars.iter() {
                        uf.merge(root, uf.find(*v));
                    }
                }
            }
        }

        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for block in blocks {
            let mut stmt = cfg[block].stmt.clone();

            let mut i: usize = 0;

            while i < stmt.len() {
                if let Instr::Phi(..) = stmt[i] {
                    stmt.remove(i);
                    continue;
                }

                if let Some(x) = stmt[i].destination_mut() {
                    *x = uf.find(*x);
                }

                for x in stmt[i].operands_mut() {
                    *x = uf.find(*x);
                }

                i += 1;
            }

            cfg.set_block_stmt(block, stmt);
        }
    }
}
