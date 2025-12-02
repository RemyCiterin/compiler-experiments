use super::*;
use slotmap::*;
use crate::utils::union_find::*;

pub struct Conventionalize {
    copies: SecondaryMap<Label, Vec<(Var, Lit)>>,
}


impl Conventionalize {
    pub fn new(cfg: &Rtl) -> Self {
        let mut copies = SecondaryMap::new();

        for (b, _) in cfg.iter_blocks() {
            copies.insert(b, vec![]);
        }

        Self { copies }
    }

    /// Conventionalize the ssa form: ensure that each phi expressions is of the form
    /// `Phi(x0, Var(x1), ... Var(xn))` where x0, x1, ..., x2 doesn't interfer with each other
    pub fn run(&mut self, cfg: &mut Rtl) {
        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for &block in blocks.iter() {
            let mut stmt: Vec<MInstr> = cfg[block].iter().cloned().collect();

            for instr in stmt.iter_mut() {
                if let MInstr::Phi{dest, args} = instr {
                    let mut new_vars: Vec<(Lit, Label)> = vec![];
                    for (old_lit, label) in args {
                        // This Phi instruction reference a deleted block
                        if !self.copies.contains_key(*label) {continue;}

                        let new_var = cfg.fresh_var();
                        self.copies[*label].push((new_var, old_lit.clone()));
                        new_vars.push((Lit::Var(new_var), *label));
                    }

                    *instr = MInstr::Phi{dest: *dest, args: new_vars};
                }
            }

            cfg.set_block_stmt(block, stmt);
        }

        // For each blocks, we solve potential parallel copies by introducing new variables, then
        // we push the generated moves just before the exit of the blocks
        for block in blocks {
            let copies = std::mem::take(&mut self.copies[block]);
            let moves =
                crate::ssa::parallel_copies::copies_to_moves(|| cfg.fresh_var(), copies);

            let mut body = vec![];

            for (i, instr) in cfg[block].iter().enumerate() {
                if i == cfg[block].len() - 1 {
                    body.extend(
                        moves.iter().map(|(v,l)|
                            MInstr::gen_move(*v, l.clone())));
                }

                body.push(instr.clone());
            }

            cfg.set_block_stmt(block, body);
        }
    }
}

pub fn out_of_ssa(cfg: &mut Rtl) {
    // We first conventionalize the IR such that each arguments of a phi instruction doesn't
    // interfer with the destination or another argument of this instruction. In other terms, we can
    // canonicalize all the arguments/destinations of each phi instruction to a unique variable
    let mut conv = Conventionalize::new(cfg);
    conv.run(cfg);

    let mut uf: UnionFind<Var> = UnionFind::new();

    for (v, _) in cfg.iter_vars() {
        uf.insert(v);
    }

    // Canonicalize the arguments/destinations of the phi instructions to a unique variable using
    // a union find data structure
    for (_, block) in cfg.iter_blocks() {
        for instr in block.iter() {
            if let MInstr::Phi{dest, args} = instr {
                let root = uf.find(dest.as_virt().unwrap());

                for (v, _) in args.iter() {
                    uf.merge(root, uf.find(v.as_var().unwrap()));
                }
            }
        }
    }

    let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

    // Remove the instructions and canonicalize the variables
    for block in blocks {
        let mut stmt: Vec<MInstr> = vec![];

        for mut ins in cfg[block].iter().cloned() {
            if matches!(ins, MInstr::Phi{..}) { continue; }

            for x in ins.destinations_mut() {
                *x = uf.find(*x);
            }

            for x in ins.operands_mut() {
                *x = uf.find(*x);
            }

            stmt.push(ins);
        }

        cfg.set_block_stmt(block, stmt);
    }
}
