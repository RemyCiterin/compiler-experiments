use crate::ssa::*;
use slotmap::*;
use crate::union_find::*;

pub struct Conventionalize {
    copies: SecondaryMap<Label, Vec<(Var, Lit)>>,
}

pub trait HasMove: Instruction {
    fn mv(dest: Var, src: Lit) -> Self;
}

/// HasPhi must be generate enough such that instructions from the general IR and the machine
/// specific ISA can use it (even if they don't use Lit directly), this is why `from_phi` take a
/// list of variables as arguments instead of literals
pub trait HasPhi: Instruction {
    fn to_phi(&self) -> Option<(Var, Vec<(Lit, Label)>)>;
    fn from_phi(dest: Var, args: Vec<(Var, Label)>) -> Self;
}

impl HasMove for Instr {
    fn mv(dest: Var, src: Lit) -> Self {
        Instr::Move(dest, src)
    }
}

impl HasPhi for Instr {
    fn from_phi(dest: Var, args: Vec<(Var, Label)>) -> Self {
        Instr::Phi(dest, args.into_iter().map(|(v,l)| (Lit::Var(v),l)).collect())
    }

    fn to_phi(&self) -> Option<(Var, Vec<(Lit, Label)>)> {
        match self.clone() {
            Instr::Phi(dest, args) => Some((dest, args)),
            _ => None
        }
    }
}

impl Conventionalize {
    pub fn new<I: Instruction>(cfg: &Cfg<I>) -> Self {
        let mut copies = SecondaryMap::new();

        for (b, _) in cfg.iter_blocks() {
            copies.insert(b, vec![]);
        }

        Self { copies }
    }

    /// Conventionalize the ssa form: ensure that each phi expressions is of the form
    /// `Phi(x0, Var(x1), ... Var(xn))` where x0, x1, ..., x2 doesn't interfer with each other
    pub fn run<I: HasPhi + HasMove>(&mut self, cfg: &mut Cfg<I>) {
        let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

        for &block in blocks.iter() {
            let mut stmt = cfg[block].stmt.clone();

            for instr in stmt.iter_mut() {
                if let Some((dest, args)) = instr.to_phi() {
                    let mut new_vars: Vec<(Var, Label)> = vec![];
                    for (old_lit, label) in args {
                        let new_var = cfg.fresh_var();
                        self.copies[label].push((new_var, old_lit));
                        new_vars.push((new_var, label));
                    }

                    *instr = I::from_phi(dest, new_vars);
                }
            }

            cfg.set_block_stmt(block, stmt);
        }

        // For each blocks, we solve potential parallel copies by introducing new variables, then
        // we push the generated moves just before the exit of the block
        for &block in blocks.iter() {
            let mut copies = std::mem::take(&mut self.copies[block]);
            let mut moves: Vec<I> = vec![];

            while let Some((dst, src)) = copies.pop() {
                // If any other copy read from `dst`, we need to save the current value
                // of `dst` into another fresh variable
                let found = copies
                    .iter()
                    .any(|(_, l)| l == &Lit::Var(dst));

                if found {
                    let tmp = cfg.fresh_var();
                    moves.push(I::mv(tmp, Lit::Var(dst)));
                    for copy in copies.iter_mut() {
                        if let (_, Lit::Var(src2)) = copy && src2 == &dst {
                            *src2 = tmp;
                        }
                    }
                }

                moves.push(I::mv(dst, src));
            }

            let mut body = vec![];

            for instr in cfg[block].stmt.iter() {
                if instr.exit_block() { body.extend(moves.iter().cloned()); }
                body.push(instr.clone());
            }

            cfg.set_block_stmt(block, body);
        }
    }
}

pub fn out_of_ssa<I: HasPhi + HasMove>(cfg: &mut Cfg<I>) {
    cfg.end_ssa();

    let mut uf: UnionFind<Var> = UnionFind::new();

    for (v, _) in cfg.iter_vars() {
        uf.insert(v);
    }

    for (_, block) in cfg.iter_blocks() {
        for instr in block.stmt.iter() {
            if let Some((dest, args)) = instr.to_phi() {
                let root = uf.find(dest);

                for (v, _) in args.iter() {
                    uf.merge(root, uf.find(v.as_var().unwrap()));
                }
            }
        }
    }

    let blocks: Vec<Label> = cfg.iter_blocks().map(|(b,_)| b).collect();

    for block in blocks {
        let mut stmt = cfg[block].stmt.clone();

        let mut i: usize = 0;

        while i < stmt.len() {
            if let Some(_) = stmt[i].to_phi() {
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
