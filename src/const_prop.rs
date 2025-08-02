use crate::ssa::*;
use slotmap::*;

pub struct ConstProp {
    map: SparseSecondaryMap<Var, Lit>,
}

impl ConstProp {
    pub fn new() -> Self {
        Self { map: SparseSecondaryMap::new() }
    }

    pub fn search(&mut self, cfg: &Cfg<Instr>) {
        for (_, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                if let Instr::Move(x, Lit::Int(i)) = instr {
                    self.map.insert(*x, Lit::Int(*i));
                }

                if let Instr::Move(x, Lit::Addr(s)) = instr {
                    self.map.insert(*x, Lit::Addr(s.clone()));
                }
            }
        }
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        self.search(cfg);

        for block in cfg.labels() {
            let mut stmt = cfg[block].stmt.clone();

            for instr in stmt.iter_mut() {
                for x in instr.literals_mut() {
                    if let Lit::Var(y) = x && self.map.contains_key(*y) {
                        *x = self.map[*y].clone();
                    }
                }
            }

            cfg.set_block_stmt(block, stmt);
        }
    }
}

