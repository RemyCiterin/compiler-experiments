use crate::ssa::*;

use crate::union_find::*;

pub struct Simplifier {
    uf: UnionFind<Var>,
    removed: Vec<Var>,
}

impl Simplifier {
    pub fn new(cfg: &Cfg) -> Self {
        // This simplification phase use an union find to simulate the fact that a variable has
        // been replaced by another
        let mut uf = UnionFind::new();

        for (var, _) in cfg.iter_vars() {
            uf.insert(var);
        }

        Self { uf, removed: Vec::new() }
    }

    fn keep_var(&self, v: Var) -> bool {
        self.uf.find(v) == v
    }

    fn search(&mut self, preorder: &Vec<Label>, cfg: &Cfg) -> bool {
        let mut progress = false;

        // Use preorder (reverse post-order) to minimize the number of passes
        for &block in preorder.iter() {
            for instr in cfg[block].stmt.iter() {
                // Look if we can keep this phi expression if not already removed
                if let Instr::Phi(x, args) = instr && self.keep_var(*x) {
                    let args: std::collections::BTreeSet<Var> =
                        args.iter()
                        .map(|(v,_)| self.uf.find(*v))
                        .filter(|v| v != x)
                        .collect();

                    // If only one variable exception `x` is used in the phi expression,
                    // then the expression is just an identity expression: we can remove it
                    if args.len() == 1 {
                        let new_root = *args.first().unwrap();
                        self.uf.merge(new_root, *x);
                        self.removed.push(*x);
                        progress = true;
                    }
                }
            }
        }

        return progress;
    }

    pub fn run(&mut self, cfg: &mut Cfg) {
        let preorder = cfg.preorder();

        loop {
            let progress = self.search(&preorder, cfg);

            if !progress { break; }
        }

        for block in preorder {
            let mut stmt = cfg[block].stmt.clone();

            let mut i: usize = 0;
            while i < stmt.len() {
                if let Some(x) = stmt[i].destination() && !self.keep_var(x) {
                    stmt.remove(i);
                    continue;
                }

                for x in stmt[i].operands_mut() {
                    *x = self.uf.find(*x);
                }

                i += 1;
            }

            cfg.set_block_stmt(block, stmt);
        }

        for &var in self.removed.iter() {
            cfg.remove_var(var);
        }
    }
}
