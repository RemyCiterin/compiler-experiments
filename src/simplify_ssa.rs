use crate::ssa::*;

use crate::union_find::*;

pub struct Simplifier {
    uf: UnionFind<Var>,
    removed: Vec<Var>,
}

impl Simplifier {
    pub fn new<Op: Operation, Cond: Condition>(cfg: &Cfg<Op, Cond>) -> Self {
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

    fn find(&self, lit: Lit) -> Lit {
        lit.when_var(|x| self.uf.find(x))
    }

    fn search<Op: Operation, Cond: Condition>
        (&mut self, preorder: &Vec<Label>, cfg: &mut Cfg<Op, Cond>) -> bool {
        let mut progress = false;

        // Use preorder (reverse post-order) to minimize the number of passes
        for &block in preorder.iter() {
            let mut stmt = cfg[block].stmt.clone();

            for instr in stmt.iter_mut() {
                // Look if we can keep this phi expression if not already removed
                if let Instr::Phi(x, args) = instr
                    && self.keep_var(*x) {

                    let args: std::collections::BTreeSet<Lit> =
                        args.iter()
                        .map(|(v,_)| self.find(v.clone()))
                        .filter(|v|
                            v != &Lit::Var(*x) && v != &Lit::Undef)
                        .collect();

                    // If only one literal (except `x`) is used in the phi expression,
                    // then the expression is just an identity expression:
                    //  - If this literal is a variable the we set this variable as the root of the
                    //  equivalence class of x (to delete the instruction, a replace each occurence
                    //  of x by this variable)
                    //  - otherwise we just replace the instruction by a move
                    if args.len() == 1 {
                        let lit = args.first().unwrap().clone();

                        if let Lit::Var(new_root) = lit {
                            self.uf.merge(new_root, *x);
                            self.removed.push(*x);
                            progress = true;
                        } else {
                            *instr = Instr::Move(*x, lit);
                        }
                    }
                }
            }

            cfg.set_block_stmt(block, stmt);
        }

        return progress;
    }

    pub fn run<Op: Operation, Cond: Condition>(&mut self, cfg: &mut Cfg<Op, Cond>) {
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
