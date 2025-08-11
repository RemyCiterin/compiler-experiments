use crate::ast::*;
use crate::ssa::*;
use slotmap::*;

use std::collections::BTreeSet;

pub struct InstCombine {
    /// SSA graph of the current RTL representation
    graph: SparseSecondaryMap<Var, BTreeSet<InstrId>>,

    /// Worklist
    worklist: Vec<InstrId>,
}

impl InstCombine {
    pub fn new<I: Instruction>(cfg: &Cfg<I>) -> Self {
        let mut worklist = vec![];

        for block in cfg.preorder() {
            for i in 0..cfg[block].stmt.len() {
                worklist.push((block,i));
            }
        }

        Self {
            worklist,
            graph: cfg.ssa_graph(),
        }
    }

    pub fn run<I: Instruction, F>(&mut self, cfg: &mut Cfg<I>, mut f: F)
    where F: FnMut(&Cfg<I>, &mut I) {

        while let Some(id) = self.worklist.pop() {
            let mut instr: I = cfg[id].clone();

            // Simplify the current instruction
            f(cfg, &mut instr);
            let progress = &instr != &cfg[id];
            if !progress {continue;}


            // Remove the old definition of the instruction from the SSA graph
            for x in cfg[id].operands() {
                self.graph[x].remove(&id);
            }

            // Add the new definition of the instruction to the SSA graph
            for x in instr.operands() {
                self.graph[x].insert(id);
            }

            // Propagate the instruction using the worklist
            if let Some(dest) = instr.destination() {
                for &x in self.graph[dest].iter() {
                    self.worklist.push(x);
                }
            }

            // Update the control flow graph with the new instruction
            cfg.set_instr(id.0, id.1, instr);
        }
    }
}


pub fn combine_generic_instr(cfg: &Cfg<Instr>, instr: &mut Instr) {
    let def_var = |x: Var| {
        let VarKind::Local(label, pos) = cfg[x] else { return None; };
        Some(&cfg[(label, pos)])
    };

    let def_lit = |lit: &Lit| {
        let Lit::Var(x) = lit else { return None; };
        def_var(*x)
    };

    // Constant propagation
    for lit in instr.literals_mut() {
        let Some(Instr::Move(_, l)) = def_lit(lit) else { continue; };
        *lit = l.clone();
    }

    // Canonicalize commutative binop
    if let Instr::Binop(dest, binop, l1, l2) = &instr
        && binop.commutative() {
        match (l1, l2) {
            (Lit::Var(_), _) => {}
            (_, Lit::Var(_)) => *instr = Instr::Binop(*dest, *binop, l2.clone(), l1.clone()),
            _ => {}
        }
    }

    // Constant folding on binary operation
    if let Instr::Binop(dest, binop, Lit::Int(i1), Lit::Int(i2)) = &instr {
        *instr = Instr::Move(*dest, Lit::Int(binop.eval(*i1, *i2)));
    }

    // Constant folding on unary operation
    if let Instr::Unop(dest, unop, Lit::Int(i)) = &instr {
        *instr = Instr::Move(*dest, Lit::Int(unop.eval(*i)));
    }

    // Change `if x != 0 ...` into `if x`
    if let Instr::Branch(x, l1, l2) = &instr {
        let Some(ins) = def_lit(&x) else { return; };

        match ins {
            Instr::Binop(_, Binop::NotEqual, lhs, Lit::Int(0)) => {
                *instr = Instr::Branch(lhs.clone(), *l1, *l2);
            }
            Instr::Binop(_, Binop::Equal, lhs, Lit::Int(0)) => {
                *instr = Instr::Branch(lhs.clone(), *l2, *l1);
            }
            _ => {}
        }
    }
}
