use crate::ssa::*;
use slotmap::*;

use std::collections::HashMap;

use crate::ast::{Binop, Unop};

new_key_type!{
    pub struct Id;
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "id{}", lsb)
        } else {
            write!(f, "id{}_{}", lsb, msb)
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Expr {
    Binop(Binop, Id, Id),
    Unop(Unop, Id),
    Phi(Vec<(Id, Label)>),
    La(String),
    Li(isize),
}

/// Global Value Numbering
pub struct Gvn {
    var2id: HashMap<Var, Id>,
    expr2id: HashMap<Expr, Id>,
    alloc: SlotMap<Id, ()>,
}

impl Gvn {
    pub fn new() -> Self {
        Self {
            var2id: HashMap::new(),
            expr2id: HashMap::new(),
            alloc: SlotMap::with_key(),
        }
    }

    pub fn encode_var(&self, var: Var) -> Option<Id> {
        self.var2id.get(&var).cloned()
    }

    pub fn encode_instr(&self, instr: &Instr) -> Option<Expr> {
        match instr {
            Instr::Binop(_, binop, v1, v2) => {
                let i1 = self.encode_var(*v1)?;
                let i2 = self.encode_var(*v2)?;
                Some(Expr::Binop(*binop, i1, i2))
            }
            Instr::Phi(_, vars) => {
                let mut ret = vec![];
                for (var, label) in vars.iter() {
                    let i = self.encode_var(*var)?;
                    ret.push((i, *label));
                }
                Some(Expr::Phi(ret))
            }
            Instr::Unop(_, unop, v) => {
                let i = self.encode_var(*v)?;
                Some(Expr::Unop(*unop, i))
            }
            Instr::Li(_, i) =>
                Some(Expr::Li(*i)),
            Instr::La(_, i) =>
                Some(Expr::La(i.clone())),
            _ => None
        }
    }

    pub fn add_instr(&mut self, instr: &Instr) {
        if let Some(var) = instr.destination() {
            if let Instr::Move(v1, v2) = instr {
                self.add_move(*v1, *v2);
                return;
            }

            if let Instr::Phi(v1, vars) = instr && vars.len() == 1 {
                self.add_move(*v1, vars[0].0);
                return;
            }

            if let Some(expr) = self.encode_instr(instr) {
                if let Some(&id) = self.expr2id.get(&expr) {
                    self.var2id.insert(var, id);
                } else {
                    let id = self.alloc.insert(());
                    self.expr2id.insert(expr, id);
                    self.var2id.insert(var, id);
                }
            }
        }
    }

    pub fn add_move(&mut self, v1: Var, v2: Var) {
        if let Some(&id) = self.var2id.get(&v2) {
            self.var2id.insert(v1, id);
        }
    }

    fn fuel(&self) -> usize {
        self.var2id.len() + self.expr2id.len() + self.alloc.len()
    }

    pub fn run_analyse(&mut self, cfg: &Cfg) {
        let order = cfg.preorder();

        loop {
            let fuel = self.fuel();

            for &block in order.iter() {

                for instr in cfg[block].stmt.iter() {
                    self.add_instr(instr);
                }
            }

            if self.fuel() == fuel { break; }
        }
    }

    pub fn show(&self) {
        for (&var, &id) in self.var2id.iter() {
            println!("{} := {}", var, id);
        }
    }
}
