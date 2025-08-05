use crate::dominance::*;
use crate::ssa::*;
use slotmap::*;

use std::collections::*;

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
    Move(Id),
    Phi(Vec<(Lit, Label)>),
    Addr(String),
    Int(i32),
    Var(Var),
}

impl From<Lit> for Expr {
    fn from(lit: Lit) -> Expr {
        match lit {
            Lit::Var(v) => Expr::Var(v),
            Lit::Int(i) => Expr::Int(i),
            Lit::Addr(s) => Expr::Addr(s),
        }
    }
}

impl From<Var> for Expr {
    fn from(var: Var) -> Expr {
        Expr::Var(var)
    }
}

impl Expr {
    pub fn operands(&self) -> Vec<Id> {
        match self {
            Self::Binop(_, x, y) => vec![*x, *y],
            Self::Unop(_, x) => vec![*x],
            Self::Move(x) => vec![*x],
            _ => vec![]
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Expr::Var(..))
    }

    pub fn to_instruction(&self, leader: &HashMap<Id, Var>, dest: Var) -> Instr {
        match self {
            Self::Binop(binop, x, y) =>
                Instr::Binop(dest, *binop, Lit::Var(leader[x]), Lit::Var(leader[y])),
            Self::Unop(unop, x) =>
                Instr::Unop(dest, *unop, Lit::Var(leader[x])),
            Self::Move(x) =>
                Instr::Move(dest, Lit::Var(leader[x])),
            Self::Addr(s) =>
                Instr::Move(dest, Lit::Addr(s.clone())),
            Self::Int(i) =>
                Instr::Move(dest, Lit::Int(*i)),
            _ => panic!()
        }
    }
}

pub type Leader = SecondaryMap<Label, HashMap<Id, Var>>;
pub type AntiLeader = SecondaryMap<Label, HashMap<Id, Expr>>;
pub type PhiGen = SecondaryMap<Label, HashMap<Id, Var>>;
pub type TmpGen = SecondaryMap<Label, HashSet<Var>>;

pub struct ValueTable {
    /// Map each expression to a unique id
    expr2id: HashMap<Expr, Id>,

    /// Id allocator
    alloc: SlotMap<Id, ()>,
}

impl std::ops::Index<&Expr> for ValueTable {
    type Output = Id;

    fn index(&self, expr: &Expr) -> &Id {
        &self.expr2id[expr]
    }
}

impl ValueTable {
    pub fn new() -> Self {
        Self {
            expr2id: HashMap::new(),
            alloc: SlotMap::with_key(),
        }
    }

    fn insert(&mut self, expr: Expr) -> Id {
        if !self.expr2id.contains_key(&expr) {
            self.expr2id.insert(expr.clone(), self.alloc.insert(()));
        }

        self.expr2id[&expr]
    }

    fn insert_with(&mut self, expr: Expr, id: Id) {
        let x = self.expr2id.insert(expr, id);
        assert!(x.is_none() || x.unwrap() == id);
    }

    pub fn insert_operand(&mut self, exp_gen: &mut HashMap<Id, Expr>, lit: Lit) -> Id {
        if let Lit::Var(var) = lit {
            let expr = Expr::from(var);
            let id = self.insert(expr.clone());

            if !exp_gen.contains_key(&id) {
                exp_gen.insert(id, expr);
            }

            return id;
        }

        self.insert(Expr::from(lit))
    }

    fn maybe_insert_instr(&mut self, instr: Instr, exp_gen: &mut HashMap<Id, Expr>)
        -> Result<(Option<Var>, Expr), Option<Var>> {
        let res = match instr {
            Instr::Binop(dest, binop, lhs, rhs) => {
                let i1 = self.insert_operand(exp_gen, lhs);
                let i2 = self.insert_operand(exp_gen, rhs);
                let expr = Expr::Binop(binop, i1, i2);
                let id = self.insert(expr.clone());
                self.insert_with(Expr::from(dest), id);
                Ok((Some(dest), expr))
            },
            Instr::Unop(dest, unop, lit) => {
                let i = self.insert_operand(exp_gen, lit);
                let expr = Expr::Unop(unop, i);
                let id = self.insert(expr.clone());
                self.insert_with(Expr::from(dest), id);
                Ok((Some(dest), expr))
            },
            Instr::Move(dest, lit) => {
                let i = self.insert_operand(exp_gen, lit);
                let expr = Expr::Move(i);
                self.insert_with(expr.clone(), i);
                self.insert_with(Expr::from(dest), i);
                Ok((Some(dest), expr))
            },
            Instr::Return(lit) => {
                self.insert_operand(exp_gen, lit.clone());
                Ok((None, Expr::from(lit)))
            }
            Instr::Phi(dest, vars) => {
                let expr = Expr::Phi(vars);
                let id = self.insert(expr.clone());
                self.insert_with(Expr::from(dest), id);
                Ok((Some(dest), expr))
            }
            _ => {
                Err(instr.destination())
            }
        };

        if let Ok((_, expr)) = &res && !exp_gen.contains_key(&self[expr]) {
            exp_gen.insert(self[expr], expr.clone());
        }

        res
    }

    pub fn build_set1(
        &mut self,
        block: Label,
        dom: &Dominance,
        cfg: &Cfg<Instr>,
        exp_gen: &mut SecondaryMap<Label, HashMap<Id, Expr>>,
        phi_gen: &mut PhiGen,
        tmp_gen: &mut TmpGen,
        leader: &mut Leader) {

        for instr in cfg[block].stmt.iter() {
            match self.maybe_insert_instr(instr.clone(), &mut exp_gen[block]) {
                Ok((Some(dest), expr)) => {
                    //leader[block]
                    leader[block].entry(self[&expr]).or_insert(dest);
                    if let Expr::Phi(..) = &expr {
                        phi_gen[block].entry(self[&expr]).or_insert(dest);
                    }
                }
                Err(Some(tmp)) =>
                    _ = tmp_gen[block].insert(tmp),
                _ => {}
            }
        }

        for succ in dom.childrens(block) {
            self.build_set1(*succ, dom, cfg, exp_gen, phi_gen, tmp_gen, leader);
        }
    }
}

//  /// Global Value Numbering
//  pub struct Gvn {
//      var2id: HashMap<Var, Id>,
//      expr2id: HashMap<Expr, Id>,
//      alloc: SlotMap<Id, ()>,
//  }
//
//  impl Gvn {
//      pub fn new() -> Self {
//          Self {
//              var2id: HashMap::new(),
//              expr2id: HashMap::new(),
//              alloc: SlotMap::with_key(),
//          }
//      }
//
//      pub fn encode_var(&self, var: Var) -> Option<Id> {
//          self.var2id.get(&var).cloned()
//      }
//
//      pub fn encode_instr(&self, instr: &Instr) -> Option<Expr> {
//          match instr {
//              Instr::Binop(_, binop, v1, v2) => {
//                  let i1 = self.encode_var(*v1)?;
//                  let i2 = self.encode_var(*v2)?;
//                  Some(Expr::Binop(*binop, i1, i2))
//              }
//              //Instr::Phi(_, vars) => {
//              //    let mut ret = vec![];
//              //    for (var, label) in vars.iter() {
//              //        let i = self.encode_var(*var)?;
//              //        ret.push((i, *label));
//              //    }
//              //    Some(Expr::Phi(ret))
//              //}
//              Instr::Unop(_, unop, v) => {
//                  let i = self.encode_var(*v)?;
//                  Some(Expr::Unop(*unop, i))
//              }
//              Instr::Li(_, i) =>
//                  Some(Expr::Li(*i)),
//              Instr::La(_, i) =>
//                  Some(Expr::La(i.clone())),
//              _ => None
//          }
//      }
//
//      pub fn add_instr(&mut self, instr: &Instr) {
//          if let Some(var) = instr.destination() {
//              if let Instr::Move(v1, v2) = instr {
//                  self.add_move(*v1, *v2);
//                  return;
//              }
//
//              if let Instr::Phi(v1, vars) = instr && vars.len() == 1 {
//                  self.add_move(*v1, vars[0].0);
//                  return;
//              }
//
//              if let Some(expr) = self.encode_instr(instr) {
//                  if let Some(&id) = self.expr2id.get(&expr) {
//                      self.var2id.insert(var, id);
//                  } else {
//                      let id = self.alloc.insert(());
//                      self.expr2id.insert(expr, id);
//                      self.var2id.insert(var, id);
//                  }
//              }
//          }
//      }
//
//      pub fn add_move(&mut self, v1: Var, v2: Var) {
//          if let Some(&id) = self.var2id.get(&v2) {
//              self.var2id.insert(v1, id);
//          }
//      }
//
//      fn fuel(&self) -> usize {
//          self.var2id.len() + self.expr2id.len() + self.alloc.len()
//      }
//
//      pub fn run_analyse(&mut self, cfg: &Cfg<Instr>) {
//          let order = cfg.preorder();
//
//          loop {
//              let fuel = self.fuel();
//
//              for &block in order.iter() {
//
//                  for instr in cfg[block].stmt.iter() {
//                      self.add_instr(instr);
//                  }
//              }
//
//              if self.fuel() == fuel { break; }
//          }
//      }
//
//      pub fn show(&self) {
//          for (&var, &id) in self.var2id.iter() {
//              println!("{} := {}", var, id);
//          }
//      }
//  }
