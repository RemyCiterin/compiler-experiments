use crate::dominance::*;
use crate::ssa::*;
use slotmap::*;

use std::collections::*;

use crate::ast::{Binop, Unop};

new_key_type!{
    pub struct Value;
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "val{}", lsb)
        } else {
            write!(f, "val{}_{}", lsb, msb)
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    /// Constant integer
    Int(i32),

    /// Constant address
    Addr(String),

    /// Local stack slot
    Stack(Slot),

    /// Unary operation
    Unop(Unop, Value),

    /// Binary operation
    Binop(Binop, Value, Value),

    /// A blackbox variable
    Reg(Var),
}

pub struct ValueTable {
    /// Associate an expression and a variable to each value
    values: SlotMap<Value, Option<Var>>,

    /// Associate a value to each known expression
    exprs: HashMap<Expr, Value>,

    /// Added expressions, for backtracking
    added: Vec<Expr>,
}

impl ValueTable {
    pub fn new() -> Self {
        Self {
            values: SlotMap::with_key(),
            exprs: HashMap::new(),
            added: Vec::new(),
        }
    }

    pub fn insert(&mut self, expr: Expr, var: Option<Var>) -> Value {
        if let Some(val) = self.exprs.get(&expr) { return *val; }

        let value = self.values.insert(var);
        self.exprs.insert(expr.clone(), value);
        self.added.push(expr);
        value
    }

    pub fn insert_with(&mut self, expr: Expr, value: Value) {
        if let Some(val) = self.exprs.get(&expr) { assert!(*val == value); }
        self.exprs.insert(expr.clone(), value);
        self.added.push(expr);
    }

    pub fn eval_lit(&mut self, lit: Lit) -> Value {
        match lit {
            Lit::Int(i) => self.insert(Expr::Int(i), None),
            Lit::Addr(s) => self.insert(Expr::Addr(s), None),
            Lit::Var(v) => self.insert(Expr::Reg(v), Some(v)),
            Lit::Stack(slot) => self.insert(Expr::Stack(slot), None),
        }
    }

    pub fn insert_instr(&mut self, instr: Instr) {
        match instr {
            Instr::Binop(dest, binop, lit1, lit2) => {
                let v1 = self.eval_lit(lit1);
                let v2 = self.eval_lit(lit2);
                let v = self.insert(Expr::Binop(binop, v1, v2), Some(dest));
                self.insert_with(Expr::Reg(dest), v);
            }
            Instr::Unop(dest, unop, lit) => {
                let v1 = self.eval_lit(lit);
                let v = self.insert(Expr::Unop(unop, v1), Some(dest));
                self.insert_with(Expr::Reg(dest), v);
            }
            Instr::Move(dest, lit) => {
                let v = self.eval_lit(lit);
                self.insert_with(Expr::Reg(dest), v);
            }
            _ => {}
        }
    }

    pub fn update_operand(&self, lit: &mut Lit) {
        if let Lit::Var(var) = lit {
            if let Some(value) = self.exprs.get(&Expr::Reg(*var)) {
                if let Some(copy) = self.values[*value] {
                    *lit = Lit::Var(copy);
                }
            }
        }
    }

    /// `self.added` must be empty at input
    pub fn run_on_block(&mut self, cfg: &mut Cfg<Instr>, dom: &Dominance, block: Label) {
        assert!(self.added.len() == 0);

        let mut stmt = vec![];
        for mut instr in cfg[block].stmt.clone() {
            for lit in instr.literals_mut() {
                self.update_operand(lit);
            }

            self.insert_instr(instr.clone());
            stmt.push(instr);
        }

        cfg.set_block_stmt(block, stmt);

        let added = std::mem::take(&mut self.added);

        for &child in dom.childrens(block).iter() {
            self.run_on_block(cfg, dom, child);
        }

        for expr in added {
            self.exprs.remove(&expr);
        }
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        for &arg in cfg.args.iter() {
            self.insert(Expr::Reg(arg), Some(arg));
        }

        self.added.clear();

        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        self.run_on_block(cfg, &dom, cfg.entry());
    }
}
