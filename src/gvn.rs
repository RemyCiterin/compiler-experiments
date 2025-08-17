use crate::dominance::*;
use crate::ssa::*;
use slotmap::*;

use crate::persistent_hash_map::*;

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
pub enum GenericExpr<Int, V> {
    /// Constant integer
    Int(Int),

    /// Constant address
    Addr(String),

    /// Local stack slot
    Stack(Slot),

    /// Unary operation
    Unop(Unop, V),

    /// Binary operation
    Binop(Binop, V, V),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::Addr(a) => write!(f, "{a}"),
            Self::Stack(s) => write!(f, "{s}"),
            Self::Unop(unop, v) => write!(f, "({unop} {v})"),
            Self::Binop(binop, v1, v2) => write!(f, "({binop} {v1} {v2})"),
        }
    }
}

pub type Expr = GenericExpr<i32, Value>;

impl Expr {
    pub fn canon(self) -> Self {
        match self {
            Self::Binop(binop, v1, v2) =>
                if binop.commutative() {
                    Self::Binop(binop, std::cmp::min(v1,v2), std::cmp::max(v1,v2))
                } else {
                    Self::Binop(binop, v1, v2)
                }
            _ => self,
        }
    }
}

pub struct ValueTable {
    /// Associate an expression and a variable to each value
    values: SlotMap<Value, Lit>,

    /// Associate a value to each known expression
    exprs: PHashMap<Expr, Value>,

    /// A map from variables to values
    vars: PHashMap<Var, Value>,
}

impl ValueTable {
    pub fn new() -> Self {
        Self {
            values: SlotMap::with_key(),
            exprs: PHashMap::new(),
            vars: PHashMap::new(),
        }
    }

    pub fn insert(&mut self, expr: Expr, lit: Lit) -> Value {
        if let Some(val) = self.exprs.get(&expr) { return *val; }

        let value = self.values.insert(lit);
        self.exprs.insert(expr.clone(), value);
        value
    }

    pub fn insert_with(&mut self, expr: Expr, value: Value) {
        if let Some(val) = self.exprs.get(&expr) { assert!(*val == value); }
        self.exprs.insert(expr.clone(), value);
    }

    pub fn eval_lit(&mut self, lit: Lit) -> Value {
        match lit {
            Lit::Int(i) => self.insert(Expr::Int(i), Lit::Int(i)),
            Lit::Addr(s) => self.insert(Expr::Addr(s.clone()), Lit::Addr(s)),
            Lit::Stack(slot) => self.insert(Expr::Stack(slot), Lit::Stack(slot)),
            Lit::Var(v) => {
                if let Some(val) = self.vars.get(&v) { return *val; }

                let val = self.values.insert(Lit::Var(v));
                self.vars.insert(v, val);
                val
            },
            Lit::Undef => {
                self.values.insert(Lit::Undef)
            }
        }
    }

    pub fn insert_instr(&mut self, instr: Instr) -> Instr {
        match &instr {
            Instr::Binop(dest, binop, Lit::Int(i1), Lit::Int(i2)) => {
                let result = binop.eval(*i1, *i2);
                Instr::Move(*dest, Lit::Int(result))
            }
            Instr::Unop(dest, unop, Lit::Int(i)) => {
                let result = unop.eval(*i);
                Instr::Move(*dest, Lit::Int(result))
            }
            Instr::Binop(dest, binop, lit1, lit2) => {
                let v1 = self.eval_lit(lit1.clone());
                let v2 = self.eval_lit(lit2.clone());
                let expr = Expr::Binop(*binop, v1, v2).canon();

                if let Some(value) = self.exprs.get(&expr) {
                    return Instr::Move(*dest, self.values[*value].clone());
                }

                let v = self.insert(expr, Lit::Var(*dest));
                self.vars.insert(*dest, v);
                instr
            }
            Instr::Unop(dest, unop, lit) => {
                let v1 = self.eval_lit(lit.clone());
                let expr = Expr::Unop(*unop, v1).canon();

                if let Some(value) = self.exprs.get(&expr) {
                    return Instr::Move(*dest, self.values[*value].clone());
                }

                let v = self.insert(expr, Lit::Var(*dest));
                self.vars.insert(*dest, v);
                instr
            }
            Instr::Move(dest, lit) => {
                let v = self.eval_lit(lit.clone());
                self.vars.insert(*dest, v);
                instr
            }
            _ => instr,
        }
    }

    pub fn update_operand(&self, lit: &Lit) -> Lit {
        if let Lit::Var(var) = lit {
            if let Some(value) = self.vars.get(&var) {
                return self.values[*value].clone()
            }
        }

        return lit.clone();
    }

    /// `self.added` must be empty at input
    pub fn run_on_block(&mut self, cfg: &mut Cfg<Instr>, dom: &Dominance, block: Label) {
        let mut stmt = vec![];

        for mut instr in cfg[block].stmt.clone() {
            for lit in instr.literals_mut() {
                *lit = self.update_operand(lit);
            }

            stmt.push(self.insert_instr(instr));
        }

        cfg.set_block_stmt(block, stmt);


        for &child in dom.childrens(block).iter() {
            self.vars.push();
            self.exprs.push();
            self.run_on_block(cfg, dom, child);
            self.exprs.pop();
            self.vars.pop();
        }

    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        for &arg in cfg.args.iter() {
            let val = self.values.insert(Lit::Var(arg));
            self.vars.insert(arg, val);
        }

        self.exprs.push();
        self.vars.push();

        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        self.run_on_block(cfg, &dom, cfg.entry());

        elim_unused_instructions(cfg);
    }
}

pub fn elim_unused_instructions<I: Instruction>(cfg: &mut Cfg<I>) {
    let graph = cfg.ssa_graph();

    for label in cfg.labels() {
        let mut stmt = vec![];

        for instr in cfg[label].stmt.iter().cloned() {
            let used =
                if let Some(dest) = instr.destination() { graph[dest].len() > 0 }
                else {false};

            if used || instr.may_have_side_effect() {
                stmt.push(instr);
            }
        }

        cfg.set_block_stmt(label, stmt);
    }
}
