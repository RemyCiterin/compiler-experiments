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

pub fn commutative(binop: Binop) -> bool {
    match binop {
        Binop::And
            | Binop::Or
            | Binop::Xor
            | Binop::Add
            | Binop::Equal
            | Binop::NotEqual
            => true,
        _ => false
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

pub type Expr = GenericExpr<i32, Value>;

pub fn eval_unop(unop: Unop, e: i32) -> i32 {
    match unop {
        Unop::Neg => e.wrapping_neg(),
        Unop::Not => !e,
    }
}

pub fn eval_binop(binop: Binop, lhs: i32, rhs: i32) -> i32 {
    match binop {
        Binop::And => lhs & rhs,
        Binop::Or => lhs | rhs,
        Binop::Xor => lhs ^ rhs,
        Binop::Add => lhs.wrapping_add(rhs),
        Binop::Sub => lhs.wrapping_sub(rhs),
        Binop::Sll => lhs << rhs,
        Binop::Sra => lhs >> rhs,
        Binop::Srl => ((lhs as u32) >> (rhs as u32)) as i32,
        Binop::Equal => (lhs == rhs) as i32,
        Binop::NotEqual => (lhs != rhs) as i32,
        Binop::LessThan => (lhs < rhs) as i32,
        Binop::LessEqual => (lhs <= rhs) as i32,
        Binop::ULessThan => ((lhs as u32) < (rhs as u32)) as i32,
        Binop::ULessEqual => ((lhs as u32) <= (rhs as u32)) as i32,
    }
}

impl Expr {
    pub fn canon(self) -> Self {
        match self {
            Self::Binop(binop, v1, v2) =>
                if commutative(binop) {
                    Self::Binop(binop, std::cmp::min(v1,v2), std::cmp::max(v1,v2))
                } else {
                    Self::Binop(binop, v1, v2)
                }
            _ => self,
        }
    }

    pub fn as_instr(&self, map: &SlotMap<Value, Lit>, dest: Var) -> Instr {
        match self {
            Self::Int(i) => Instr::Move(dest, Lit::Int(*i)),
            Self::Addr(a) => Instr::Move(dest, Lit::Addr(a.clone())),
            Self::Stack(s) => Instr::Move(dest, Lit::Stack(*s)),
            Self::Unop(unop, var) => Instr::Unop(dest, *unop, map[*var].clone()),
            Self::Binop(binop, lhs, rhs) =>
                Instr::Binop(dest, *binop, map[*lhs].clone(), map[*rhs].clone()),

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
                let val = self.values.insert(Lit::Var(v));
                self.vars.insert(v, val);
                val
            },
        }
    }

    pub fn insert_instr(&mut self, instr: Instr) -> Vec<Instr> {
        match &instr {
            Instr::Binop(dest, binop, Lit::Int(i1), Lit::Int(i2)) => {
                let result = eval_binop(*binop, *i1, *i2);
                let v = self.insert(Expr::Int(result), Lit::Var(*dest));
                self.vars.insert(*dest, v);
                vec![Instr::Move(*dest, Lit::Int(result))]
            }
            Instr::Unop(dest, unop, Lit::Int(i)) => {
                let result = eval_unop(*unop, *i);
                let v = self.insert(Expr::Int(result), Lit::Var(*dest));
                self.vars.insert(*dest, v);
                vec![Instr::Move(*dest, Lit::Int(result))]
            }
            Instr::Binop(dest, binop, lit1, lit2) => {
                let v1 = self.eval_lit(lit1.clone());
                let v2 = self.eval_lit(lit2.clone());
                let v = self.insert(Expr::Binop(*binop, v1, v2).canon(), Lit::Var(*dest));
                self.vars.insert(*dest, v);
                vec![instr]
            }
            Instr::Unop(dest, unop, lit) => {
                let v1 = self.eval_lit(lit.clone());
                let v = self.insert(Expr::Unop(*unop, v1).canon(), Lit::Var(*dest));
                self.vars.insert(*dest, v);
                vec![instr]
            }
            Instr::Move(dest, lit) => {
                let v = self.eval_lit(lit.clone());
                self.vars.insert(*dest, v);
                vec![instr]
            }
            _ => vec![instr],
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

            stmt.extend(self.insert_instr(instr));
        }

        cfg.set_block_stmt(block, stmt);

        self.vars.push();
        self.exprs.push();

        for &child in dom.childrens(block).iter() {
            self.run_on_block(cfg, dom, child);
        }

        self.exprs.pop();
        self.vars.pop();
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
