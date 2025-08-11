use crate::rtl::*;
use crate::ssa::*;

use crate::dominance::*;
use slotmap::*;

use crate::persistent_hash_map::*;

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
pub enum Expr<Op> {
    /// Constant integer
    Int(i32),

    /// Constant address
    Addr(String),

    /// Local stack slot
    Stack(Slot),

    /// Binary operation
    Operation(Op, Vec<Value>),
}

impl<Op: Operation> std::fmt::Display for Expr<Op> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::Addr(a) => write!(f, "{a}"),
            Self::Stack(s) => write!(f, "{s}"),
            Self::Operation(op, args) => {
                write!(f, "({op}")?;
                for a in args.iter() { write!(f, " {a}")?; }
                write!(f, ")")
            },
        }
    }
}

pub struct ValueTable<Op> {
    /// Associate an expression and a variable to each value
    values: SlotMap<Value, Lit>,

    /// Associate a value to each known expression
    exprs: PHashMap<Expr<Op>, Value>,

    /// A map from variables to values
    vars: PHashMap<Var, Value>,
}

impl<Op: Operation> ValueTable<Op> {
    pub fn new() -> Self {
        Self {
            values: SlotMap::with_key(),
            exprs: PHashMap::new(),
            vars: PHashMap::new(),
        }
    }

    pub fn insert(&mut self, expr: Expr<Op>, lit: Lit) -> Value {
        if let Some(val) = self.exprs.get(&expr) { return *val; }

        let value = self.values.insert(lit);
        self.exprs.insert(expr.clone(), value);
        value
    }

    pub fn eval_var(&mut self, v: Var) -> Value {
        if let Some(val) = self.vars.get(&v) { return *val; }

        let val = self.values.insert(Lit::Var(v));
        self.vars.insert(v, val);
        val
    }

    pub fn eval_lit(&mut self, lit: Lit) -> Value {
        match lit {
            Lit::Int(i) => self.insert(Expr::Int(i), Lit::Int(i)),
            Lit::Addr(s) => self.insert(Expr::Addr(s.clone()), Lit::Addr(s)),
            Lit::Stack(slot) => self.insert(Expr::Stack(slot), Lit::Stack(slot)),
            Lit::Var(v) => self.eval_var(v),
        }
    }

    /// Evaluate and register an instruction, and replace it by a move if the instruction is
    /// redondant
    pub fn insert_instr<Cond: Condition>(&mut self, instr: RInstr<Op, Cond>) -> RInstr<Op, Cond> {
        match &instr {
            RInstr::Operation(dest, op, args) => {
                let vals = args.iter().map(|v| self.eval_var(*v)).collect();
                let expr = Expr::Operation(op.clone(), vals);

                if !op.may_have_side_effect() {
                    if let Some(value) = self.exprs.get(&expr) {
                        return RInstr::Move(*dest, self.values[*value].clone());
                    }
                }

                let v = self.insert(expr, Lit::Var(*dest));
                self.vars.insert(*dest, v);
                instr
            }
            RInstr::Move(dest, lit) => {
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

    pub fn run_on_block<Cond: Condition>(&mut self, cfg: &mut Rtl<Op, Cond>, dom: &Dominance, block: Label) {
        let mut stmt = vec![];

        for instr in cfg[block].stmt.clone() {
            stmt.push(self.insert_instr(instr));
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

    pub fn run<Cond: Condition>(&mut self, cfg: &mut Rtl<Op, Cond>) {
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
