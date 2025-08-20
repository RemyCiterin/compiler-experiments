use crate::ssa::*;

use crate::dominance::*;
use slotmap::*;

use crate::persistent_hash_map::*;

use std::collections::HashSet;

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

    /// A copy from a register
    Reg(Var),
}

impl<Op: Operation> std::fmt::Display for Expr<Op> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(v) => write!(f, "{v}"),
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
    values: SlotMap<Value, Var>,

    /// Associate a value to each known expression
    exprs: PHashMap<Expr<Op>, Value>,

    /// A set of locations where we must kills all the loads
    kill_loads: HashSet<Label>,

    /// A map from load addresses to their values, must be cleared at each store or at the
    /// begining of each block in the `self.kill_loads` set
    loads: PHashMap<Expr<Op>, Value>,
}

impl<Op: Operation> ValueTable<Op> {
    pub fn new() -> Self {
        Self {
            values: SlotMap::with_key(),
            kill_loads: HashSet::new(),
            loads: PHashMap::new(),
            exprs: PHashMap::new(),
        }
    }

    pub fn insert(&mut self, expr: Expr<Op>, var: Var) -> Value {
        if let Some(val) = self.exprs.get(&expr) { return *val; }

        let value = self.values.insert(var);
        self.exprs.insert(expr.clone(), value);
        value
    }

    pub fn insert_move(&mut self, dest: Var, lit: Lit) -> Value {
        match lit {
            Lit::Int(i) => self.insert(Expr::Int(i), dest),
            Lit::Addr(s) => self.insert(Expr::Addr(s.clone()), dest),
            Lit::Stack(slot) => self.insert(Expr::Stack(slot), dest),
            Lit::Var(v) => self.insert(Expr::Reg(v), v),
            Lit::Undef => self.insert(Expr::Reg(dest), dest),
        }
    }

    pub fn get_lit(&self, lit: Lit) -> Option<Value> {
        match lit {
            Lit::Int(i) => self.exprs.get(&Expr::Int(i)).cloned(),
            Lit::Addr(a) => self.exprs.get(&Expr::Addr(a)).cloned(),
            Lit::Stack(s) => self.exprs.get(&Expr::Stack(s)).cloned(),
            Lit::Var(v) => self.exprs.get(&Expr::Reg(v)).cloned(),
            Lit::Undef => None,
        }
    }

    pub fn insert_var(&mut self, v: Var)-> Value {
        self.insert(Expr::Reg(v), v)
    }

    /// Evaluate and register an instruction, and replace it by a move if the instruction is
    /// redondant
    pub fn insert_instr<Cond: Condition>(&mut self, instr: Instr<Op, Cond>) -> Instr<Op, Cond> {
        match &instr {
            Instr::Operation(dest, op, args) => {
                if op.may_have_side_effect() { return instr; }

                let vals = args.iter().map(|v| self.insert_var(*v)).collect();
                let expr = Expr::Operation(op.clone(), vals);

                if let Some(value) = self.exprs.get(&expr).cloned() {
                    self.exprs.insert(Expr::Reg(*dest), value);
                    return Instr::Move(*dest, Lit::Var(self.values[value].clone()));
                }

                let v = self.insert(expr, *dest);
                self.exprs.insert(Expr::Reg(*dest), v);
                instr
            }
            Instr::Move(dest, lit) => {
                if let Some(value) = self.get_lit(lit.clone()) {
                    self.exprs.insert(Expr::Reg(*dest), value);
                    return Instr::Move(*dest, Lit::Var(self.values[value].clone()));
                }

                let v = self.insert_move(*dest, lit.clone());
                self.exprs.insert(Expr::Reg(*dest), v);
                instr
            }
            Instr::Load{addr, dest, volatile: false} => {
                if let Some(value) = self.loads.get(&Expr::Reg(*addr)) {
                    self.exprs.insert(Expr::Reg(*dest), *value);
                    return Instr::Move(*dest, Lit::Var(self.values[*value].clone()));
                }

                let v = self.values.insert(*dest);
                self.loads.insert(Expr::Reg(*addr), v);
                self.exprs.insert(Expr::Reg(*dest), v);
                instr
            }
            Instr::LoadLocal{addr, dest} => {
                let mut addr_expr = Expr::Stack(*addr);

                if let Some(value) = self.exprs.get(&addr_expr) {
                    addr_expr = Expr::Reg(self.values[*value].clone());
                }

                if let Some(value) = self.loads.get(&addr_expr) {
                    self.exprs.insert(Expr::Reg(*dest), *value);
                    return Instr::Move(*dest, Lit::Var(self.values[*value].clone()));
                }

                let v = self.values.insert(*dest);
                self.loads.insert(addr_expr, v);
                self.exprs.insert(Expr::Reg(*dest), v);
                instr
            }
            Instr::StoreLocal{addr, val} => {
                let mut addr_expr = Expr::Stack(*addr);

                if let Some(value) = self.exprs.get(&addr_expr) {
                    addr_expr = Expr::Reg(self.values[*value].clone());
                }

                self.loads.clear();
                let value = self.insert_var(*val);
                self.loads.insert(addr_expr, value);
                instr
            }
            Instr::Store{addr, val, volatile: false} => {
                self.loads.clear();
                let value = self.insert_var(*val);
                self.loads.insert(Expr::Reg(*addr), value);
                instr
            }
            Instr::Call(..) | Instr::Store{..} => {
                self.loads.clear();
                instr
            }
            _ => instr,
        }
    }

    pub fn update_operand(&self, var: Var) -> Var {
        if let Some(value) = self.exprs.get(&Expr::Reg(var)) {
            return self.values[*value].clone();
        }

        return var;
    }

    pub fn run_on_block<Cond: Condition>
        (&mut self, cfg: &mut Cfg<Op, Cond>, dom: &Dominance, block: Label) {

        self.loads.push();
        self.exprs.push();

        if self.kill_loads.contains(&block) { self.loads.clear(); }

        let mut stmt = vec![];

        for mut instr in cfg[block].stmt.clone() {
            for var in instr.operands_mut() {
                *var = self.update_operand(*var);
            }

            stmt.push(self.insert_instr(instr));
        }

        cfg.set_block_stmt(block, stmt);

        for &child in dom.childrens(block).iter() {
            self.run_on_block(cfg, dom, child);
        }

        self.exprs.pop();
        self.loads.pop();
    }

    pub fn set_kill_loads<Cond: Condition>(&mut self, cfg: &Cfg<Op, Cond>, dom: &Dominance) {
        let mut dirty: Vec<Label> = Vec::new();

        for (label, block) in cfg.iter_blocks() {
            for instr in block.stmt.iter() {
                if matches!(instr, Instr::Store{..}) || matches!(instr, Instr::Call(..)) {
                    dirty.push(label);
                }
            }
        }

        let mut set = HashSet::new();

        while let Some(block) = dirty.pop() {
            for &succ in dom.frontier(block) {
                // We only insert the Phi instruction if the variable is still alive
                if set.contains(&succ) { continue; }
                set.insert(succ);
                dirty.push(succ);
            }
        }

        self.kill_loads = set;
    }

    pub fn run<Cond: Condition>(&mut self, cfg: &mut Cfg<Op, Cond>) {
        for &arg in cfg.args.iter() {
            let val = self.values.insert(arg);
            self.exprs.insert(Expr::Reg(arg), val);
        }

        self.exprs.push();
        self.loads.push();

        let mut dom = Dominance::new(cfg);
        dom.run(cfg);

        self.set_kill_loads(cfg, &dom);

        self.run_on_block(cfg, &dom, cfg.entry());

        elim_unused_instructions(cfg);
    }
}

pub fn elim_unused_instructions<Op: Operation, Cond: Condition>(cfg: &mut Cfg<Op, Cond>) {
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
