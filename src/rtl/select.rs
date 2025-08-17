//! Instruction selection, it take as input a C.F.G. using the ```Instr``` type of instructions
//! and return a Rtl using a custom type of operations/conditions, it take as input:
//!
//! - A closure to transform an operation (binop/unop) into a vector or Rtl instructions.
//! - A closure to transform a condition (in case of a branch) into a vector of Rtl instructions.
//!
//! These closures have access to the instruction selector itself, as example to interpret the
//! literals presents in the original CFG into variables (e.g. replacing a literal
//! `Lit::Stack(slot)` into a move to a fresh virtual register using the eval_lit function).
//!
//! This instructions selection pass handle automatically the Phi/Return/Load/Store/Move/Jump
//! instructions.
//!
//! This module also define some macros to define simplification rules, as example, one can
//! define a rule to simplify additions with known integers:
//!
//! ```
//! pub fn translate_operation
//!     (select: &mut Selection<RvOp, RvCond>, instr: &Instr, dest: Var) -> Vec<RvInstr> {
//!
//!     let rule =
//!         translate_operation_rule!(
//!             ( Add x (int y) ), valid_arch_immediate(y),
//!             select dest =>
//!                 vec![RInstr::Operation(dest, ArchOp::Unop(ArchOpUnop::Add, y), vec![x])]
//!         );
//!
//!     let result =
//!         search_pattern(rule.pattern(), &mut select.old, instr);
//!     if let Some(occ) = result && rule.test(&occ) {
//!         return rule.transform(select, occ, dest);
//!     }
//!
//!     ...
//!
//!     unreachable!();
//! }
//! ```
//!
//! See [crate::rtl::rv32] for a full example.

use crate::ssa::*;

use slotmap::*;
use crate::pattern::*;
use super::*;


pub struct Selection<Op: Operation, Cond: Condition> {
    new: Rtl<Op, Cond>,
    pub old: Cfg<Instr>,
    labels: SecondaryMap<Label, Label>,
    vars: SparseSecondaryMap<Var, Var>,
    slots: SparseSecondaryMap<Slot, Slot>,
    stmt: Vec<RInstr<Op, Cond>>,
    label: Label,
}

impl<Op: Operation, Cond: Condition> Selection<Op, Cond> {
    pub fn new(mut old: Cfg<Instr>) -> Self {

        // Ensure phi expressions only depende on variables (no need to have instructions before
        // the phis of the generated block...)
        let mut conv = crate::out_of_ssa::Conventionalize::new(&old);
        conv.run(&mut old);

        let mut labels = SecondaryMap::new();
        let mut slots = SparseSecondaryMap::new();
        let mut vars = SparseSecondaryMap::new();
        let mut new = Cfg::new(true);

        for (b, _) in old.iter_blocks() {
            if b == old.entry() {
                labels.insert(b, new.entry());
                continue;
            }
            labels.insert(b, new.fresh_label());
        }

        for v in old.args.iter() {
            vars.insert(*v, new.fresh_arg());
        }

        for (slot, kind) in old.stack.iter() {
            match kind {
                SlotKind::Local(size) =>
                    _ = slots.insert(slot, new.fresh_stack_var(*size)),
                _ => panic!("on local stack slots are allowed until `Rtl` representation"),
            }
        }

        for (v, kind) in old.iter_vars() {
            match kind {
                VarKind::Arg => {}
                _ => {
                    vars.insert(v, new.fresh_var());
                }
            }
        }

        Self {
            label: new.entry(),
            stmt: vec![],
            labels,
            slots,
            vars,
            new,
            old,
        }
    }

    pub fn fresh(&mut self) -> Var {
        let id: Var = self.old.fresh_var();
        let new_id = self.new.fresh_var();
        self.vars.insert(id, new_id);
        id
    }

    pub fn eval_lit(&mut self, lit: Lit) -> Var {
        if let Lit::Var(v) = lit { return v; }
        let id = self.fresh();

        self.stmt.push(RInstr::Move(id, lit.clone()));
        id
    }

    fn set_block(&mut self) {
        let mut stmt = std::mem::take(&mut self.stmt);

        for instr in stmt.iter_mut() {
            for label in instr.labels_mut() {
                *label = self.labels[*label];
            }

            for var in instr.operands_mut() {
                *var = self.vars[*var];
            }

            if let Some(dest) = instr.destination_mut() {
                *dest = self.vars[*dest];
            }

            if let RInstr::Phi(_, args) = instr {
                for (_, label) in args.iter_mut() {
                    *label = self.labels[*label];
                }
            }

            if let RInstr::LoadLocal{addr, ..} = instr {
                *addr = self.slots[*addr];
            }

            if let RInstr::StoreLocal{addr, ..} = instr {
                *addr = self.slots[*addr];
            }

            if let RInstr::Move(_, Lit::Stack(slot)) = instr {
                *slot = self.slots[*slot];
            }
        }

        self.new.set_block_stmt(self.labels[self.label], stmt);
    }

    fn translate_instr<F1, F2>
        (&mut self, instr: &Instr, tr_op: F1, tr_cond: F2)
    where
        F1: Fn (&mut Self, &Instr, Var) -> Vec<RInstr<Op, Cond>>,
        F2: Fn (&mut Self, Lit, Label, Label) -> Vec<RInstr<Op, Cond>> {

        match instr {
            Instr::Binop(dest, _, _, _) => {
                let ops = tr_op(self, instr, *dest);
                self.stmt.extend(ops)
            }
            Instr::Branch(lit, l1, l2) => {
                let ops = tr_cond(self, lit.clone(), *l1, *l2);
                self.stmt.extend(ops)
            }
            Instr::Unop(dest, _, _) => {
                let ops = tr_op(self, instr, *dest);
                self.stmt.extend(ops)
            }
            Instr::Return(lit) => {
                let id = self.eval_lit(lit.clone());
                self.stmt.push(RInstr::Return(id));
            }
            Instr::Move(dest, lit) =>
                self.stmt.push(RInstr::Move(*dest, lit.clone())),
            Instr::Call(dest, name, args) => {
                let args =
                    args.iter().map(|l|self.eval_lit(l.clone())).collect();
                self.stmt.push(RInstr::Call(*dest, name.clone(), args));
            }
            Instr::Jump(label) =>
                self.stmt.push(RInstr::Jump(*label)),
            Instr::Phi(dest, args) => {
                let args =
                    args.iter()
                    .map(|(v,l)| (self.eval_lit(v.clone()), *l)).collect();
                self.stmt.push(RInstr::Phi(*dest, args));
            }
            Instr::Load{dest, addr: Lit::Stack(slot), volatile: false} => {
                self.stmt.push(RInstr::LoadLocal{dest: *dest, addr: *slot});
            }
            Instr::Store{val, addr: Lit::Stack(slot), volatile: false} => {
                let val = self.eval_lit(val.clone());
                self.stmt.push(RInstr::StoreLocal{val, addr: *slot});
            }
            Instr::Load{dest, addr, volatile} => {
                let addr = self.eval_lit(addr.clone());
                self.stmt.push(RInstr::Load{dest: *dest, addr, volatile: *volatile});
            }
            Instr::Store{val, addr, volatile} => {
                let val = self.eval_lit(val.clone());
                let addr = self.eval_lit(addr.clone());
                self.stmt.push(RInstr::Store{val, addr, volatile: *volatile});
            }
        }
    }

    pub fn run<F1, F2>
        (&mut self, tr_op: F1, tr_cond: F2)
    where
        F1: Fn (&mut Self, &Instr, Var) -> Vec<RInstr<Op, Cond>>,
        F2: Fn (&mut Self, Lit, Label, Label) -> Vec<RInstr<Op, Cond>> {

        for label in self.old.labels() {
            self.label = label;

            for instr in self.old[label].stmt.clone() {
                self.translate_instr(&instr, &tr_op, &tr_cond);
            }

            self.set_block();
        }
    }

    pub fn rtl(self) -> Rtl<Op, Cond> { self.new }
}

#[macro_export]
macro_rules! translate_operation_rule {
    ( $p:tt , $test:expr , $select:ident $dest:ident =>  $transform:expr ) => {
        OpRule::new(
            pattern!($p),
            |occ| {declare_pattern_vars!(occ, $p); $test},
            |select, occ, v| {
                #[allow(unused_variables)]
                let $dest = v;
                eval_pattern_vars!(select, occ, $p);
                #[allow(unused_variables)]
                let $select = select;
                $transform
            }
        )
    }
}

#[macro_export]
macro_rules! translate_condition_rule {
    ( $p:tt , $test:expr , $select:ident $l1:ident $l2:ident =>  $transform:expr ) => {
        CondRule::new(
            pattern!($p),
            |occ| {declare_pattern_vars!(occ, $p); $test},
            |select, occ, l1, l2| {
                #[allow(unused_variables)]
                let $l1 = l1;
                #[allow(unused_variables)]
                let $l2 = l2;
                eval_pattern_vars!(select, occ, $p);
                #[allow(unused_variables)]
                let $select = select;
                $transform
            }
        )
    }
}


pub struct OpRule<'a, Op: Operation, Cond: Condition> {
    pub pattern: Pattern,
    pub test: Box<dyn Fn(&Occurence) -> bool + 'a>,
    pub transform:
        Box<dyn Fn(&mut Selection<Op, Cond>, Occurence, Var) -> Vec<RInstr<Op, Cond>> + 'a>,
}

impl<'a, Op: Operation, Cond: Condition> OpRule<'a, Op, Cond> {
    pub fn new<F1, F2>(pattern: Pattern, test: F1, tr: F2) -> Self
        where
            F1: Fn(&Occurence) -> bool + 'a,
            F2: Fn(&mut Selection<Op, Cond>, Occurence, Var) -> Vec<RInstr<Op, Cond>> + 'a {
        Self {
            pattern,
            test: Box::new(test),
            transform: Box::new(tr),
        }
    }

    pub fn pattern(&self) -> Pattern {
        self.pattern.clone()
    }

    pub fn test(&self, occ: &Occurence) -> bool {
        let test = &self.test;
        test(occ)
    }

    pub fn transform(
        &self,
        select: &mut Selection<Op, Cond>,
        occ: Occurence,
        dest: Var
    ) -> Vec<RInstr<Op, Cond>> {
        let transform = &self.transform;
        transform(select, occ, dest)
    }
}

pub struct CondRule<'a, Op: Operation, Cond: Condition> {
    pub pattern: Pattern,
    pub test: Box<dyn Fn(&Occurence) -> bool + 'a>,
    pub transform:
        Box<dyn
            Fn(&mut Selection<Op, Cond>, Occurence, Label, Label) -> Vec<RInstr<Op, Cond>> + 'a
        >,
}

impl<'a, Op: Operation, Cond: Condition> CondRule<'a, Op, Cond> {
    pub fn new<F1, F2>(pattern: Pattern, test: F1, tr: F2) -> Self
        where F1: Fn(&Occurence) -> bool + 'a,
        F2: Fn(&mut Selection<Op, Cond>,Occurence, Label, Label) -> Vec<RInstr<Op, Cond>> + 'a {
        Self {
            pattern,
            test: Box::new(test),
            transform: Box::new(tr),
        }
    }

    pub fn pattern(&self) -> Pattern {
        self.pattern.clone()
    }

    pub fn test(&self, occ: &Occurence) -> bool {
        let test = &self.test;
        test(occ)
    }

    pub fn transform(
        &self,
        select: &mut Selection<Op, Cond>,
        occ: Occurence,
        l1: Label,
        l2: Label
    ) -> Vec<RInstr<Op, Cond>> {
        let transform = &self.transform;
        transform(select, occ, l1, l2)
    }
}
