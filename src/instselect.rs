//! Instruction selection, it take as input a C.F.G. using the ```Instr``` type of instructions
//! and return a Cfg using a custom type of operations/conditions, it take as input:
//!
//! - A closure to transform an operation (binop/unop) into a vector or Cfg instructions.
//! - A closure to transform a condition (in case of a branch) into a vector of Cfg instructions.
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


pub struct Selection<Op1, Op2, Cond1, Cond2> {
    new: Cfg<Op2, Cond2>,
    pub old: Cfg<Op1, Cond1>,
    labels: SecondaryMap<Label, Label>,
    vars: SparseSecondaryMap<Var, Var>,
    slots: SparseSecondaryMap<Slot, Slot>,
    stmt: Vec<Instr<Op2, Cond2>>,
    label: Label,
}

impl<Op1: Operation, Op2: Operation, Cond1: Condition, Cond2: Condition>
Selection<Op1, Op2, Cond1, Cond2> {
    pub fn new(mut old: Cfg<Op1, Cond1>) -> Self {

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
                _ => panic!("on local stack slots are allowed until `Cfg` representation"),
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

        self.stmt.push(Instr::Move(id, lit.clone()));
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

            if let Instr::Phi(_, args) = instr {
                for (lit, label) in args.iter_mut() {
                    if let Lit::Stack(slot) = lit {
                        *slot = self.slots[*slot];
                    }
                    *label = self.labels[*label];
                }
            }

            if let Instr::LoadLocal{addr, ..} = instr {
                *addr = self.slots[*addr];
            }

            if let Instr::StoreLocal{addr, ..} = instr {
                *addr = self.slots[*addr];
            }

            if let Instr::Move(_, Lit::Stack(slot)) = instr {
                *slot = self.slots[*slot];
            }
        }

        self.new.set_block_stmt(self.labels[self.label], stmt);
    }

    fn translate_instr<F1, F2>
        (&mut self, instr: &Instr<Op1, Cond1>, tr_op: F1, tr_cond: F2)
    where
        F1: Fn (&mut Self, Var, Instr<Op1, Cond1>) -> Vec<Instr<Op2, Cond2>>,
        F2: Fn (&mut Self, Cond1, Vec<Var>, Label, Label) -> Vec<Instr<Op2, Cond2>> {

        match instr {
            Instr::Operation(dest, _, _) => {
                let ops = tr_op(self, *dest, instr.clone());
                self.stmt.extend(ops)
            }
            Instr::Branch(cond, args, l1, l2) => {
                let ops =
                    tr_cond(self, cond.clone(), args.clone(), *l1, *l2);
                self.stmt.extend(ops)
            }
            Instr::Return(id) => {
                self.stmt.push(Instr::Return(*id));
            }
            Instr::Move(dest, lit) =>
                self.stmt.push(Instr::Move(*dest, lit.clone())),
            Instr::Call(dest, name, args) => {
                self.stmt.push(Instr::Call(*dest, name.clone(), args.clone()));
            }
            Instr::Jump(label) =>
                self.stmt.push(Instr::Jump(*label)),
            Instr::Phi(dest, args) => {
                self.stmt.push(Instr::Phi(*dest, args.clone()));
            }
            Instr::LoadLocal{dest, addr: slot, kind} => {
                self.stmt.push(Instr::LoadLocal{dest: *dest, addr: *slot, kind: *kind});
            }
            Instr::StoreLocal{val, addr: slot, kind} => {
                self.stmt.push(Instr::StoreLocal{val: *val, addr: *slot, kind: *kind});
            }
            Instr::Load{dest, addr, volatile, kind} => {
                self.stmt.push(
                    Instr::Load{dest: *dest, addr: *addr, volatile: *volatile, kind: *kind});
            }
            Instr::Store{val, addr, volatile, kind} => {
                self.stmt.push(
                    Instr::Store{val: *val, addr: *addr, volatile: *volatile, kind: *kind});
            }
        }
    }

    pub fn run<F1, F2>
        (&mut self, tr_op: F1, tr_cond: F2)
    where
        F1: Fn (&mut Self, Var, Instr<Op1, Cond1>) -> Vec<Instr<Op2, Cond2>>,
        F2: Fn (&mut Self, Cond1, Vec<Var>, Label, Label) -> Vec<Instr<Op2, Cond2>> {

        for label in self.old.labels() {
            self.label = label;

            for instr in self.old[label].stmt.clone() {
                self.translate_instr(&instr, &tr_op, &tr_cond);
            }

            self.set_block();
        }
    }

    pub fn rtl(self) -> Cfg<Op2, Cond2> { self.new }
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


pub struct OpRule<'a, Op1, Op2, Cond1, Cond2> {
    pub pattern: Pattern<Op1>,
    pub test: Box<dyn Fn(&Occurence) -> bool + 'a>,
    pub transform:
        Box<dyn Fn(&mut Selection<Op1, Op2, Cond1, Cond2>, Occurence, Var)
            -> Vec<Instr<Op2, Cond2>> + 'a>,
}

impl<'a, Op1: Operation, Op2: Operation, Cond1: Condition, Cond2: Condition>
OpRule<'a, Op1, Op2, Cond1, Cond2> {
    pub fn new<F1, F2>(pattern: Pattern<Op1>, test: F1, tr: F2) -> Self
        where
            F1: Fn(&Occurence) -> bool + 'a,
            F2: Fn(&mut Selection<Op1, Op2, Cond1, Cond2>, Occurence, Var)
                -> Vec<Instr<Op2, Cond2>> + 'a {
        Self {
            pattern,
            test: Box::new(test),
            transform: Box::new(tr),
        }
    }

    pub fn pattern(&self) -> Pattern<Op1> {
        self.pattern.clone()
    }

    pub fn test(&self, occ: &Occurence) -> bool {
        let test = &self.test;
        test(occ)
    }

    pub fn transform(
        &self,
        select: &mut Selection<Op1, Op2, Cond1, Cond2>,
        occ: Occurence,
        dest: Var
    ) -> Vec<Instr<Op2, Cond2>> {
        let transform = &self.transform;
        transform(select, occ, dest)
    }
}

pub struct CondRule<'a, Op1, Op2, Cond1, Cond2> {
    pub pattern: Pattern<Op1>,
    pub test: Box<dyn Fn(&Occurence) -> bool + 'a>,
    pub transform:
        Box<dyn
            Fn(&mut Selection<Op1, Op2, Cond1, Cond2>, Occurence, Label, Label)
            -> Vec<Instr<Op2, Cond2>> + 'a
        >,
}

impl<'a, Op1: Operation, Op2: Operation, Cond1: Condition, Cond2: Condition>
CondRule<'a, Op1, Op2, Cond1, Cond2> {
    pub fn new<F1, F2>(pattern: Pattern<Op1>, test: F1, tr: F2) -> Self
        where F1: Fn(&Occurence) -> bool + 'a,
        F2: Fn(&mut Selection<Op1, Op2, Cond1, Cond2>,Occurence, Label, Label)
            -> Vec<Instr<Op2, Cond2>> + 'a {
        Self {
            pattern,
            test: Box::new(test),
            transform: Box::new(tr),
        }
    }

    pub fn pattern(&self) -> Pattern<Op1> {
        self.pattern.clone()
    }

    pub fn test(&self, occ: &Occurence) -> bool {
        let test = &self.test;
        test(occ)
    }

    pub fn transform(
        &self,
        select: &mut Selection<Op1, Op2, Cond1, Cond2>,
        occ: Occurence,
        l1: Label,
        l2: Label
    ) -> Vec<Instr<Op2, Cond2>> {
        let transform = &self.transform;
        transform(select, occ, l1, l2)
    }
}
