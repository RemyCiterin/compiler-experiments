/// RTL is an intermediate representation between instruction selection and
/// register allocation, it still contains virtual registers and Phi instructions
/// but it dosn't requires to use an SSA form and use machine specific instructions
/// that may write into multiple registers at a time.

use crate::ssa::{Label, Slot, Var, SlotKind, Lit, VarKind, Cfg, COp, CCond, Instr, InstrId};
use slotmap::{SlotMap, SecondaryMap};
use std::collections::BTreeSet;


// Type of machine instructions
pub enum RInstr<MInstr> {
    Phi(Var, Vec<(Lit,Label)>),
    MInstr(MInstr),
}

pub trait MachineInstr {
    /// Return if an instruction is a move between two registers
    fn is_move(&self) -> bool;

    /// Return the set of destination registers of an instruction
    fn destinations(&self) -> &[Var];

    /// Return the (mutable) list of destination registers of an instruction
    fn destinations_mut(&mut self) -> &mut [Var];

    /// Return the list of operands of an instruction
    fn operands(&self) -> &[Var];

    /// Return the (mutable) list of operands of an instruction
    fn operands_mut(&mut self) -> &mut [Var];

    /// Generate a
    fn labels(&self) -> &[Label];

    /// Generate a move instruction
    fn gen_move(dst: Var, src: Lit) -> Self;

    /// Generate a jump
    fn gen_jump(label: Label) -> Self;
}

pub struct Rtl<MInstr> {
    blocks: SlotMap<Label, Vec<RInstr<MInstr>>>,

    entry: Label,

    vars: SlotMap<Var, VarKind>,

    pub stack: SlotMap<Slot, SlotKind>,

    preds: SecondaryMap<Label, BTreeSet<Label>>,
}

impl<MInstr> std::ops::Index<Label> for Rtl<MInstr> {
    type Output = [RInstr<MInstr>];

    fn index(&self, name: Label) -> &[RInstr<MInstr>] {
        &self.blocks[name]
    }
}

impl<MInstr: MachineInstr> Rtl<MInstr> {
    pub fn new() -> Self {
    let mut blocks = SlotMap::with_key();
        let mut preds = SecondaryMap::new();
        let entry: Label = blocks.insert(vec![]);
        preds.insert(entry, BTreeSet::new());
        Self {
            stack: SlotMap::with_key(),
            vars: SlotMap::with_key(),
            blocks,
            preds,
            entry
        }
    }

    pub fn entry(&self) -> Label {
        self.entry
    }

    pub fn fresh_label(&mut self) -> Label {
        let label = self.blocks.insert(vec![]);
        self.preds.insert(label, BTreeSet::new());
        label
    }

    pub fn set_block_stmt(&mut self, label: Label, stmt: Vec<RInstr<MInstr>>) {
        let old = std::mem::take(&mut self.blocks[label]);

        for instr in old {
            if let RInstr::MInstr(mi) = instr {
                for next in mi.labels().iter() {
                    self.preds[*next].remove(&label);
                }
            }
        }

        for instr in stmt.iter() {
            if let RInstr::MInstr(mi) = instr {
                for next in mi.labels().iter() {
                    self.preds[*next].insert(label);
                }
            }
        }

        self.blocks[label] = stmt;
    }

    pub fn fresh_slot(&mut self, kind: SlotKind) -> Slot {
        self.stack.insert(kind)
    }

    pub fn fresh_var(&mut self) -> Var {
        self.vars.insert(VarKind::Undef)
    }

    pub fn fresh_var_with(&mut self, kind: VarKind) -> Var {
        self.vars.insert(kind)
    }

    pub fn fresh_arg(&mut self) -> Var {
        self.vars.insert(VarKind::Arg)
    }
}



//pub struct RtlTranslator<MInstr> {
//    rtl: Rtl<MInstr>,
//    cfg: Cfg<COp, CCond>,
//    vars: SecondaryMap<Var, Var>,
//    labels: SecondaryMap<Label, Label>,
//    slots: SecondaryMap<Slot, Slot>,
//}
//
//impl<MInstr> std::ops::Index<Var> for RtlTranslator<MInstr> {
//    type Output = Var;
//
//    fn index(&self, x: Var) -> &Var {
//        &self.vars[x]
//    }
//}
//
//impl<MInstr> std::ops::Index<Slot> for RtlTranslator<MInstr> {
//    type Output = Slot;
//
//    fn index(&self, x: Slot) -> &Slot {
//        &self.slots[x]
//    }
//}
//
//impl<MInstr> std::ops::Index<Label> for RtlTranslator<MInstr> {
//    type Output = Label;
//
//    fn index(&self, x: Label) -> &Label {
//        &self.labels[x]
//    }
//}
//
//pub trait InstrTranslator<MInstr> {
//    fn translate_instr(cfg: &Cfg<COp,CCond>, instr: InstrId, rtl: &mut Rtl<MInstr>)
//        -> Vec<MInstr>;
//}
//
//impl<MInstr: MachineInstr> RtlTranslator<MInstr> {
//    pub fn cfg(&self) -> &Cfg<COp, CCond> {
//        &self.cfg
//    }
//
//    pub fn new(cfg: Cfg<COp, CCond>) -> Self {
//        let mut rtl = Rtl::new();
//        let mut vars = SecondaryMap::new();
//        let mut labels = SecondaryMap::new();
//        let mut slots = SecondaryMap::new();
//
//        for (old, kind) in cfg.iter_vars() {
//            let new = rtl.fresh_var_with(*kind);
//            vars.insert(old, new);
//        }
//
//        for (old, kind) in cfg.stack.iter() {
//            let new = rtl.fresh_slot(*kind);
//            slots.insert(old, new);
//        }
//
//        for (old, _) in cfg.iter_blocks() {
//            let new = rtl.fresh_label();
//            labels.insert(old, new);
//        }
//
//        Self {
//            labels,
//            slots,
//            vars,
//            rtl,
//            cfg,
//        }
//    }
//
//    pub fn exit(self) -> Rtl<MInstr> {
//        self.rtl
//    }
//}
