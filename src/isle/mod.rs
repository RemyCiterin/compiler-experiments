pub mod generated;

use crate::ssa::{Lit, Var, VarKind, SlotKind};
use std::collections::BTreeSet;
//use smallvec::*;
use slotmap::*;

pub type Instr = crate::ssa::InstrId;
pub type Binop = crate::ssa::COp;
pub type Unop = crate::ssa::COp;
pub type Label = crate::ssa::Label;
pub type Slot = crate::ssa::Slot;
pub type CallArgs = (String, Vec<Var>);
pub type PhiArgs = Vec<(Lit, Label)>;
pub type MemopKind = crate::ssa::MemopKind;

use generated::*;

pub type COp = crate::ssa::COp;
pub type CCond = crate::ssa::CCond;
pub type I = crate::ssa::Instr<COp,CCond>;
pub type Cfg = crate::ssa::Cfg<COp,CCond>;

//type SVec<T> = SmallVec<[T;4]>;

pub trait MachineInstr {
    /// Return if an instruction is a move between two registers
    fn is_move(&self) -> bool;

    /// Return if the current instruciton is a PHI
    fn is_phi(&self) -> Option<(&Var, &Vec<(Lit,Label)>)>;

    /// Return if the current instruciton is a PHI
    fn is_phi_mut(&mut self) -> Option<(&mut Var, &mut Vec<(Lit,Label)>)>;

    /// Generate a move instruction
    fn gen_move(dst: Var, src: Lit) -> Self;

    /// Generate a jump
    fn gen_jump(label: Label) -> Self;

    /// Return the set of destination registers of an instruction
    fn destinations(&self) -> Vec<Var>;

    /// Return the (mutable) list of destination registers of an instruction
    fn destinations_mut(&mut self) -> Vec<&mut Var>;

    /// Return the list of operands of an instruction
    fn operands(&self) -> Vec<Var>;

    /// Return the (mutable) list of operands of an instruction
    fn operands_mut(&mut self) -> Vec<&mut Var>;

    /// Generate a list of labels of an instruction
    fn targets(&self) -> Vec<Label>;

    /// Generate a list of slots of an instruction
    fn slots(&self) -> Vec<Slot>;

    /// Generate a list of (mutable) labels of an instruction
    fn targets_mut(&mut self) -> Vec<&mut Label>;

    /// Generate a list of (mutable) slots of an instruction
    fn slots_mut(&mut self) -> Vec<&mut Slot>;
}

impl MachineInstr for MInstr {
    fn is_move(&self) -> bool {
        matches!(self, MInstr::Move{..})
    }

    fn is_phi(&self) -> Option<(&Var, &Vec<(Lit,Label)>)> {
        match self {
            Self::Phi{dest, args} => Some((dest,args)),
            _ => None
        }
    }

    fn is_phi_mut(&mut self) -> Option<(&mut Var, &mut Vec<(Lit,Label)>)> {
        match self {
            Self::Phi{dest, args} => Some((dest,args)),
            _ => None
        }
    }

    fn gen_move(dest: Var, src: Lit) -> Self {
        match src {
            Lit::Int(imm) => MInstr::MoveInt{dest, imm},
            Lit::Var(rs1) => MInstr::Move{dest, rs1},
            Lit::Addr(addr) => MInstr::MoveAddr{dest, addr},
            Lit::Stack(slot) => MInstr::MoveSlot{dest, slot},
            Lit::Undef => MInstr::Nop
        }
    }

    fn gen_jump(label: Label) -> Self {
        MInstr::Jump{label}
    }

    fn destinations(&self) -> Vec<Var> {
        match self {
            MInstr::OpR{dest,..}
            | MInstr::OpRR{dest, ..}
            | MInstr::OpRI{dest, ..}
            | MInstr::Move{dest, ..}
            | MInstr::MoveInt{dest, ..}
            | MInstr::MoveAddr{dest, ..}
            | MInstr::MoveSlot{dest, ..}
            | MInstr::Load{dest, ..}
            | MInstr::LoadLocal{dest, ..}
            | MInstr::Phi{dest, ..}
            | MInstr::Call{dest, ..} =>
                vec![*dest],
            _ =>
                vec![],

        }
    }

    fn destinations_mut(&mut self) -> Vec<&mut Var> {
        match self {
            MInstr::OpR{dest,..}
            | MInstr::OpRR{dest, ..}
            | MInstr::OpRI{dest, ..}
            | MInstr::Move{dest, ..}
            | MInstr::MoveInt{dest, ..}
            | MInstr::MoveAddr{dest, ..}
            | MInstr::MoveSlot{dest, ..}
            | MInstr::Load{dest, ..}
            | MInstr::LoadLocal{dest, ..}
            | MInstr::Call{dest, ..}
            | MInstr::Phi{dest, ..} =>
                vec![dest],
            MInstr::Nop
            | MInstr::Store{..}
            | MInstr::StoreLocal{..}
            | MInstr::BranchR{..}
            | MInstr::BranchRR{..}
            | MInstr::Jump{..}
            | MInstr::Return{..} =>
                vec![],
        }
    }

    fn operands(&self) -> Vec<Var> {
        match self {
            MInstr::Call{args: (_, args), ..} =>
                args.iter().cloned().collect(),
            MInstr::Phi{args, ..} =>
                args.iter().filter_map(|(l,_)| {
                    if let Lit::Var(v) = l {Some(*v)}
                    else {None}
                }).collect(),
            MInstr::OpRR{rs1, rs2, ..}
            | MInstr::Store{val: rs1, addr: rs2, ..}
            | MInstr::BranchRR{rs1, rs2, ..} =>
                vec![*rs1, *rs2],
            MInstr::OpRI{rs1, ..}
            | MInstr::OpR{rs1, ..}
            | MInstr::Load{addr: rs1, ..}
            | MInstr::StoreLocal{val: rs1, ..}
            | MInstr::BranchR{rs1, ..}
            | MInstr::Return{rs1}
            | MInstr::Move{rs1, ..} =>
                vec![*rs1],
            MInstr::Nop
            | MInstr::Jump{..}
            | MInstr::MoveSlot{..}
            | MInstr::MoveAddr{..}
            | MInstr::LoadLocal{..}
            | MInstr::MoveInt{..} =>
                vec![]
        }
    }

    fn operands_mut(&mut self) -> Vec<&mut Var> {
        match self {
            MInstr::Call{args: (_, args), ..} =>
                args.iter_mut().collect(),
            MInstr::Phi{args, ..} =>
                args.iter_mut().filter_map(|(l,_)| {
                    if let Lit::Var(v) = l {Some(v)}
                    else {None}
                }).collect(),
            MInstr::OpRR{rs1, rs2, ..}
            | MInstr::Store{val: rs1, addr: rs2, ..}
            | MInstr::BranchRR{rs1, rs2, ..} =>
                vec![rs1, rs2],
            MInstr::OpRI{rs1, ..}
            | MInstr::OpR{rs1, ..}
            | MInstr::Load{addr: rs1, ..}
            | MInstr::StoreLocal{val: rs1, ..}
            | MInstr::BranchR{rs1, ..}
            | MInstr::Return{rs1}
            | MInstr::Move{rs1, ..} =>
                vec![rs1],
            MInstr::Nop
            | MInstr::Jump{..}
            | MInstr::MoveSlot{..}
            | MInstr::MoveAddr{..}
            | MInstr::LoadLocal{..}
            | MInstr::MoveInt{..} =>
                vec![]
        }
    }

    fn targets(&self) -> Vec<Label> {
        match self {
            MInstr::BranchR{l1, l2, ..}
            | MInstr::BranchRR{l1, l2, ..} =>
                vec![*l1, *l2],
            MInstr::Jump{label} =>
                vec![*label],
            MInstr::Call{..}
            | MInstr::Phi{..}
            | MInstr::OpRR{..}
            | MInstr::Store{..}
            | MInstr::OpRI{..}
            | MInstr::OpR{..}
            | MInstr::Load{..}
            | MInstr::StoreLocal{..}
            | MInstr::Return{..}
            | MInstr::Move{..}
            | MInstr::Nop
            | MInstr::MoveSlot{..}
            | MInstr::MoveAddr{..}
            | MInstr::LoadLocal{..}
            | MInstr::MoveInt{..} =>
                vec![]
        }
    }

    fn slots(&self) -> Vec<Slot> {
        match self {
            MInstr::StoreLocal{addr, ..}
            | MInstr::LoadLocal{addr, ..}
            | MInstr::MoveSlot{slot: addr, ..} =>
                vec![*addr],
            MInstr::Call{..}
            | MInstr::BranchR{..}
            | MInstr::BranchRR{..}
            | MInstr::Jump{..}
            | MInstr::Phi{..}
            | MInstr::OpRR{..}
            | MInstr::Store{..}
            | MInstr::OpRI{..}
            | MInstr::OpR{..}
            | MInstr::Load{..}
            | MInstr::Return{..}
            | MInstr::Move{..}
            | MInstr::Nop
            | MInstr::MoveAddr{..}
            | MInstr::MoveInt{..} =>
                vec![]
        }
    }

    fn targets_mut(&mut self) -> Vec<&mut Label> {
        match self {
            MInstr::BranchR{l1, l2, ..}
            | MInstr::BranchRR{l1, l2, ..} =>
                vec![l1, l2],
            MInstr::Jump{label} =>
                vec![label],
            MInstr::Call{..}
            | MInstr::Phi{..}
            | MInstr::OpRR{..}
            | MInstr::Store{..}
            | MInstr::OpRI{..}
            | MInstr::OpR{..}
            | MInstr::Load{..}
            | MInstr::StoreLocal{..}
            | MInstr::Return{..}
            | MInstr::Move{..}
            | MInstr::Nop
            | MInstr::MoveSlot{..}
            | MInstr::MoveAddr{..}
            | MInstr::LoadLocal{..}
            | MInstr::MoveInt{..} =>
                vec![]
        }
    }

    fn slots_mut(&mut self) -> Vec<&mut Slot> {
        match self {
            MInstr::StoreLocal{addr, ..}
            | MInstr::LoadLocal{addr, ..}
            | MInstr::MoveSlot{slot: addr, ..} =>
                vec![addr],
            MInstr::Call{..}
            | MInstr::BranchR{..}
            | MInstr::BranchRR{..}
            | MInstr::Jump{..}
            | MInstr::Phi{..}
            | MInstr::OpRR{..}
            | MInstr::Store{..}
            | MInstr::OpRI{..}
            | MInstr::OpR{..}
            | MInstr::Load{..}
            | MInstr::Return{..}
            | MInstr::Move{..}
            | MInstr::Nop
            | MInstr::MoveAddr{..}
            | MInstr::MoveInt{..} =>
                vec![]
        }
    }
}

pub struct Rtl {
    blocks: SlotMap<Label, Vec<MInstr>>,

    entry: Label,

    vars: SlotMap<Var, VarKind>,

    pub stack: SlotMap<Slot, SlotKind>,

    preds: SecondaryMap<Label, BTreeSet<Label>>,

    pub args: Vec<Var>,
}

impl std::ops::Index<Label> for Rtl {
    type Output = [MInstr];

    fn index(&self, name: Label) -> &[MInstr] {
        &self.blocks[name]
    }
}

impl Rtl {
    pub fn new() -> Self {
    let mut blocks = SlotMap::with_key();
        let mut preds = SecondaryMap::new();
        let entry: Label = blocks.insert(vec![]);
        preds.insert(entry, BTreeSet::new());
        Self {
            stack: SlotMap::with_key(),
            vars: SlotMap::with_key(),
            args: vec![],
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

    pub fn set_block_stmt(&mut self, label: Label, stmt: Vec<MInstr>) {
        let old = std::mem::take(&mut self.blocks[label]);

        for mi in old {
            for next in mi.targets().iter() {
                self.preds[*next].remove(&label);
            }
        }

        for mi in stmt.iter() {
            for next in mi.targets().iter() {
                self.preds[*next].insert(label);
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

    pub fn fresh_arg(&mut self) -> Var {
        let arg = self.vars.insert(VarKind::Arg);
        self.args.push(arg);
        arg
    }
}

pub struct Translator{
    /// Static-Single-Assignment representation
    cfg: Cfg,

    /// Register-Transfers-Language representation
    rtl: Rtl,

    /// Machine instructions of the currenty decoded instruction (in-order)
    instr_stmt: Vec<MInstr>,

    /// Machine instructions of the currently decoded block (in reverse order)
    block_stmt: Vec<MInstr>,

    /// Currently decoded instruction
    current_instr: Instr,

    /// Say if a variable is used: we generate the instructions in post-order and set
    /// all the destinations of PHI instruction first as used, so if an instruction is
    /// unused and doesn't have any side effect then this instruction is dead.
    used: SecondaryMap<Var, bool>,

    /// Map all the variables from `cfg` into variables from `rtl`
    vars: SecondaryMap<Var, Var>,

    /// Map all the slots from `cfg` into slots from `rtl`
    slots: SecondaryMap<Slot, Slot>,

    /// Map all the labels from `cfg` into labels from `rtl`
    labels: SecondaryMap<Label, Label>,
}

impl Translator {
    pub fn new(cfg: Cfg) -> Self {
        let mut rtl = Rtl::new();

        let mut used = SecondaryMap::new();
        let mut vars = SecondaryMap::new();
        let mut slots = SecondaryMap::new();
        let mut labels = SecondaryMap::new();

        for (slot,kind) in cfg.stack.iter() {
            slots.insert(slot, rtl.fresh_slot(*kind));
        }

        labels.insert(cfg.entry(), rtl.entry());
        for (label,_) in cfg.iter_blocks() {
            if label == cfg.entry() { continue; }
            labels.insert(label, rtl.fresh_label());
        }

        for var in cfg.args.iter().copied() {
            vars.insert(var, rtl.fresh_arg());
            used.insert(var, false);
        }

        for (var, kind) in cfg.iter_vars() {
            if matches!(kind, VarKind::Arg) {continue;}
            vars.insert(var, rtl.fresh_var());
            used.insert(var, false);
        }

        for (_, blk) in cfg.iter_blocks() {
            for instr in blk.stmt.iter() {
                if let I::Phi(..) = instr {
                    for var in instr.operands() {
                        used[var] = true;
                    }
                }
            }
        }

        Self{
            current_instr: (cfg.entry(),0),
            instr_stmt: vec![],
            block_stmt: vec![],
            labels,
            slots,
            vars,
            used,
            rtl,
            cfg,
        }
    }

    /// Remap all the variables/labels/slots into ones from `rtl`
    pub fn remap(&self, instr: &mut MInstr) {
        for op in instr.operands_mut() {
            *op = self.vars[*op];
        }

        for op in instr.destinations_mut() {
            *op = self.vars[*op];
        }

        for slot in instr.slots_mut() {
            *slot = self.slots[*slot];
        }

        for label in instr.targets_mut() {
            *label = self.labels[*label];
        }

        if let Some((_,args)) = instr.is_phi_mut() {
            for (_,l) in args.iter_mut() {
                *l = self.labels[*l];
            }
        }
    }

    pub fn translate_block(&mut self, block: Label) {
        for pos in (0..self.cfg[block].stmt.len()).rev() {
            let id = (block, pos);
            self.current_instr = id;

            let mut used = self.cfg[id].may_have_side_effect();
            if let Some(dest) = self.cfg[id].destination() {
                used |= self.used[dest];
            }

            if !used {continue;}
            let new_instr = constructor_lower(self, id);
            self.instr_stmt.push(new_instr);

            for mi in self.instr_stmt.iter() {
                for op in mi.operands() {
                    self.used[op] = true;
                }
            }

            self.instr_stmt.reverse();
            self.block_stmt.extend(std::mem::take(&mut self.instr_stmt));
        }

        self.block_stmt.reverse();
        let mut stmt = std::mem::take(&mut self.block_stmt);

        for mi in stmt.iter_mut() {
            self.remap(mi);
        }

        self.rtl.set_block_stmt(self.labels[block], stmt);
    }

    pub fn translate(mut self) -> Rtl {
        for label in self.cfg.labels() {
            self.translate_block(label);
        }

        self.rtl
    }
}

impl std::fmt::Display for Rtl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "entry: {} args:", self.entry())?;
        for arg in self.args.iter() {
            write!(f, " {}", arg)?;
        }

        write!(f, "\nstack:")?;
        for (slot, kind) in self.stack.iter() {
            write!(f, " [{slot}; {:?}]", kind)?;
        }

        write!(f, "\n")?;

        for (name, block) in self.blocks.iter() {
            // Use an empty line between each block
            write!(f, "\n{}:", name)?;

            for instr in block.iter() {
                write!(f, "\n\t{:?}", instr)?;
            }

            write!(f, "\n")?;
        }

        Ok(())
    }
}

impl Context for Translator {
    fn rv_imm(&mut self, imm: i32) -> Option<i16> {
        if imm >= -2048 && imm <= 2047 {Some(imm as i16)} else {None}
    }

    fn fresh_var(&mut self) -> Var {
        let old = self.cfg.fresh_var();
        let new = self.rtl.fresh_var();
        self.used.insert(old,false);
        self.vars.insert(old, new);
        new
    }

    fn destination(&mut self) -> Var {
        let instr = &self.cfg[self.current_instr];
        instr.destination().unwrap()
    }

    fn assign_var(&mut self, mi: &MInstr) -> Var {
        self.instr_stmt.push(mi.clone());
        *mi.destinations().first().unwrap()
    }

    // Return the instruction that define a given variable, and skip some moves if possible
    fn def_instr(&mut self, mut var: Var) -> Option<Instr> {
        let mut ret = None;

        while let VarKind::Local(label, pos) = self.cfg[var] {
            ret = Some((label, pos));

            if let I::Move(_, Lit::Var(v)) = self.cfg[(label,pos)] {
                var = v;
            } else {break;}
        }

        ret
    }

    fn binop_extract(&mut self, id: Instr) -> Option<(COp, Var, Var)> {
        match &self.cfg[id] {
            I::Operation(_, op, args) =>
                if args.len() == 2 {Some((*op,args[0],args[1]))} else {None},
            _ => None
        }
    }

    fn unop_extract(&mut self, id: Instr) -> Option<(COp, Var)> {
        match &self.cfg[id] {
            I::Operation(_, op, args) =>
                if args.len() == 1 {Some((*op,args[0]))} else {None},
            _ => None
        }
    }

    fn jump_extract(&mut self, id: Instr) -> Option<Label> {
        match &self.cfg[id] {
            I::Jump(label) => Some(*label),
            _ => None
        }
    }

    fn branch_extract(&mut self, id: Instr) -> Option<(Var,Label,Label)> {
        match &self.cfg[id] {
            I::Branch(_, arg, l1, l2) => Some((arg[0],*l1,*l2)),
            _ => None
        }
    }

    fn immediate_extract(&mut self, id: Instr) -> Option<i32> {
        match &self.cfg[id] {
            I::Move(_, Lit::Int(i)) => Some(*i),
            _ => None
        }
    }

    fn move_extract(&mut self, id: Instr) -> Option<Var> {
        match &self.cfg[id] {
            I::Move(_, Lit::Var(v)) => Some(*v),
            _ => None
        }
    }

    fn slot_extract(&mut self, id: Instr) -> Option<Slot> {
        match &self.cfg[id] {
            I::Move(_, Lit::Stack(s)) => Some(*s),
            _ => None
        }
    }

    fn addr_extract(&mut self, id: Instr) -> Option<String> {
        match &self.cfg[id] {
            I::Move(_, Lit::Addr(s)) => Some(s.clone()),
            _ => None
        }
    }

    fn undef_extract(&mut self, id: Instr) -> Option<()> {
        match &self.cfg[id] {
            I::Move(_, Lit::Undef) => Some(()),
            _ => None
        }
    }

    fn return_extract(&mut self, id: Instr) -> Option<Var> {
        match &self.cfg[id] {
            I::Return(v) => Some(*v),
            _ => None
        }
    }

    fn call_extract(&mut self, id: Instr) -> Option<CallArgs> {
        match &self.cfg[id] {
            I::Call(_,name,args) => Some((name.clone(), args.clone())),
            _ => None
        }
    }

    fn phi_extract(&mut self, id: Instr) -> Option<PhiArgs> {
        match &self.cfg[id] {
            I::Phi(_,args) => Some(args.clone()),
            _ => None
        }
    }

    fn load_extract(&mut self, id: Instr) -> Option<(Var,MemopKind)> {
        match &self.cfg[id] {
            I::Load{addr,kind,..} => Some((*addr,*kind)),
            _ => None
        }
    }

    fn load_local_extract(&mut self, id: Instr) -> Option<(Slot,MemopKind)> {
        match &self.cfg[id] {
            I::LoadLocal{addr,kind,..} => Some((*addr,*kind)),
            _ => None
        }
    }

    fn store_extract(&mut self, id: Instr) -> Option<(Var,Var,MemopKind)> {
        match &self.cfg[id] {
            I::Store{val, addr,kind,..} => Some((*val, *addr,*kind)),
            _ => None
        }
    }

    fn store_local_extract(&mut self, id: Instr) -> Option<(Var,Slot,MemopKind)> {
        match &self.cfg[id] {
            I::StoreLocal{val, addr,kind,..} =>
                Some((*val, *addr,*kind)),
            _ => None
        }
    }
}
