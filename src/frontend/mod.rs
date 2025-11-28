//! This module define a higher level intermediate representation before ssa/mod.rs

pub mod llvm;

use slotmap::*;

new_key_type!{
    pub struct Ref;
}

impl std::fmt::Display for Ref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "%{}", lsb)
        } else {
            write!(f, "%{}_{}", lsb, msb)
        }
    }
}

new_key_type!{
    pub struct Label;
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "^{}", lsb)
        } else {
            write!(f, "^{}_{}", lsb, msb)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TYPE {
    /// Number of bits
    pub bits: usize,
}

pub const VOID: TYPE = TYPE{bits: 0};

pub const U1: TYPE = TYPE{bits: 1};
pub const U8: TYPE = TYPE{bits: 8};
pub const U16: TYPE = TYPE{bits: 16};
pub const U32: TYPE = TYPE{bits: 32};
pub const U64: TYPE = TYPE{bits: 64};

pub const BOOL: TYPE = U1;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Binop {
    PtrAdd, Add, Sub, Xor, And, Or, Slt, Sle, Ult, Ule, Sra, Srl, Sll, Equal, Neq,
    Mul, UDiv, URem, SDiv, SRem,
}

impl std::fmt::Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PtrAdd  => write!(f, "ptr_add"),
            Self::Add     => write!(f, "add"),
            Self::Sub     => write!(f, "sub"),
            Self::Xor     => write!(f, "xor"),
            Self::And     => write!(f, "and"),
            Self::Or      => write!(f, "or"),
            Self::Slt     => write!(f, "slt"),
            Self::Sle     => write!(f, "sle"),
            Self::Ult     => write!(f, "ult"),
            Self::Ule     => write!(f, "ule"),
            Self::Sra     => write!(f, "sra"),
            Self::Srl     => write!(f, "srl"),
            Self::Sll     => write!(f, "sll"),
            Self::Equal   => write!(f, "equal"),
            Self::Neq     => write!(f, "neq"),
            Self::Mul     => write!(f, "mul"),
            Self::UDiv    => write!(f, "udiv"),
            Self::URem    => write!(f, "urem"),
            Self::SDiv    => write!(f, "sdiv"),
            Self::SRem    => write!(f, "srem"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Instr {
    Binop{ dest: Ref, ty: TYPE, lhs: Ref, binop: Binop, rhs: Ref },
    Jump(Label),
    Move{ dest: Ref, ty: TYPE, value: Ref },
    SExt{ dest: Ref, ty: TYPE, value: Ref },
    ZExt{ dest: Ref, ty: TYPE, value: Ref },
    GetBits{ dest: Ref, ty: TYPE, value: Ref, start: usize },
    SetBits{ dest: Ref, ty: TYPE, value: Ref, item: Ref, start: usize },
    Symbol{ dest: Ref, ty: TYPE, value: String },
    Branch(Ref, Label, Label),
    Return(Ref),
    Call{ dest: Ref, ty: TYPE, func: Ref, args: Vec<Ref> },
    Constant{ dest: Ref, ty: TYPE, value: usize },
    Phi{ dest: Ref, ty: TYPE, args: Vec<(Ref, Label)> },
    Load{ dest: Ref, ty: TYPE, addr: Ref, volatile: bool, align: usize },
    Store{ addr: Ref, val: Ref, volatile: bool, align: usize },
}

impl std::fmt::Display for TYPE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self == &VOID { return write!(f, "void"); }

        write!(f, "u{}", self.bits)?;

        Ok(())
    }
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binop { dest, binop, lhs, rhs, ty } =>
                write!(f, "{dest} = {} {ty} {lhs}, {rhs}", binop),
            Self::Jump(l) =>
                write!(f, "jump {l}"),
            Self::Move { dest, value, ty } =>
                write!(f, "{dest} = move {ty} {value}"),
            Self::SExt { dest, ty, value } =>
                write!(f, "{dest} = sext {}, {value}", ty),
            Self::ZExt { dest, ty, value } =>
                write!(f, "{dest} = zext {}, {value}", ty),
            Self::GetBits { dest, ty, value, start } =>
                write!(f, "{dest} = get_bits {ty} {value}, {}, {}", start, start+ty.bits),
            Self::SetBits { dest, ty, value, item, start } =>
                write!(f, "{dest} = set_bits {ty} {value}, {item}, {}, {}", start, start+ty.bits),
            Self::Symbol { dest, ty, value } =>
                write!(f, "{dest} = symbol {ty} {value}"),
            Self::Load { dest, ty, addr, volatile: _, align } =>
                write!(f, "{dest} = load {ty}(align: {align}) {addr}"),
            Self::Store { val, addr, volatile: _, align } =>
                write!(f, "store (align: {align}) {addr}, {val}"),
            Self::Call { dest, ty, func, args } => {
                write!(f, "{dest} = call {ty} {func}(")?;
                for (i,a) in args.iter().enumerate() {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{a}")?;
                }
                write!(f, ")")
            }
            Self::Phi { dest, ty, args } => {
                write!(f, "{dest} = phi {ty}")?;
                for (r, l) in args.iter() {
                    write!(f, " ({r}, {l})")?;
                }
                Ok(())
            }
            Self::Branch(cond, l1, l2) =>
                write!(f, "branch {cond}, {l1}, {l2}"),
            Self::Return(value) =>
                write!(f, "return {value}"),
            Self::Constant { dest, ty, value } =>
                write!(f, "{dest} = const {ty} {value}"),
        }
    }
}

impl Instr {
    pub fn dest_type(&self) -> TYPE {
        match self {
            Instr::Binop{ty, ..}
                | Instr::Move{ty, ..}
                | Instr::GetBits{ty, ..}
                | Instr::SetBits{ty, ..}
                | Instr::Symbol{ty, ..}
                | Instr::Call{ty, ..}
                | Instr::Constant{ty, ..}
                | Instr::Phi{ty, ..}
                | Instr::Load{ty, ..}
                | Instr::SExt{ty, ..}
                | Instr::ZExt{ty, ..}
                => *ty,
            Instr::Branch(..)
                | Instr::Return(..)
                | Instr::Jump(..)
                | Instr::Store{..}
                => VOID,
        }
    }

    pub fn destination(&self) -> Option<Ref> {
        match self {
            Instr::Binop{dest, ..}
                | Instr::Move{dest, ..}
                | Instr::GetBits{dest, ..}
                | Instr::SetBits{dest, ..}
                | Instr::Symbol{dest, ..}
                | Instr::Constant{dest, ..}
                | Instr::Phi{dest, ..}
                | Instr::Load{dest, ..}
                | Instr::SExt{dest, ..}
                | Instr::ZExt{dest, ..}
                | Instr::Call{dest, ..}
                => Some(*dest),
            Instr::Branch(..)
                | Instr::Return(..)
                | Instr::Jump(..)
                | Instr::Store{..}
                => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RefData {
    ty: TYPE,
    kind: RefKind,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RefKind {
    /// The variable is the n-th argument
    Arg(usize),

    /// The variable is a stack variable
    Slot,

    /// The variable is define at a given position
    Local(Label, usize),

    /// The variable is not defined yet
    Undef,
}

pub struct Builder {
    pub blocks: SlotMap<Label, Vec<Instr>>,
    pub vars: SlotMap<Ref, RefData>,
    pub args: Vec<Ref>,
    pub entry: Label,

    /// Current label to be generated
    pub label: Label,

    /// Current statement to be generated
    pub stmt: Vec<Instr>,
}

impl std::ops::Index<Ref> for Builder {
    type Output = TYPE;

    fn index(&self, index: Ref) -> &TYPE {
        &self.vars[index].ty
    }
}

impl std::ops::Index<Label> for Builder {
    type Output = Vec<Instr>;

    fn index(&self, index: Label) -> &Vec<Instr> {
        &self.blocks[index]
    }
}

impl Builder {
    pub fn new() -> Self {
        let mut blocks = SlotMap::with_key();
        let entry = blocks.insert(vec![]);
        let vars = SlotMap::with_key();

        Self {
            label: entry,
            stmt: vec![],
            args: vec![],
            blocks,
            entry,
            vars,
        }
    }

    pub fn fresh_ref(&mut self, ty: TYPE) -> Ref {
        self.vars.insert(RefData{ty, kind: RefKind::Undef})
    }

    pub fn fresh_slot(&mut self, ty: TYPE) -> Ref {
        self.vars.insert(RefData{ty, kind: RefKind::Slot})
    }

    pub fn fresh_arg(&mut self, ty: TYPE) -> Ref {
        let arg = self.vars.insert(RefData{ty, kind: RefKind::Arg(self.args.len())});
        self.args.push(arg);
        return arg;
    }

    pub fn fresh_label(&mut self) -> Label {
        self.blocks.insert(vec![])
    }

    pub fn type_of(&self, r: Ref) -> TYPE {
        self.vars[r].ty
    }

    pub fn push(&mut self, instr: Instr) {
        match instr {
            Instr::GetBits { ty, value, start, .. }
                if start == 0 && ty == self[value] => {}
            Instr::SetBits { ty, item, start, .. }
                if start == 0 && ty == self[item] => {}
            Instr::ZExt { ty, value, .. }
                if ty == self[value] => {}
            Instr::SExt { ty, value, .. }
                if ty == self[value] => {}
            _ => self.stmt.push(instr),
        }
    }

    pub fn mk_select<F1, F2>(&mut self, cond: Ref, f1: F1, f2: F2) -> Ref where
        F1: FnOnce(&mut Self) -> Ref, F2: FnOnce(&mut Self) -> Ref {
        let t_begin = self.fresh_label();
        let e_begin = self.fresh_label();
        let join = self.fresh_label();

        self.push(Instr::Branch(cond, t_begin, e_begin));
        self.finish_block();

        self.label = t_begin;
        let v1 = f1(self);
        let t_end = self.label;
        self.stmt.push(Instr::Jump(join));
        self.finish_block();

        self.label = e_begin;
        let v2 = f2(self);
        let e_end = self.label;
        self.stmt.push(Instr::Jump(join));
        self.finish_block();

        assert!(self.type_of(v1) == self.type_of(v2));

        self.label = join;
        let ty = self.type_of(v1);
        let dest = self.fresh_ref(self.type_of(v1));
        self.push(Instr::Phi{dest, ty, args: vec![(v1, t_end), (v2, e_end)]});
        dest
    }

    pub fn finish_block(&mut self) {
        let stmt = std::mem::take(&mut self.stmt);
        self.write_block(self.label, stmt);
    }

    pub fn write_block(&mut self, label: Label, block: Vec<Instr>) {
        let old = std::mem::take(&mut self.blocks[label]);

        for ins in old {
            if let Some(dest) = ins.destination() {
                self.vars[dest].kind = RefKind::Undef;
            }
        }

        for (i, ins) in block.iter().enumerate() {
            if let Some(dest) = ins.destination() {
                assert!(self[dest] == ins.dest_type());
                assert!(self.vars[dest].kind == RefKind::Undef);
                self.vars[dest].kind = RefKind::Local(label, i);
            }
        }

        self.blocks[label] = block;
    }
}



impl std::fmt::Display for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "entry: {} args:", self.entry)?;
        for arg in self.args.iter() {
            write!(f, " {}", arg)?;
        }

        write!(f, "\nstack:")?;
        for (slot, data) in self.vars.iter() {
            if data.kind != RefKind::Slot { continue; }
            write!(f, " [{slot}; {}]", data.ty)?;
        }

        write!(f, "\n")?;

        for (name, block) in self.blocks.iter() {
            // Use an empty line between each block
            write!(f, "\n{}:", name)?;

            for instr in block.iter() {
                write!(f, "\n\t{}", instr)?;
            }

            write!(f, "\n")?;
        }

        Ok(())
    }
}
