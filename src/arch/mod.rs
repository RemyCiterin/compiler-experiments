pub mod regalloc;
pub mod rv32;

use crate::ssa::*;
use std::fmt::*;

/// Physical register definition
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct Phys(pub usize);

impl std::fmt::Display for Phys {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.0)
    }
}

pub const LEN: usize = 4;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub struct PhysSet(pub [u64; LEN]);

impl PhysSet {
    pub fn new() -> Self {
        Self([0;LEN])
    }

    pub fn empty() -> Self {Self::new()}

    pub fn insert(&mut self, phys: Phys) {
        self.0[phys.0 / 64] |= 1 << (phys.0 % 64);
    }

    pub fn singleton(phys: Phys) -> Self {
        let mut set = Self::empty();
        set.0[phys.0 / 64] = 1 << (phys.0 % 64);
        set
    }

    pub fn contains(&self, phys: Phys) -> bool {
        let b = self.0[phys.0 / 64] >> (phys.0 % 64);
        (b & 1) != 0
    }

    pub fn union(&mut self, other: Self) {
        for i in 0..LEN {
            self.0[i] |= other.0[i];
        }
    }

    pub fn intersection(&mut self, other: Self) {
        for i in 0..LEN {
            self.0[i] &= other.0[i];
        }
    }

    pub fn difference(&mut self, other: Self) {
        for i in 0..LEN {
            self.0[i] &= !other.0[i];
        }
    }
}

impl FromIterator<Phys> for PhysSet {
    fn from_iter<T>(iter: T) -> Self where T: IntoIterator<Item=Phys> {
        let mut set = Self::new();
        for x in iter { set.insert(x); }
        set
    }
}

impl std::iter::Iterator for PhysSet {
    type Item = Phys;

    fn next(&mut self) -> Option<Phys> {
        for i in 0..LEN {
            if self.0[i] != 0 {
                let x = self.0[i].ilog2() as u64;
                self.0[i] &= !(1 << x);
                return Some(Phys(x as usize + 64*i));
            }
        }

        None
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub enum Reg {
    Phys(Phys),
    Virt(Var),
}

impl Reg {
    pub fn as_virt(&self) -> Option<Var> {
        match self {
            Self::Virt(virt) => Some(*virt),
            _ => None
        }
    }

    pub fn as_virt_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Virt(virt) => Some(virt),
            _ => None
        }
    }

    pub fn unwrap_virt(&self) -> Var {
        self.as_virt().unwrap()
    }

    pub fn as_phys(&self) -> Option<Phys> {
        match self {
            Self::Phys(phys) => Some(*phys),
            _ => None
        }
    }

    pub fn as_phys_mut(&mut self) -> Option<&mut Phys> {
        match self {
            Self::Phys(phys) => Some(phys),
            _ => None
        }
    }

    pub fn unwrap_phys(&self) -> Phys {
        self.as_phys().unwrap()
    }
}

pub trait Arch {
    type Cond: Condition;
    type Op: Operation;

    /// Declare the set of callee saved registers
    fn callee_saved() -> Vec<Phys>;

    /// Declare the set of caller saved registers
    fn caller_saved() -> Vec<Phys>;

    /// Declare the set of argument registers, other arguments are saved on the stack, those
    /// registers must be caller saved
    fn arg_regs() -> Vec<Phys>;

    /// Declare the register used to return the result of a function
    fn ret_reg() -> Phys;

    /// Pretty print a move between two registers
    fn pp_mv(f: &mut Formatter<'_>, dest: Phys, src: Phys) -> Result;

    /// Pretty print a move from a constant integer to a register
    fn pp_from_int(f: &mut Formatter<'_>, dest: Phys, src: i32) -> Result;

    /// Pretty print a move from a global symbol to a register
    fn pp_from_addr(f: &mut Formatter<'_>, dest: Phys, src: &str) -> Result;

    /// Pretty print a move from a stack address into a register
    fn pp_from_stack(f: &mut Formatter<'_>, dest: Phys, offset: i32) -> Result;

    /// Pretty print a basic operation
    fn pp_op(f: &mut Formatter<'_>, dest: Phys, op: Self::Op, args: Vec<Phys>) -> Result;

    /// Pretty print a conditional jump
    fn pp_jcc(f: &mut Formatter<'_>, cond: Self::Cond, args: Vec<Phys>, label: &str) -> Result;

    /// Pretty print an unconditional jump
    fn pp_jump(f: &mut Formatter<'_>, label: &str) -> Result;

    /// Pretty print a load from a local variables at address `sp + offset`
    fn pp_load_local(f: &mut Formatter<'_>, dest: Phys, offset: i32, kind: MemopKind) -> Result;

    /// Pretty print a store to a local variables at address `sp + offset`
    fn pp_store_local(f: &mut Formatter<'_>, offset: i32, val: Phys, kind: MemopKind) -> Result;

    /// Pretty print a load from a variable in a register
    fn pp_load(f: &mut Formatter<'_>, dest: Phys, addr: Phys, kind: MemopKind) -> Result;

    /// Pretty print a store to a variable in a register
    fn pp_store(f: &mut Formatter<'_>, addr: Phys, val: Phys, kind: MemopKind) -> Result;

    /// Pretty print return instruction
    fn pp_return(f: &mut Formatter<'_>) -> Result;

    /// Pretty print call instruction
    fn pp_call(f: &mut Formatter<'_>, symbol: &str) -> Result;

    /// Push some variables to the stack
    fn pp_push(f: &mut Formatter<'_>, size: i32) -> Result;

    /// Pop some variables from the stack
    fn pp_pop(f: &mut Formatter<'_>, size: i32) -> Result;

    /// Generate the stack layout for the architecture and the instructions to push/pop the stack
    /// frame at the entry of a function. In case push and pop contains multiple instruction, it's
    /// better to use one level of identation. It also take a boolean as argument to known if the
    /// procedure contains call instruction, otherwise some architecture may not store the return
    /// address and save some space in the stack
    fn gen_layout(stack: &slotmap::SlotMap<Slot, SlotKind>, contain_calls: bool) ->
        (String, String, slotmap::SparseSecondaryMap<Slot, i32>);
}
