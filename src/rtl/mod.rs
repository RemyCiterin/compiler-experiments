pub mod gvn;

use crate::ssa::*;
use std::fmt::*;

pub trait Arch {
    type Cond: Condition;
    type Op: Operation;
    type Reg;

    /// Declare the set of callee saved registers
    fn callee_saved() -> Vec<Self::Reg>;

    /// Declare the set of caller saved registers
    fn caller_saved() -> Vec<Self::Reg>;

    /// Declare the set of argument registers, other arguments are saved on the stack, those
    /// registers must be caller saved
    fn arg_regs() -> Vec<Self::Reg>;

    /// Declare the register used to return the result of a function
    fn ret_reg() -> Self::Reg;

    /// Pretty print a move between two registers
    fn pp_mv(f: &mut Formatter<'_>, dest: Self::Reg, src: Self::Reg) -> Result;

    /// Pretty print a move from a constant integer to a register
    fn pp_from_int(f: &mut Formatter<'_>, dest: Self::Reg, src: i32) -> Result;

    /// Pretty print a move from a global symbol to a register
    fn pp_from_addr(f: &mut Formatter<'_>, dest: Self::Reg, src: &str) -> Result;

    /// Pretty print a move from a stack address into a register
    fn pp_from_stack(f: &mut Formatter<'_>, dest: Self::Reg, offset: i32) -> Result;

    /// Pretty print a basic operation
    fn pp_op(f: &mut Formatter<'_>, dest: Self::Reg, op: Self::Op, args: Vec<Self::Reg>) -> Result;

    /// Pretty print a conditional jump
    fn pp_jcc(f: &mut Formatter<'_>, cond: Self::Cond, args: Vec<Self::Reg>, label: &str) -> Result;

    /// Pretty print an unconditional jump
    fn pp_j(f: &mut Formatter<'_>, label: &str) -> Result;

    /// Pretty print a load from a local variables at address `sp + offset`
    fn pp_load_local(f: &mut Formatter<'_>, dest: Self::Reg, offset: i32) -> Result;

    /// Pretty print a store to a local variables at address `sp + offset`
    fn pp_store_local(f: &mut Formatter<'_>, val: Self::Reg, offset: i32) -> Result;

    /// Pretty print a load from a variable in a register
    fn pp_load(f: &mut Formatter<'_>, dest: Self::Reg, addr: Self::Reg) -> Result;

    /// Pretty print a store to a variable in a register
    fn pp_store(f: &mut Formatter<'_>, dest: Self::Reg, addr: Self::Reg) -> Result;

    /// Pretty print return instruction
    fn pp_return(f: &mut Formatter<'_>) -> Result;

    /// Pretty print call instruction
    fn pp_call(f: &mut Formatter<'_>, symbol: &str) -> Result;

    /// Push some variables to the stack
    fn pp_push(f: &mut Formatter<'_>, args: Vec<Self::Reg>) -> Result;

    /// Pop some variables from the stack
    fn pp_pop(f: &mut Formatter<'_>, args: Vec<Self::Reg>) -> Result;
}

/// Define the basic operations of an architecture
pub trait Operation: Clone + Eq + Ord + std::fmt::Display + std::hash::Hash {
    /// Number of register arguments of an operation
    fn arity(&self) -> usize;
    fn eval(&self, args: Vec<i32>) -> Option<i32>;

    fn may_have_side_effect(&self) -> bool;
}

/// Define a branch condition in a specific architecture
pub trait Condition: Clone + Eq + Ord + std::fmt::Display + std::hash::Hash {
    /// Number of register arguments of a conditional jump
    fn arity(&self) -> usize;
    fn eval(&self, args: Vec<i32>) -> bool;

    fn may_have_side_effect(&self) -> bool;
}

/// RTL instructions, use architecture specific operations
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RInstr<Op, Cond> {
    /// A generic architecture specific operation
    Operation(Var, Op, Vec<Var>),

    /// A generic architecture specific jump condition
    Branch(Cond, Vec<Var>, Label, Label),

    /// A move instruction
    Move(Var, Lit),

    /// A jump
    Jump(Label),

    /// Load from the current stack frame
    LoadLocal{dest: Var, addr: Slot},

    /// Load from the current stack frame
    StoreLocal{val: Var, addr: Slot},

    /// A load instruction
    Load{dest: Var, addr: Var},

    /// A store instruction
    Store{val: Var, addr: Var},

    /// A return instruction
    Return(Var),

    /// A phy symbol
    Phi(Var, Vec<(Var, Label)>),

    /// Call instruction
    Call(Var, String, Vec<Var>),
}


impl<Op: std::fmt::Display, Cond: std::fmt::Display> std::fmt::Display for RInstr<Op, Cond> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operation(dest, op, args) => {
                write!(f, "{dest} := {op}")?;
                for v in args { write!(f, " {v}")?; }
                Ok(())
            }
            Self::Branch(cond, args, l1, l2) => {
                write!(f, "{cond}")?;
                for v in args { write!(f, " {v}")?; }
                write!(f, " to {l1} {l2}")
            }
            Self::Load{dest, addr} =>
                write!(f, "{} := [{}]", dest, addr),
            Self::Store{addr, val, ..} =>
                write!(f, "[{}] := {}", addr, val),
            Self::LoadLocal{dest, addr} =>
                write!(f, "{} := [{}]", dest, addr),
            Self::StoreLocal{addr, val, ..} =>
                write!(f, "[{}] := {}", addr, val),
            Self::Move(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Self::Jump(l) =>
                write!(f, "jump to {}", l),
            Self::Return(cond) =>
                write!(f, "return {}", cond),
            Self::Call(dest, name, args) => {
                write!(f, "{} := {}(", dest, name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                } write!(f, ")")
            }
            Self::Phi(dest, args) => {
                write!(f, "{} := phi", dest)?;
                for (var, label) in args.iter() {
                    write!(f, " ({}, {})", var, label)?;
                }
                Ok(())
            }
        }
    }
}


impl<Op: Operation, Cond: Condition> Instruction for RInstr<Op, Cond> {
    fn labels(&self) -> Vec<Label> {
        match self {
            Self::Branch(_, _, l1, l2) => vec![*l1, *l2],
            Self::Jump(l) => vec![*l],
            _ => vec![],

        }
    }

    fn labels_mut(&mut self) -> Vec<&mut Label> {
        match self {
            Self::Branch(_, _, l1, l2) => vec![l1, l2],
            Self::Jump(l) => vec![l],
            _ => vec![],

        }
    }

    fn may_have_side_effect(&self) -> bool {
        match self {
            Self::Load{..}
                | Self::Store{..}
                | Self::Call(..)
                | Self::Return(..)
                | Self::Branch(..)
                | Self::Jump(..)
                => true,
            _ => false
        }
    }

    fn exit_block(&self) -> bool {
        match self {
            Self::Branch(..)
                | Self::Jump(..)
                | Self::Return(..)
                => true,
            _ => false
        }
    }

    fn destination(&self) -> Option<Var> {
        match self {
            Self::Operation(dest, _, _) => Some(*dest),
            Self::Branch(_, _, _, _) => None,
            Self::Move(dest, _) => Some(*dest),
            Self::Load{dest, ..} => Some(*dest),
            Self::Store{..} => None,
            Self::LoadLocal{dest, ..} => Some(*dest),
            Self::StoreLocal{..} => None,
            Self::Return(_) => None,
            Self::Call(dest, _, _) => Some(*dest),
            Self::Phi(dest, _) => Some(*dest),
            Self::Jump(_) => None,
        }
    }

    fn destination_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Operation(dest, _, _) => Some(dest),
            Self::Branch(_, _, _, _) => None,
            Self::Move(dest, _) => Some(dest),
            Self::Load{dest, ..} => Some(dest),
            Self::Store{..} => None,
            Self::LoadLocal{dest, ..} => Some(dest),
            Self::StoreLocal{..} => None,
            Self::Return(_) => None,
            Self::Call(dest, _, _) => Some(dest),
            Self::Phi(dest, _) => Some(dest),
            Self::Jump(_) => None,
        }
    }

    fn operands(&self) -> Vec<Var> {
        match self {
            Self::Operation(_, _, args) => args.clone(),
            Self::Branch(_, args, _, _) => args.clone(),
            Self::Move(_, Lit::Var(v)) => vec![*v],
            Self::Load{addr, ..} => vec![*addr],
            Self::Store{val, addr} => vec![*addr, *val],
            Self::LoadLocal{..} => vec![],
            Self::StoreLocal{val, ..} => vec![*val],
            Self::Return(var) => vec![*var],
            Self::Call(_, _, args) => args.clone(),
            Self::Phi(_, args) =>
                args.iter().map(|(v,_)| *v).collect(),
            Self::Jump(_) => vec![],
            _ => vec![],
        }
    }

    fn operands_mut(&mut self) -> Vec<&mut Var> {
        match self {
            Self::Operation(_, _, args) => args.iter_mut().collect(),
            Self::Branch(_, args, _, _) => args.iter_mut().collect(),
            Self::Move(_, Lit::Var(v)) => vec![v],
            Self::Load{addr, ..} => vec![addr],
            Self::Store{val, addr} => vec![addr, val],
            Self::LoadLocal{..} => vec![],
            Self::StoreLocal{val, ..} => vec![val],
            Self::Return(var) => vec![var],
            Self::Call(_, _, args) => args.iter_mut().collect(),
            Self::Phi(_, args) =>
                args.iter_mut().map(|(v,_)| v).collect(),
            Self::Jump(_) => vec![],
            _ => vec![],
        }
    }
}

pub type Rtl<Op, Cond> = Cfg<RInstr<Op, Cond>>;
