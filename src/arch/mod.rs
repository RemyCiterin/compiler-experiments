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
