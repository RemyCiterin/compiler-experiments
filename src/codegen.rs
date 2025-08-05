use crate::interference::InterferenceGraph;
// use crate::liveness::Liveness;
use crate::ssa::*;
use slotmap::*;

new_key_type!{
    pub struct Color;
}

pub struct Coloring {
    pub var2color: SparseSecondaryMap<Var, Color>,
}

pub struct RegisterAllocator {
    pub matrix: InterferenceGraph,
}

pub trait Arch {
    type Register: Clone + std::fmt::Display + Ord + Eq + std::hash::Hash;
    type Opcode: Clone + std::fmt::Display + Ord + Eq + std::hash::Hash;

    /// List all the callee saved registers
    fn callee_save() -> Vec<Self::Register>;

    /// List all the callee saved registers
    fn caller_save() -> Vec<Self::Register>;

    /// List of the function parameters (must be caller saved)
    fn params() -> Vec<Self::Register>;

    /// Result register (must be callee saved)
    fn result() -> Self::Register;

    /// Declare a function using it's name, arguments, stack variables
    fn declare_function(cfg: &Cfg<Instr>, ) -> Vec<Self::Opcode>;

    //fn entry(cfg: &Cfg<Instr>, coloring: )
    //fn encode_block(stmt: Box<Instr>, coloring: ) -> Vec<Self::Opcode>;
}

//pub enum Rv32Instr {
//    Add(),
//}
