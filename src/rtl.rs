use crate::ast::*;
use crate::ssa::*;

pub trait Opcode: Clone + Eq + Ord + std::fmt::Debug + std::hash::Hash {
    fn arity(&self) -> usize;

    /// Opcode of a phi expression
    fn phi(args: usize) -> Self;
}
