use crate::parallel_copies::*;


use crate::interference::*;
use crate::ssa::*;
use super::*;

use slotmap::*;

pub type Coloring = SparseSecondaryMap<Var, usize>;

/// Prepare coloring, in particular it introduce copies and precolor registers for
/// calls/return instructions
pub fn prepare_coloring<A: Arch>(cfg: &mut Cfg<RInstr<A::Op, A::Cond>>) -> Coloring {
    todo!()
}
