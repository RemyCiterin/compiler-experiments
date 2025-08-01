use crate::interference::InterferenceGraph;
use crate::liveness::Liveness;
use crate::ssa::*;


pub struct RegisterAllocator {
    matrix: InterferenceGraph,
}
