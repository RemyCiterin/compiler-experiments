#[macro_use]
pub mod int_set;
pub mod int_map;
pub mod ast;
pub mod identifiers;
pub mod union_find;
pub mod ir;
pub mod codegen;
pub mod trie;
pub mod ssa;
pub mod liveness;
pub mod into_ssa;
pub mod builder;
pub mod dominance;
pub mod gvn;
pub mod simplify_ssa;
