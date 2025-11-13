pub mod generated;


pub type Var = crate::ssa::Var;
pub type Lit = crate::ssa::Lit;
pub type Instr = crate::ssa::InstrId;
pub type Binop = crate::ast::Binop;
pub type Unop = crate::ast::Unop;
pub type Label = crate::ssa::Label;
pub type Slot = crate::ssa::Slot;
pub type CallArgs = (String, Vec<Var>);
pub type PhiArgs = Vec<(Lit, Label)>;
pub type MemopKind = crate::ssa::MemopKind;
