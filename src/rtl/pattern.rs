use crate::ssa::*;

use slotmap::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern<Op> {
    Leaf(Lit),
    Node(Op, Vec<Pattern<Op>>),
}

pub fn as_patterns<Op: Operation, Cond: Condition>(cfg: &Cfg<Op, Cond>) ->
    SparseSecondaryMap<Var, Pattern<Op>>
{
    let mut map = SparseSecondaryMap::new();

    for block in cfg.preorder() {
        for instr in cfg[block].stmt.iter() {

            match instr {
                Instr::Operation(dest, op, args) => {
                    let patterns: Vec<Pattern<Op>> =
                        args.iter().cloned()
                        .map(|v|{
                            map.get(v).cloned()
                                .unwrap_or(Pattern::Leaf(Lit::Var(v)))
                        }).collect();

                    map.insert(*dest, Pattern::Node(op.clone(), patterns));
                }
                Instr::Move(dest, lit) =>
                    _ = map.insert(*dest, Pattern::Leaf(lit.clone())),
                Instr::Branch(..) => {}
                Instr::Return(..) => {}
                Instr::Jump(..) => {}
                Instr::LoadLocal{..} => {}
                Instr::StoreLocal{..} => {}
                Instr::Load{..} => {}
                Instr::Store{..} => {}
                Instr::Call{..} => {}
                Instr::Phi(..) => {}
            }
        }
    }

    return map;
}
