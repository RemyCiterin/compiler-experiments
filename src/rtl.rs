use crate::ssa::*;

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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MInstr<Op, Cond> {
    /// A generic architecture specific operation
    Operation(Var, Op, Vec<Var>),

    /// A generic architecture specific jump condition
    Branch(Cond, Vec<Var>, Label, Label),

    /// A move instruction
    Move(Var, Lit),

    /// A jump
    Jump(Label),

    /// A generic load instruction
    Load{dest: Var, addr: Var},

    /// A generic store instruction
    Store{val: Var, addr: Var},

    /// A return instruction
    Return(Var),

    /// A phy symbol
    Phi(Var, Vec<(Var, Label)>),

    /// Call instruction
    Call(Var, String, Vec<Var>),
}


impl<Op: std::fmt::Display, Cond: std::fmt::Display> std::fmt::Display for MInstr<Op, Cond> {
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


impl<Op: Operation, Cond: Condition> Instruction for MInstr<Op, Cond> {
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
            Self::Return(var) => vec![var],
            Self::Call(_, _, args) => args.iter_mut().collect(),
            Self::Phi(_, args) =>
                args.iter_mut().map(|(v,_)| v).collect(),
            Self::Jump(_) => vec![],
            _ => vec![],
        }
    }
}

pub type Rtl<Op, Cond> = Cfg<MInstr<Op, Cond>>;
