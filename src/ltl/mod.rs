
use std::collections::{HashMap, HashSet};
use crate::rtl::regalloc::*;
use crate::rtl::*;
use crate::ssa::*;
use slotmap::*;

pub enum LInstr<Op, Cond> {
    /// A generic architecture specific operation
    Operation(Phys, Op, Vec<Phys>),

    /// A generic architecture specific jump condition
    Jcc(Cond, Vec<Phys>, usize),

    /// A move instruction
    Move(Phys, Phys),

    /// Move an integer
    Li(Phys, i32),

    /// Move a stack address
    Ls(Phys, Slot),

    /// Moove a global symbol
    La(Phys, String),

    /// A jump
    Jump(usize),

    /// Load from the current stack frame
    LoadLocal{dest: Phys, addr: Slot},

    /// Load from the current stack frame
    StoreLocal{val: Phys, addr: Slot},

    /// A load instruction
    Load{dest: Phys, addr: Phys},

    /// A store instruction
    Store{val: Phys, addr: Phys},

    /// A return instruction
    Return,

    /// Call instruction
    Call(String),
}

pub struct Ltl<A: Arch> {
    pub blocks: Vec<Vec<LInstr<A::Op, A::Cond>>>,
    pub stack: SlotMap<Slot, usize>,
}

fn write_regs<A: Arch>(cfg: &Rtl<A::Op, A::Cond>, color: &Coloring) -> HashSet<Phys> {
    let mut set = HashSet::new();

    for (_, block) in cfg.iter_blocks() {
        for instr in block.stmt.iter() {
            if let Some(dest) = instr.destination() {
                set.insert(Phys(color[dest]));
            }
        }
    }

    set
}

impl<A: Arch> Ltl<A> {
    pub fn new(mut cfg: Rtl<A::Op, A::Cond>, color: Coloring) -> Self {
        let preorder = cfg.preorder();

        let phys = |v| Phys(color[v]);

        let callee_saved: HashSet<Phys> = A::callee_saved().into_iter().collect();
        let saved: HashMap<Phys, Slot> =
            write_regs::<A>(&cfg, &color).into_iter()
            .filter_map(|p| {
                if callee_saved.contains(&p) {Some((p, cfg.fresh_stack_var(4)))}
                else {None}
            }).collect();

        let labels: HashMap<Label, usize> =
            preorder.iter().enumerate()
            .map(|(i, l)| (*l, i))
            .collect();

        let mut blocks: Vec<Vec<LInstr<A::Op, A::Cond>>> = vec![];

        for (i, block) in preorder.into_iter().enumerate() {
            let mut stmt: Vec<LInstr<A::Op, A::Cond>> = vec![];

            if block == cfg.entry() {
                for (&p, &s) in saved.iter() {
                    stmt.push(LInstr::StoreLocal{addr: s, val: p});
                }
            }

            for instr in cfg[block].stmt.iter().cloned() {

                match instr {
                    RInstr::Phi(..) => unreachable!(),
                    RInstr::Jump(label) => {
                        if labels[&label] != i + 1 {
                            stmt.push(LInstr::Jump(labels[&label]));
                        }
                    }
                    RInstr::Branch(cond, vars, l1, l2) => {
                        let vars = vars.into_iter().map(|v|phys(v)).collect();
                        stmt.push(LInstr::Jcc(cond, vars, labels[&l1]));

                        if labels[&l2] != i + 1 {
                            stmt.push(LInstr::Jump(labels[&l2]));
                        }
                    }
                    RInstr::Operation(dest, op, vars) => {
                        let vars = vars.into_iter().map(|v|phys(v)).collect();
                        stmt.push(LInstr::Operation(phys(dest), op, vars));
                    }
                    RInstr::Move(dst, Lit::Var(src)) => {
                        if color[dst] != color[src] {
                            stmt.push(LInstr::Move(phys(dst), phys(src)));
                        }
                    }
                    RInstr::Move(dst, Lit::Addr(src)) => {
                        stmt.push(LInstr::La(phys(dst), src));
                    }
                    RInstr::Move(dst, Lit::Int(src)) => {
                        stmt.push(LInstr::Li(phys(dst), src));
                    }
                    RInstr::Move(dst, Lit::Stack(src)) => {
                        stmt.push(LInstr::Ls(phys(dst), src));
                    }
                    RInstr::LoadLocal{addr, dest} => {
                        stmt.push(LInstr::LoadLocal{addr, dest: phys(dest)});
                    }
                    RInstr::Load{addr, dest} => {
                        stmt.push(LInstr::Load{addr: phys(addr), dest: phys(dest)});
                    }
                    RInstr::StoreLocal{addr, val} => {
                        stmt.push(LInstr::StoreLocal{addr, val: phys(val)});
                    }
                    RInstr::Store{addr, val} => {
                        stmt.push(LInstr::Store{addr: phys(addr), val: phys(val)});
                    }
                    RInstr::Call(_, name, _) => {
                        stmt.push(LInstr::Call(name));
                    }
                    RInstr::Return(_) => {
                        for (&p, &s) in saved.iter() {
                            stmt.push(LInstr::LoadLocal{addr: s, dest: p});
                        }
                        stmt.push(LInstr::Return);
                    }
                }

            }

            blocks.push(stmt);
        }

        Self {
            blocks,
            stack: cfg.stack
        }
    }
}

impl<Op: std::fmt::Display, Cond: std::fmt::Display> std::fmt::Display for LInstr<Op, Cond> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operation(dest, op, args) => {
                write!(f, "{dest} := {op}")?;
                for v in args { write!(f, " {v}")?; }
                Ok(())
            }
            Self::Jcc(cond, args, l1) => {
                write!(f, "{cond}")?;
                for v in args { write!(f, " {v}")?; }
                write!(f, " to {l1}")
            }
            Self::Load{dest, addr} =>
                write!(f, "{} := [{}]", dest, addr),
            Self::Store{addr, val, ..} =>
                write!(f, "[{}] := {}", addr, val),
            Self::LoadLocal{dest, addr} =>
                write!(f, "{} := [stack({})]", dest, addr),
            Self::StoreLocal{addr, val, ..} =>
                write!(f, "[stack({})] := {}", addr, val),
            Self::Move(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Self::La(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Self::Ls(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Self::Li(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Self::Jump(l) =>
                write!(f, "jump to {}", l),
            Self::Return =>
                write!(f, "ret"),
            Self::Call(name) => {
                write!(f, "call {name}")
            }
        }
    }
}
