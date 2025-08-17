pub mod interpreter;

use std::collections::{HashMap, HashSet};
use crate::rtl::regalloc::*;
use crate::rtl::*;
use crate::ssa::*;
use slotmap::*;

#[derive(Clone)]
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
    pub stack: SlotMap<Slot, SlotKind>,
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
                    RInstr::Move(_, Lit::Undef) => {}
                    RInstr::LoadLocal{addr, dest} => {
                        stmt.push(LInstr::LoadLocal{addr, dest: phys(dest)});
                    }
                    RInstr::Load{addr, dest, ..} => {
                        stmt.push(LInstr::Load{addr: phys(addr), dest: phys(dest)});
                    }
                    RInstr::StoreLocal{addr, val} => {
                        stmt.push(LInstr::StoreLocal{addr, val: phys(val)});
                    }
                    RInstr::Store{addr, val, ..} => {
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

    pub fn layout(&self) -> (i32, SparseSecondaryMap<Slot, i32>) {
        let mut slots: SparseSecondaryMap<Slot, i32> = SparseSecondaryMap::new();
        let mut stack_size: i32 = 0;

        let mut num_outgoing = 0;
        for (_, kind) in self.stack.iter() {
            match kind {
                SlotKind::Local(size) => {
                    //slots.insert(slot, stack_size);
                    stack_size += *size as i32;
                },
                SlotKind::Outgoing(num) =>
                    num_outgoing = usize::max(*num, num_outgoing+1),
                _ => {}
            }
        }

        stack_size += num_outgoing as i32 * 4;
        stack_size += 4;

        if stack_size % 16 != 0 {
            stack_size += 16 - (stack_size % 16);
        }

        let mut offset: i32 = 0;
        for (slot, kind) in self.stack.iter() {
            match kind {
                SlotKind::Local(size) => {
                    slots.insert(slot, stack_size - offset - 4);
                    offset += *size as i32;
                }
                SlotKind::Outgoing(num) =>
                    _ = slots.insert(slot, 4 * (*num+1) as i32),
                SlotKind::Incoming(num) =>
                    _ = slots.insert(slot, stack_size + 4 * (*num+1) as i32),
            }
        }

        (stack_size, slots)
    }

    pub fn pp(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (stack_size, slots) = self.layout();

        write!(f, "\t")?;
        A::pp_push(f, stack_size)?;
        write!(f, "\n")?;

        for (i, block) in self.blocks.iter().enumerate() {
            let from_label = |j: usize| {
                if j > i {
                    format!("{j}f")
                } else {
                    format!("{j}b")
                }
            };

            write!(f, "{}:\n", i)?;

            for instr in block.iter().cloned() {

                if matches!(instr, LInstr::Return) {
                    write!(f, "\t")?;
                    A::pp_pop(f, stack_size)?;
                    write!(f, "\n")?;
                }

                write!(f, "\t")?;
                match instr {
                    LInstr::Operation(dest, op, args) =>
                        _ = A::pp_op(f, dest, op, args)?,
                    LInstr::Jcc(cond, args, label) =>
                        _ = A::pp_jcc(f, cond, args, &from_label(label))?,
                    LInstr::Jump(label) =>
                        _ = A::pp_jump(f, &from_label(label))?,
                    LInstr::Move(dest, src) =>
                        _ = A::pp_mv(f, dest, src)?,
                    LInstr::Li(dest, src) =>
                        _ = A::pp_from_int(f, dest, src)?,
                    LInstr::Ls(dest, src) =>
                        _ = A::pp_from_int(f, dest, slots[src])?,
                    LInstr::La(dest, src) =>
                        _ = A::pp_from_addr(f, dest, &src)?,
                    LInstr::Call(name) =>
                        _ = A::pp_call(f, &name)?,
                    LInstr::Return =>
                        _ = A::pp_return(f)?,
                    LInstr::Load{addr, dest} =>
                        _ = A::pp_load(f, dest, addr)?,
                    LInstr::Store{addr, val} =>
                        _ = A::pp_store(f, addr, val)?,
                    LInstr::LoadLocal{addr, dest} =>
                        _ = A::pp_load_local(f, dest, slots[addr])?,
                    LInstr::StoreLocal{addr, val} =>
                        _ = A::pp_store_local(f, slots[addr], val)?,
                }
                write!(f, "\n")?;
            }
        }

        Ok(())

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

impl<A: Arch> std::fmt::Display for Ltl<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pp(f)
    }
}

pub enum LtlSection<A: Arch> {
    Text(Ltl<A>),
    Data(Vec<Word>),
}


impl<A: Arch> LtlSection<A> {
    pub fn as_text(&self) -> Option<&Ltl<A>> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_text_mut(&mut self) -> Option<&mut Ltl<A>> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_data_mut(&mut self) -> Option<&mut Vec<Word>> {
        if let Self::Data(vec) = self {return Some(vec);}
        return None;
    }
}

pub struct LtlSymbolTable<A: Arch> {
    pub symbols: HashMap<String, LtlSection<A>>,
}

impl<A: Arch> LtlSymbolTable<A> {
    pub fn new(table: SymbolTable<RInstr<A::Op, A::Cond>>) -> Self {
        let mut symbols: HashMap<String, LtlSection<A>> = HashMap::new();

        for (name, section) in table.symbols {
            match section {
                Section::Data(words) =>
                    _ = symbols.insert(name, LtlSection::Data(words)),
                Section::Text(mut cfg) => {
                    use crate::rtl::regalloc::*;
                    let coloring = alloc_register::<A>(&mut cfg);
                    _ = symbols.insert(name, LtlSection::Text(Ltl::new(cfg, coloring)));
                }
            }
        }

        Self {symbols}
    }
}

impl<A: Arch> std::fmt::Display for LtlSection<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text(cfg) =>  write!(f, "{cfg}"),
            Self::Data(items) => {
                for x in items.iter() {
                    write!(f, "\n\t.word {x}")?;
                }

                write!(f, "\n")?;
                Ok(())
            },
        }
    }
}

impl<A: Arch> std::fmt::Display for LtlSymbolTable<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (symbol, section) in self.symbols.iter() {
            let is_text = matches!(section, LtlSection::Text(..));

            if is_text {
                write!(f, ".section .text\n")?;
            } else {
                write!(f, ".section .data\n")?;
            }

            write!(f, ".globl {symbol}\n{symbol}:\n{section}\n\n")?;
        }

        Ok(())
    }
}
