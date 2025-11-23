//! This module take a LTL representation of a program and perform sofware pipelining on each block
//! to build bundles for a VLIW architecture.

use crate::arch::*;
use crate::ssa::*;
use slotmap::*;

use crate::ltl::*;

#[derive(Clone)]
pub struct Bundle<Op, Cond>(pub Vec<LInstr<Op, Cond>>);

pub struct Btl<A: Arch> {
    pub blocks: Vec<Vec<Bundle<A::Op, A::Cond>>>,
    pub stack: SlotMap<Slot, SlotKind>,
}

impl<A: Arch> Btl<A> {
    pub fn new(ltl: Ltl<A>) -> Self {
        let mut blocks = vec![];
        let stack = ltl.stack;

        for block in ltl.blocks {
            let mut stmt: Vec<Bundle<A::Op, A::Cond>> = vec![];

            let mut avail = vec![];
            for _ in block.iter() {
                avail.push(true);
            }

            while block.len() > 0 {
                if avail.iter().all(|b| !b) { break; }

                let mut bundle: Vec<LInstr<A::Op, A::Cond>> = vec![];
                let mut written = PhysSet::empty();
                let mut read = PhysSet::empty();

                let mut num_op: usize = 0;
                let mut num_mem: usize = 0;

                let mut store_effect: bool = false;
                let mut load_effect: bool = false;
                let mut all_instr: bool = true;

                for index in 0..block.len() {
                    if !avail[index] { continue; }
                    let instr: &LInstr<A::Op, A::Cond> = &block[index];
                    let mut used: bool;

                    match instr {
                        LInstr::Move(..) => used = num_op < 4,
                        LInstr::Operation(..) => used = num_op < 4,
                        LInstr::Li(..) | LInstr::Ls(..) | LInstr::La(..) => used = num_op < 4,
                        LInstr::Call(..) =>
                            used = all_instr,
                        LInstr::Jcc(..) =>
                            used = all_instr,
                        LInstr::Jump(..) =>
                            used = all_instr,
                        LInstr::Return =>
                            used = all_instr,

                        LInstr::LoadLocal{..} | LInstr::Load{..} =>
                            used = num_mem == 0 && !store_effect,
                        LInstr::StoreLocal{..} | LInstr::Store{..} =>
                            used = num_mem == 0 && !load_effect,
                    }

                    if let Some(dest) = instr.destination() && read.contains(dest) {
                        used = false;
                    }

                    if let Some(dest) = instr.destination() && written.contains(dest) {
                        used = false;
                    }

                    for x in instr.operands() {
                        if written.contains(x) { used = false; }
                    }

                    if used {
                        bundle.push(instr.clone());
                        avail[index] = false;
                    }

                    if !used { all_instr = false; }

                    if let Some(dest) = instr.destination() {
                        written.insert(dest);
                    }

                    for x in instr.operands() {
                        if !used { read.insert(x); }
                    }

                    match instr {
                        LInstr::Operation(..) => {
                            num_op += 1;
                        }
                        LInstr::Move(..) => {
                            num_op += 1;
                        }
                        LInstr::Li(..)
                            | LInstr::La(..)
                            | LInstr::Ls(..) => {}
                        LInstr::LoadLocal{..}
                            | LInstr::Load{..} => {
                            if !used { load_effect = true; }
                            if used { num_mem += 1; }
                        }
                        LInstr::StoreLocal{..}
                            | LInstr::Store{..} => {
                            if used { num_mem += 1; }
                            store_effect = true;
                        }
                        LInstr::Jcc(..) => break,
                        LInstr::Return => break,
                        LInstr::Jump(_) => break,
                        LInstr::Call(..) => break,
                    }
                }


                stmt.push(Bundle(bundle));
            }

            blocks.push(stmt);
        }

        Self{blocks, stack}
    }

    pub fn contains_call(&self) -> bool {
        for block in self.blocks.iter() {
            for bundle in block.iter() {
                for instr in bundle.0.iter() {
                    if matches!(instr, LInstr::Call(..)) { return true; }
                }
            }
        }

        return false;
    }

    pub fn pp(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (push, pop, slots) =
            A::gen_layout(&self.stack, self.contains_call());

        write!(f, "\t{push}\n")?;

        for (i, block) in self.blocks.iter().enumerate() {
            let from_label = |j: usize| {
                if j > i {
                    format!("{j}f")
                } else {
                    format!("{j}b")
                }
            };

            write!(f, "{}:\n", i)?;

            for bundle in block.iter() {
                for idx in 0..bundle.0.len() {
                    let instr = bundle.0[idx].clone();

                    if matches!(instr, LInstr::Return) {
                        write!(f, "\t{pop}\n")?;
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
                            _ = A::pp_from_stack(f, dest, slots[src])?,
                        LInstr::La(dest, src) =>
                            _ = A::pp_from_addr(f, dest, &src)?,
                        LInstr::Call(name) =>
                            _ = A::pp_call(f, &name)?,
                        LInstr::Return =>
                            _ = A::pp_return(f)?,
                        LInstr::Load{addr, dest, kind} =>
                            _ = A::pp_load(f, dest, addr, kind)?,
                        LInstr::Store{addr, val, kind} =>
                            _ = A::pp_store(f, addr, val, kind)?,
                        LInstr::LoadLocal{addr, dest, kind} =>
                            _ = A::pp_load_local(f, dest, slots[addr], kind)?,
                        LInstr::StoreLocal{addr, val, kind} =>
                            _ = A::pp_store_local(f, slots[addr], val, kind)?,
                    }

                    if idx == bundle.0.len() - 1 { write!(f, ";;")?; }
                    else if matches!(bundle.0[idx+1], LInstr::Return) { write!(f, ";;")?; }
                    write!(f, "\n")?;
                }
            }
        }

        Ok(())

    }
}

impl<A: Arch> std::fmt::Display for Btl<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pp(f)
    }
}

pub enum BtlSection<A: Arch> {
    Text(Btl<A>),
    Data(Vec<Word>),
    Bss(usize),
}


impl<A: Arch> BtlSection<A> {
    pub fn as_text(&self) -> Option<&Btl<A>> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_text_mut(&mut self) -> Option<&mut Btl<A>> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_data_mut(&mut self) -> Option<&mut Vec<Word>> {
        if let Self::Data(vec) = self {return Some(vec);}
        return None;
    }
}

pub struct BtlSymbolTable<A: Arch> {
    pub symbols: HashMap<String, BtlSection<A>>,
}

impl<A: Arch> BtlSymbolTable<A> {
    pub fn new(table: LtlSymbolTable<A>) -> Self {
        let mut symbols: HashMap<String, BtlSection<A>> = HashMap::new();

        for (name, section) in table.symbols {
            match section {
                LtlSection::Bss(size) =>
                    _ = symbols.insert(name, BtlSection::Bss(size)),
                LtlSection::Data(words) =>
                    _ = symbols.insert(name, BtlSection::Data(words)),
                LtlSection::Text(cfg) => {
                    _ = symbols.insert(name, BtlSection::Text(Btl::new(cfg)));
                }
            }
        }

        Self {symbols}
    }
}

impl<A: Arch> std::fmt::Display for BtlSection<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bss(size) => write!(f, ".zero {size}"),
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

impl<A: Arch> std::fmt::Display for BtlSymbolTable<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (symbol, section) in self.symbols.iter() {
            let is_text = matches!(section, BtlSection::Text(..));

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
