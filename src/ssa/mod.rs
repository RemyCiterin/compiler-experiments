pub mod into_ssa;
pub mod out_of_ssa;
pub mod dce;
pub mod gvn;
pub mod interpreter;
pub mod mem_to_reg;
pub mod simplify_ssa;
pub mod tail_call_elim;
pub mod licm;
pub mod dominance;
pub mod instcombine;
pub mod instselect;
pub mod parallel_copies;
pub mod pattern;
pub mod interference;
pub mod liveness;
pub mod alias;


use slotmap::*;
use std::collections::BTreeSet;
use std::collections::HashMap;
use crate::ast::*;

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
    fn eval(&self, args: Vec<i32>) -> Option<bool>;

    fn may_have_side_effect(&self) -> bool;
}

/// In case of a store, indicate the size of the memory chunk being stored, in this case only the
/// least significant bits of the value are stored.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum MemopKind {
    Signed8,
    Signed16,
    Unsigned8,
    Unsigned16,
    Word
}

/// RTL instructions, use architecture specific operations
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instr<Op, Cond> {
    /// A generic architecture specific operation
    Operation(Var, Op, Vec<Var>),

    /// A generic architecture specific jump condition
    Branch(Cond, Vec<Var>, Label, Label),

    /// A move instruction
    Move(Var, Lit),

    /// A jump
    Jump(Label),

    /// Load from the current stack frame
    LoadLocal{dest: Var, addr: Slot, kind: MemopKind},

    /// Load from the current stack frame
    StoreLocal{val: Var, addr: Slot, kind: MemopKind},

    /// A load instruction
    Load{dest: Var, addr: Var, volatile: bool, kind: MemopKind},

    /// A store instruction
    Store{val: Var, addr: Var, volatile: bool, kind: MemopKind},

    /// A return instruction
    Return(Var),

    /// A phy symbol
    Phi(Var, Vec<(Lit, Label)>),

    /// Call instruction
    Call(Var, String, Vec<Var>),
}


impl<Op: std::fmt::Display, Cond: std::fmt::Display> std::fmt::Display for Instr<Op, Cond> {
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
            Self::Load{dest, addr, kind, ..} =>
                write!(f, "{} := [{}] as {kind}", dest, addr),
            Self::Store{addr, val, kind, ..} =>
                write!(f, "[{}] := {} as {kind}", addr, val),
            Self::LoadLocal{dest, addr, kind} =>
                write!(f, "{} := [stack({})] as {kind}", dest, addr),
            Self::StoreLocal{addr, val, kind, ..} =>
                write!(f, "[stack({})] := {} as {kind}", addr, val),
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


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Lit {
    /// Define the address of a symbol
    Addr(String),

    /// Define an integer constant
    Int(i32),

    /// A value from a variable (a virtual register)
    Var(Var),

    /// A reference to the local stack frame
    Stack(Slot),

    /// Undefined value
    Undef,
}

impl Lit {
    pub fn as_var(&self) -> Option<Var> {
        match self {
            Lit::Var(var) => Some(*var),
            _ => None
        }
    }

    pub fn as_var_mut(&mut self) -> Option<&mut Var> {
        match self {
            Lit::Var(var) => Some(var),
            _ => None
        }
    }

    pub fn when_var(&self, f: impl FnOnce(Var) -> Var) -> Self {
        match self {
            Lit::Var(x) => Lit::Var(f(*x)),
            _ => self.clone()
        }
    }
}

impl<Op: Operation, Cond: Condition> Instr<Op, Cond> {
    pub fn labels(&self) -> Vec<Label> {
        match self {
            Self::Branch(_, _, l1, l2) => vec![*l1, *l2],
            Self::Jump(l) => vec![*l],
            _ => vec![],

        }
    }

    pub fn labels_mut(&mut self) -> Vec<&mut Label> {
        match self {
            Self::Branch(_, _, l1, l2) => vec![l1, l2],
            Self::Jump(l) => vec![l],
            _ => vec![],

        }
    }

    pub fn may_have_side_effect(&self) -> bool {
        match self {
            Self::Load{..}
                | Self::Store{..}
                | Self::LoadLocal{..}
                | Self::StoreLocal{..}
                | Self::Call(..)
                | Self::Return(..)
                | Self::Branch(..)
                | Self::Jump(..)
                => true,
            _ => false
        }
    }

    pub fn exit_block(&self) -> bool {
        match self {
            Self::Branch(..)
                | Self::Jump(..)
                | Self::Return(..)
                => true,
            _ => false
        }
    }

    pub fn destination(&self) -> Option<Var> {
        match self {
            Self::Operation(dest, _, _) => Some(*dest),
            Self::Branch(_, _, _, _) => None,
            Self::Move(dest, _) => Some(*dest),
            Self::Load{dest, ..} => Some(*dest),
            Self::Store{..} => None,
            Self::LoadLocal{dest, ..} => Some(*dest),
            Self::StoreLocal{..} => None,
            Self::Return(_) => None,
            Self::Call(dest, _, _) => Some(*dest),
            Self::Phi(dest, _) => Some(*dest),
            Self::Jump(_) => None,
        }
    }

    pub fn destination_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Operation(dest, _, _) => Some(dest),
            Self::Branch(_, _, _, _) => None,
            Self::Move(dest, _) => Some(dest),
            Self::Load{dest, ..} => Some(dest),
            Self::Store{..} => None,
            Self::LoadLocal{dest, ..} => Some(dest),
            Self::StoreLocal{..} => None,
            Self::Return(_) => None,
            Self::Call(dest, _, _) => Some(dest),
            Self::Phi(dest, _) => Some(dest),
            Self::Jump(_) => None,
        }
    }

    pub fn operands(&self) -> Vec<Var> {
        match self {
            Self::Operation(_, _, args) => args.clone(),
            Self::Branch(_, args, _, _) => args.clone(),
            Self::Move(_, Lit::Var(v)) => vec![*v],
            Self::Load{addr, ..} => vec![*addr],
            Self::Store{val, addr, ..} => vec![*addr, *val],
            Self::LoadLocal{..} => vec![],
            Self::StoreLocal{val, ..} => vec![*val],
            Self::Return(var) => vec![*var],
            Self::Call(_, _, args) => args.clone(),
            Self::Phi(_, args) =>
                args.iter().filter_map(|(lit,_)| {
                    if let Lit::Var(v) = lit { Some(*v) }
                    else { None }
                }).collect(),
            Self::Jump(_) => vec![],
            _ => vec![],
        }
    }

    pub fn operands_mut(&mut self) -> Vec<&mut Var> {
        match self {
            Self::Operation(_, _, args) => args.iter_mut().collect(),
            Self::Branch(_, args, _, _) => args.iter_mut().collect(),
            Self::Move(_, Lit::Var(v)) => vec![v],
            Self::Load{addr, ..} => vec![addr],
            Self::Store{val, addr, ..} => vec![addr, val],
            Self::LoadLocal{..} => vec![],
            Self::StoreLocal{val, ..} => vec![val],
            Self::Return(var) => vec![var],
            Self::Call(_, _, args) => args.iter_mut().collect(),
            Self::Phi(_, args) =>
                args.iter_mut().filter_map(|(lit,_)| {
                    if let Lit::Var(v) = lit { Some(v) }
                    else { None }
                }).collect(),
            Self::Jump(_) => vec![],
            _ => vec![],
        }
    }
}

new_key_type!{
    pub struct Var;
}

new_key_type!{
    pub struct Slot;
}

new_key_type!{
    pub struct Label;
}

pub type InstrId = (Label, usize);

pub struct Block<Op, Cond> {
    pub stmt: Vec<Instr<Op, Cond>>,
}

impl<Op: Operation, Cond: Condition> Block<Op, Cond> {
    pub fn succs(&self) -> Vec<Label> {
        for instr in self.stmt.iter() {
            if instr.labels().is_empty() { continue; }
            return instr.labels();
        }

        vec![]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    /// A local variable defined by a given instruction
    Local(Label, usize),

    /// A function argument passed directly
    Arg,

    /// This variable is not defined in the current cfg
    Undef,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum SlotKind {
    Local(usize),
    Incoming(usize),
    Outgoing(usize),
}

/// A control flow graph, each control flow graph is specific to one function or procedure,
/// it contains an entry point, a set of blocks, and a set of variables
pub struct Cfg<Op, Cond> {
    /// Associate for each label the definition of it's
    /// associated block
    blocks: SlotMap<Label, Block<Op, Cond>>,

    /// Entry of the control flow graph
    entry: Label,

    /// Associate a kind to each variable
    vars: SlotMap<Var, VarKind>,

    /// preds of each block
    preds: SecondaryMap<Label, BTreeSet<Label>>,

    /// If true, then the graph is expected to be in SSA form
    ssa: bool,

    /// A set of variables representing stack locations with a size and alignment constraint
    pub stack: SlotMap<Slot, SlotKind>,

    /// Arguments of the function
    pub args: Vec<Var>,
}

impl<Op, Cond> std::ops::Index<Label> for Cfg<Op, Cond> {
    type Output = Block<Op, Cond>;

    fn index(&self, name: Label) -> &Block<Op, Cond> {
        &self.blocks[name]
    }
}

impl<Op, Cond> std::ops::Index<Var> for Cfg<Op, Cond> {
    type Output = VarKind;

    fn index(&self, name: Var) -> &VarKind {
        &self.vars[name]
    }
}

impl<Op, Cond> std::ops::Index<InstrId> for Cfg<Op, Cond> {
    type Output = Instr<Op, Cond>;

    fn index(&self, (blk, pos): InstrId) -> &Instr<Op, Cond> {
        &self[blk].stmt[pos]
    }
}

impl<Op: Operation, Cond: Condition> Cfg<Op, Cond> {
    /// If ssa is set, then the SSA form will be checked during modification of the arguments
    /// and content of a block
    pub fn new(ssa: bool) -> Self {
        let mut blocks =
            SlotMap::with_key();
        let entry =
            blocks.insert(Block{stmt: vec![]});
        let mut preds = SecondaryMap::new();
        preds.insert(entry, BTreeSet::new());
        Self {
            ssa,
            entry,
            preds,
            blocks,
            args: vec![],
            vars: SlotMap::with_key(),
            stack: SlotMap::with_key(),
        }
    }

    pub fn entry(&self) -> Label {
        self.entry
    }

    pub fn preds(&self, block: Label) -> &BTreeSet<Label> {
        &self.preds[block]
    }

    fn mk_postorder(&self, block: Label, order: &mut Vec<Label>, seen: &mut BTreeSet<Label>) {
        if seen.contains(&block) { return; }
        seen.insert(block);

        for succ in self[block].succs() {
            self.mk_postorder(succ, order, seen);
        }

        order.push(block);
    }

    /// Return the preorder (also called reverse postorder) of a control flow graph,
    /// the returned order contains all the nodes of the graph, even if they are
    /// unreachable
    pub fn preorder(&self) -> Vec<Label> {
        let mut seen = BTreeSet::new();
        let mut order = vec![];

        self.mk_postorder(self.entry, &mut order, &mut seen);

        for (block, _) in self.iter_blocks() {
            self.mk_postorder(block, &mut order, &mut seen);
        }

        order.reverse();
        order
    }

    /// Return the postorder of a control flow graph
    pub fn postorder(&self) -> Vec<Label> {
        let mut order = self.preorder();
        order.reverse();
        order
    }

    pub fn iter_blocks(&self) -> slotmap::basic::Iter<'_, Label, Block<Op, Cond>> {
        self.blocks.iter()
    }

    pub fn iter_vars(&self) -> slotmap::basic::Iter<'_, Var, VarKind> {
        self.vars.iter()
    }

    /// Return the predecessor of an instruction in it's block
    pub fn prev_instr(&self, (blk, pos): InstrId) -> Option<InstrId> {

        if pos != 0 { Some((blk, pos-1)) }
        else { None }
    }

    /// Return the successor of an instruction in it's block
    pub fn next_instr(&self, (blk, pos): InstrId) -> Option<InstrId> {

        if pos + 1 < self[blk].stmt.len() { Some((blk, pos+1)) }
        else { None }
    }

    /// Return the label of an instruction
    pub fn label_instr(&self, (blk, _): InstrId) -> Label {
        blk
    }

    /// Return the list of labels in a control flow graph
    pub fn labels(&self) -> Vec<Label> {
        self.iter_blocks().map(|b| b.0).collect()
    }

    /// Return a fresh local variable
    pub fn fresh_var(&mut self) -> Var {
        self.vars.insert(VarKind::Undef)
    }

    /// Return a fresh stack slot, and allocate it on the stack
    pub fn fresh_stack_var(&mut self, size: usize) -> Slot {
        self.stack.insert(SlotKind::Local(size))
    }

    /// Generate a fresh outgoing stack slot
    pub fn fresh_outgoing_var(&mut self, num: usize) -> Slot {
        self.stack.insert(SlotKind::Outgoing(num))
    }

    /// Generate a fresh incoming stack slot
    pub fn fresh_incoming_var(&mut self, num: usize) -> Slot {
        self.stack.insert(SlotKind::Incoming(num))
    }

    /// Generate a fresh function argument
    pub fn fresh_arg(&mut self) -> Var {
        let var = self.vars.insert(VarKind::Arg);
        self.args.push(var);
        var
    }

    pub fn end_ssa(&mut self) {
        self.ssa = false;
    }

    pub fn start_ssa(&mut self) {
        let mut vars = std::mem::take(&mut self.vars);
        assert!(!self.ssa);

        for (_, kind) in vars.iter_mut() {
            *kind = VarKind::Undef;
        }

        for (label, block) in self.blocks.iter() {
            for (id, instr) in block.stmt.iter().enumerate() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Undef);
                    vars[x] = VarKind::Local(label, id);
                }
            }
        }

        for &x in self.args.iter() {
            assert!(vars[x] == VarKind::Undef);
            vars[x] = VarKind::Arg;
        }

        self.vars = vars;
        self.ssa = true;
    }

    /// Generate a fresh block and return it's associated label
    pub fn fresh_label(&mut self) -> Label {
        let new =
            self.blocks.insert(Block{stmt: vec![]});
        self.preds.insert(new, BTreeSet::new());
        new
    }

    /// Garbadge collect a variable from the control flow graph
    pub fn remove_var(&mut self, var: Var) {
        assert!(self.vars[var] == VarKind::Undef);
        self.vars.remove(var);
    }

    /// Garbadge collect a stack slot
    pub fn remove_slot(&mut self, slot: Slot) {
        self.stack.remove(slot);
    }

    pub fn remove_block(&mut self, block: Label) {
        self.set_block_stmt(block, vec![]);
        self.blocks.remove(block);
        self.preds.remove(block);
    }

    /// GC all the unreachable blocks, variable from the control flow graph
    pub fn gc(&mut self) {
        let mut seen = BTreeSet::new();

        self.mk_postorder(self.entry, &mut vec![], &mut seen);

        let blocks: Vec<Label> = self.blocks.iter().map(|(b,_)| b).collect();

        for block in blocks {
            if !seen.contains(&block) {
                self.set_block_stmt(block, vec![]);
                self.blocks.remove(block);
            }
        }

        let vars: Vec<Var> = self.iter_vars().map(|(v,_)| v).collect();

        for var in vars {
            if self.vars[var] == VarKind::Undef {
                self.remove_var(var);
            }
        }
    }

    fn clear_preds(&mut self, block: Label) {
        for succ in self[block].succs() {
            if self.preds.contains_key(succ) {
                self.preds[succ].remove(&block);
            }
        }
    }

    fn set_preds(&mut self, block: Label) {
        for succ in self[block].succs() {
            self.preds[succ].insert(block);
        }
    }

    /// Update the body of a block
    pub fn set_block_stmt(&mut self, block: Label, stmt: Vec<Instr<Op, Cond>>) {
        if self.ssa {
            let mut vars = std::mem::take(&mut self.vars);

            for instr in self[block].stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(matches!(vars[x], VarKind::Local(..)));
                    vars[x] = VarKind::Undef;
                }
            }

            for (pos, instr) in stmt.iter().enumerate() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Undef);
                    vars[x] = VarKind::Local(block, pos);
                }
            }

            self.vars = vars;
        }

        // A non-empty block must exit at (and only at) it's last instruction
        for (i, instr) in stmt.iter().enumerate() {
            let last: bool = i == stmt.len() - 1;
            assert!(last == instr.exit_block());
        }


        self.clear_preds(block);
        self.blocks[block].stmt = stmt;
        self.set_preds(block);
    }

    pub fn remove_instr(&mut self, blk: Label, pos: usize) {
        if self.ssa {
            if let Some(x) = self[blk].stmt[pos].destination() {
                assert!(matches!(self.vars[x], VarKind::Local(_, _)));
                self.vars[x] = VarKind::Undef;
            }

            for (instr, pos) in self.blocks[blk].stmt[pos..].iter().zip(pos..) {
                if let Some(dest) = instr.destination() {
                    self.vars[dest] = VarKind::Local(blk, pos-1);
                }
            }
        }

        self.clear_preds(blk);
        self.blocks[blk].stmt.remove(pos);
        self.set_preds(blk);
    }

    pub fn set_instr(&mut self, blk: Label, pos: usize, instr: Instr<Op, Cond>) {
        if self.ssa {
            if let Some(x) = self[blk].stmt[pos].destination() {
                assert!(matches!(self.vars[x], VarKind::Local(..)));
                self.vars[x] = VarKind::Undef;
            }

            if let Some(x) = instr.destination() {
                assert!(self.vars[x] == VarKind::Undef);
                self.vars[x] = VarKind::Local(blk, pos);
            }
        }

        self.clear_preds(blk);
        self.blocks[blk].stmt[pos] = instr;
        self.set_preds(blk);
    }

    pub fn insert_instr(&mut self, blk: Label, pos: usize, instr: Instr<Op, Cond>) {
        if self.ssa {
            if let Some(x) = instr.destination() {
                assert!(self.vars[x] == VarKind::Undef);
                self.vars[x] = VarKind::Local(blk, pos);
            }

            for (instr, pos) in self.blocks[blk].stmt[pos..].iter().zip(pos..) {
                if let Some(dest) = instr.destination() {
                    self.vars[dest] = VarKind::Local(blk, pos+1);
                }
            }
        }

        for succ in instr.labels() {
            self.preds[succ].insert(blk);
        }

        self.blocks[blk].stmt.insert(pos, instr);
    }

    pub fn ssa(&self) -> bool { self.ssa }

    /// Return the list of instructions that use each variables
    pub fn ssa_graph(&self) -> SparseSecondaryMap<Var, BTreeSet<InstrId>> {
        let mut graph = SparseSecondaryMap::new();

        for (var, _) in self.iter_vars() {
            graph.insert(var, BTreeSet::new());
        }

        for (label, block) in self.iter_blocks() {
            for (pos, instr) in block.stmt.iter().enumerate() {
                for op in instr.operands() {
                    graph[op].insert((label, pos));
                }
            }
        }

        graph
    }
}

#[derive(Clone)]
pub enum Word {
    Addr(String, i32),
    Int(i32),
}

pub enum Section<Op, Cond> {
    Text(Cfg<Op, Cond>),
    Data(Vec<Word>),
    Bss(usize),
}

impl<Op: Operation, Cond: Condition> Section<Op, Cond> {
    pub fn as_text(&self) -> Option<&Cfg<Op, Cond>> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_text_mut(&mut self) -> Option<&mut Cfg<Op, Cond>> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_data_mut(&mut self) -> Option<&mut Vec<Word>> {
        if let Self::Data(vec) = self {return Some(vec);}
        return None;
    }
}

pub struct SymbolTable<Op, Cond> {
    pub symbols: HashMap<String, Section<Op, Cond>>,
}

impl<Op: Operation, Cond: Condition> SymbolTable<Op, Cond> {
    pub fn pp_text(&self) {
        for (symbol, section) in self.symbols.iter() {
            if matches!(section, Section::Text(..)) {
                print!(".globl {symbol}:\n{section}\n\n");
            }
        }
    }
}

impl std::fmt::Display for MemopKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Signed8 => write!(f, "u8"),
            Self::Signed16 => write!(f, "u16"),
            Self::Unsigned16 => write!(f, "i16"),
            Self::Unsigned8 => write!(f, "u8"),
            Self::Word => write!(f, "i32"),
        }
    }
}

impl std::fmt::Display for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Addr(s, offset) =>  write!(f, "{s}+{offset}"),
            Self::Int(i) => write!(f, "{i}"),
        }
    }
}

impl<Op: Operation, Cond: Condition> std::fmt::Display for Section<Op, Cond> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bss(size) => write!(f, ".zero {size}"),
            Self::Text(cfg) =>  write!(f, "{cfg}"),
            Self::Data(items) => {
                for x in items.iter() {
                    write!(f, "\n\t{x}")?;
                }

                write!(f, "\n")?;
                Ok(())
            },
        }
    }
}

impl<Op: Operation, Cond: Condition> std::fmt::Display for SymbolTable<Op, Cond> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (symbol, section) in self.symbols.iter() {
            write!(f, ".globl {symbol}:\n{section}\n\n")?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "l{}", lsb)
        } else {
            write!(f, "l{}_{}", lsb, msb)
        }
    }
}

impl std::fmt::Display for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "s{}", lsb)
        } else {
            write!(f, "s{}_{}", lsb, msb)
        }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lsb = self.data().as_ffi() & 0xffff_ffff;
        let msb = self.data().as_ffi() >> 32;

        if msb == 1 {
            write!(f, "v{}", lsb)
        } else {
            write!(f, "v{}_{}", lsb, msb)
        }
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Addr(s) => write!(f, "{}", s),
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Var(v) => write!(f, "{}", v),
            Lit::Stack(off) => write!(f, "stack({off})"),
            Lit::Undef => write!(f, "undef"),
        }
    }
}
impl<Op: Operation, Cond: Condition> std::fmt::Display for Cfg<Op, Cond> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "entry: {} args:", self.entry())?;
        for arg in self.args.iter() {
            write!(f, " {}", arg)?;
        }

        write!(f, "\nstack:")?;
        for (slot, kind) in self.stack.iter() {
            write!(f, " [{slot}; {:?}]", kind)?;
        }

        write!(f, "\n")?;

        for (name, block) in self.iter_blocks() {
            // Use an empty line between each block
            write!(f, "\n{}:", name)?;

            for instr in block.stmt.iter() {
                write!(f, "\n\t{}", instr)?;
            }

            write!(f, "\n")?;
        }

        Ok(())
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum COp {
    And,
    Or,
    Xor,
    Add,
    Sub,
    Sll,
    Sra,
    Srl,
    PtrAdd,
    Equal,
    NotEqual,
    LessThan,
    ULessThan,
    LessEqual,
    ULessEqual,
    Mul,
    UDiv,
    URem,
    SDiv,
    SRem,
    Not,
    Neg,
}

impl COp {
    pub fn from_binop(binop: Binop) -> Self {
        match binop {
            Binop::PtrAdd => Self::PtrAdd,
            Binop::And => Self::And,
            Binop::Add => Self::Add,
            Binop::Or => Self::Or,
            Binop::Xor => Self::Xor,
            Binop::Sub => Self::Sub,
            Binop::Sll => Self::Sll,
            Binop::Sra => Self::Sra,
            Binop::Srl => Self::Srl,
            Binop::Equal => Self::Equal,
            Binop::NotEqual => Self::NotEqual,
            Binop::LessThan => Self::LessThan,
            Binop::ULessThan => Self::ULessThan,
            Binop::LessEqual => Self::LessEqual,
            Binop::ULessEqual => Self::ULessEqual,
            Binop::Mul => Self::Mul,
            Binop::SDiv => Self::SDiv,
            Binop::UDiv => Self::UDiv,
            Binop::SRem => Self::SRem,
            Binop::URem => Self::URem,
        }
    }

    pub fn from_unop(unop: Unop) -> Self {
        match unop {
            Unop::Neg => Self::Neg,
            Unop::Not => Self::Not
        }
    }
}

impl std::fmt::Display for COp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Operation for COp {
    fn arity(&self) -> usize {
        match self {
            Self::Neg => 1,
            Self::Not => 1,
            _ => 2
        }
    }

    fn may_have_side_effect(&self) -> bool {false}

    fn eval(&self, args: Vec<i32>) -> Option<i32> {
        Some(match self {
            Self::Neg => -args[0],
            Self::Not => !args[0],
            Self::PtrAdd => args[0] + args[1],
            Self::And => args[0] & args[1],
            Self::Or => args[0] | args[1],
            Self::Xor => args[0] ^ args[1],
            Self::Add => args[0].wrapping_add(args[1]),
            Self::Sub => args[0].wrapping_sub(args[1]),
            Self::Sll => sll(args[0], args[1]),
            Self::Sra => args[0].wrapping_shr(args[1].cast_unsigned()),
            Self::Srl => srl(args[0], args[1]),
            Self::Equal => (args[0] == args[1]) as i32,
            Self::NotEqual => (args[0] != args[1]) as i32,
            Self::LessThan => (args[0] < args[1]) as i32,
            Self::LessEqual => (args[0] <= args[1]) as i32,
            Self::ULessThan => (args[0].cast_unsigned() < args[1].cast_unsigned()) as i32,
            Self::ULessEqual => (args[0].cast_unsigned() <= args[1].cast_unsigned()) as i32,
            Self::Mul => args[0].wrapping_mul(args[1]),
            Self::SDiv => args[0].wrapping_div(args[1]),
            Self::SRem => args[0].wrapping_rem(args[1]),
            Self::UDiv => (args[0].cast_unsigned() / args[1].cast_unsigned()) as i32,
            Self::URem => (args[0].cast_unsigned() % args[1].cast_unsigned()) as i32,
        })
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum CCond { Nez }

impl std::fmt::Display for CCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Nez")
    }
}


impl Condition for CCond {
    fn arity(&self) -> usize {1}

    fn may_have_side_effect(&self) -> bool {false}

    fn eval(&self, args: Vec<i32>) -> Option<bool> {
        Some(args[0] != 0)
    }
}
