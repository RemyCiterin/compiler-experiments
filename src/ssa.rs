use downcast_rs::*;

use slotmap::*;
use std::collections::BTreeSet;
use std::collections::HashMap;
use crate::ast::*;

pub trait Instruction : Downcast + std::fmt::Display + std::fmt::Debug {
    fn operands(&self) -> Vec<Var>;
    fn operands_mut(&mut self) -> Vec<&mut Var>;
    fn destination(&self) -> Option<Var>;
    fn destination_mut(&mut self) -> Option<&mut Var>;
    fn labels(&self) -> Vec<Label>;
    fn labels_mut(&mut self) -> Vec<&mut Label>;
    fn exit_block(&self) -> bool;
    fn may_have_side_effect(&self) -> bool;

    fn clone_dyn(&self) -> Box<dyn Instruction>;
}

impl_downcast!(Instruction);

pub type Instr = Box<dyn Instruction>;

pub fn mk_instr<I: Instruction>(i: I) -> Instr {
    Box::new(i)
}

impl Clone for Instr {
    fn clone(&self) -> Self {
        self.clone_dyn()
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum GInstr {
    /// Binary operations
    Binop(Var, Binop, Lit, Lit),

    /// Unary operations
    Unop(Var, Unop, Lit),

    /// Return a value from a function call
    Return(Lit),

    /// Move a value from one position to another
    Move(Var, Lit),

    /// Load a value from a pointer
    Load{dest: Var, addr: Lit, volatile: bool},

    /// Store a value from a pointer
    Store{val: Lit, addr: Lit, volatile: bool},

    /// Call a function with a given number of arguments
    Call(Var, String, Vec<Lit>),

    /// Jump to a label
    Jump(Label),

    /// Branch to a lable depending of a value
    Branch(Lit, Label, Label),
}



impl GInstr {
    /// Return the list of operands the instruction depends on,
    /// including all the operands in the label arguments
    pub fn literals(&self) -> Vec<Lit> {
        match self {
            Self::Binop(_, _, x, y)
                | Self::Store{addr: x, val: y, ..}
                => vec![x.clone(), y.clone()],
            Self::Unop(_, _, x)
                | Self::Move(_, x)
                | Self::Load{addr: x, ..}
                | Self::Branch(x, _, _)
                | Self::Return(x)
                => vec![x.clone()],
            Self::Jump(_) => vec![],
            Self::Call(_, _, args) =>
                args.iter().cloned().collect(),
        }
    }

    /// Return the list of operands the instruction depends on,
    /// including all the operands in the label arguments
    pub fn literals_mut(&mut self) -> Vec<&mut Lit> {
        match self {
            Self::Binop(_, _, x, y)
                | Self::Store{addr: x, val: y, ..}
                => vec![x, y],
            Self::Unop(_, _, x)
                | Self::Move(_, x)
                | Self::Load{addr: x, ..}
                | Self::Branch(x, _, _)
                | Self::Return(x)
                => vec![x],
            Self::Jump(_) => vec![],
            Self::Call(_, _, args) =>
                args.iter_mut().collect(),
        }
    }
}


impl Instruction for GInstr {
    fn clone_dyn(&self) -> Instr {
        Box::new(self.clone())
    }

    fn exit_block(&self) -> bool {
        match self {
            Self::Jump(..)
                | Self::Branch(..)
                | Self::Return(..)
                => true,
            _ => false,
        }
    }

    fn destination(&self) -> Option<Var> {
        match self {
            Self::Binop(ret, ..)
                | Self::Unop(ret, ..)
                | Self::Move(ret, ..)
                | Self::Load{dest:ret, ..}
                | Self::Call(ret, ..)
                => Some(*ret),
            _ => None
        }
    }

    fn destination_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Binop(ret, ..)
                | Self::Unop(ret, ..)
                | Self::Move(ret, ..)
                | Self::Load{dest: ret, ..}
                | Self::Call(ret, ..)
                => Some(ret),
            _ => None
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
            _ => false,
        }
    }

    fn labels(&self) -> Vec<Label> {
        match self {
            Self::Jump(label) => vec![*label],
            Self::Branch(_, l1, l2) => vec![*l1, *l2],
            _ => vec![]
        }
    }

    fn labels_mut(&mut self) -> Vec<&mut Label> {
        match self {
            Self::Jump(label) => vec![label],
            Self::Branch(_, l1, l2) => vec![l1, l2],
            _ => vec![]
        }
    }

    fn operands(&self) -> Vec<Var> {
        self.literals().into_iter().filter_map(|v| v.as_var()).collect()
    }

    fn operands_mut(&mut self) -> Vec<&mut Var> {
        self.literals_mut().into_iter().filter_map(|v| v.as_var_mut()).collect()
    }
}


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Phi{pub dest: Var, pub args: Vec<(Lit, Label)>}

impl Phi {
    pub fn literals(&self) -> Vec<Lit> {
        self.args.iter().map(|(v,_)| v.clone()).collect()
    }

    pub fn literals_mut(&mut self) -> Vec<&mut Lit> {
        self.args.iter_mut().map(|(v,_)| v).collect()
    }
}

impl Instruction for Phi {
    fn destination(&self) -> Option<Var> { Some(self.dest) }

    fn destination_mut(&mut self) -> Option<&mut Var> { Some(&mut self.dest) }

    fn operands(&self) -> Vec<Var> {
        self.literals().into_iter().filter_map(|v| v.as_var()).collect()
    }

    fn operands_mut(&mut self) -> Vec<&mut Var> {
        self.literals_mut().into_iter().filter_map(|v| v.as_var_mut()).collect()
    }

    fn labels(&self) -> Vec<Label> { vec![] }

    fn labels_mut(&mut self) -> Vec<&mut Label> { vec![] }

    fn exit_block(&self) -> bool { false }

    fn may_have_side_effect(&self) -> bool { false }

    fn clone_dyn(&self) -> Instr {
        Box::new(self.clone())
    }
}


new_key_type!{
    pub struct Var;
}

new_key_type!{
    pub struct Label;
}

new_key_type!{
    pub struct InstrId;
}


pub struct Block {
    pub stmt: Vec<Instr>,
    pub ids: Vec<InstrId>,
}

impl Block {
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
    Local(InstrId),

    /// A stack variable defined at the function entry point
    Stack,

    /// A function argument passed directly
    Arg,

    /// This variable is not defined in the current cfg
    Undef,
}

/// A control flow graph, each control flow graph is specific to one function or procedure,
/// it contains an entry point, a set of blocks, and a set of variables
pub struct Cfg {
    /// Associate for each label the definition of it's
    /// associated block
    blocks: SlotMap<Label, Block>,

    /// Entry of the control flow graph
    entry: Label,

    /// Associate a kind to each variable
    vars: SlotMap<Var, VarKind>,

    /// preds of each block
    preds: SecondaryMap<Label, BTreeSet<Label>>,

    /// If true, then the graph is expected to be in SSA form
    ssa: bool,

    /// List all the instructions in a program, for each ID it contains
    /// it's instruction, the label of it's block, and it's previous/next instruction in the block
    instructions: SlotMap<InstrId, (Label, Instr, Option<InstrId>, Option<InstrId>)>,

    /// A set of variables representing stack locations with a size and alignment constraint
    pub stack: Vec<(Var, usize)>,

    /// Arguments of the function
    pub args: Vec<Var>,
}

impl std::ops::Index<Label> for Cfg {
    type Output = Block;

    fn index(&self, name: Label) -> &Block {
        &self.blocks[name]
    }
}

impl std::ops::Index<Var> for Cfg {
    type Output = VarKind;

    fn index(&self, name: Var) -> &VarKind {
        &self.vars[name]
    }
}

impl std::ops::Index<InstrId> for Cfg {
    type Output = Instr;

    fn index(&self, name: InstrId) -> &Instr {
        &self.instructions[name].1
    }
}

impl Cfg {
    /// If ssa is set, then the SSA form will be checked during modification of the arguments
    /// and content of a block
    pub fn new(ssa: bool, args: Vec<Var>) -> Self {
        let mut blocks =
            SlotMap::with_key();
        let entry =
            blocks.insert(Block{stmt: vec![], ids: vec![]});
        let mut preds = SecondaryMap::new();
        preds.insert(entry, BTreeSet::new());
        Self {
            ssa,
            entry,
            args,
            preds,
            blocks,
            stack: Vec::new(),
            vars: SlotMap::with_key(),
            instructions: SlotMap::with_key(),
        }
    }

    pub fn entry(&self) -> Label {
        self.entry
    }

    pub fn preds(&self, block: Label) -> &BTreeSet<Label> {
        &self.preds[block]
    }

    fn mk_preorder(&self, block: Label, order: &mut Vec<Label>, seen: &mut BTreeSet<Label>) {
        if seen.contains(&block) { return; }
        seen.insert(block);
        order.push(block);

        for succ in self[block].succs() {
            self.mk_preorder(succ, order, seen);
        }
    }

    /// Return the preorder (also called reverse postorder) of a control flow graph,
    /// the returned order contains all the nodes of the graph, even if they are
    /// unreachable
    pub fn preorder(&self) -> Vec<Label> {
        let mut seen = BTreeSet::new();
        let mut order = vec![];

        self.mk_preorder(self.entry, &mut order, &mut seen);

        for (block, _) in self.iter_blocks() {
            self.mk_preorder(block, &mut order, &mut seen);
        }

        order
    }

    /// Return the postorder of a control flow graph
    pub fn postorder(&self) -> Vec<Label> {
        let mut order = self.preorder();
        order.reverse();
        order
    }

    pub fn iter_blocks(&self) -> slotmap::basic::Iter<'_, Label, Block> {
        self.blocks.iter()
    }

    pub fn iter_vars(&self) -> slotmap::basic::Iter<'_, Var, VarKind> {
        self.vars.iter()
    }

    pub fn iter_instr(&self)
        -> slotmap::basic::Iter<'_, InstrId, (Label, Instr, Option<InstrId>, Option<InstrId>)> {
        self.instructions.iter()
    }

    /// Return the predecessor of an instruction in it's block
    pub fn prev_instr(&self, instr: InstrId) -> Option<InstrId> {
        self.instructions[instr].2
    }

    /// Return the successor of an instruction in it's block
    pub fn next_instr(&self, instr: InstrId) -> Option<InstrId> {
        self.instructions[instr].3
    }

    /// Return the label of an instruction
    pub fn label_instr(&self, instr: InstrId) -> Label {
        self.instructions[instr].0
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
    pub fn fresh_stack_var(&mut self, size: usize) -> Var {
        let var = self.vars.insert(VarKind::Stack);
        self.stack.push((var, size));
        var
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

        for (_, block) in self.blocks.iter() {
            for (instr, id) in block.stmt.iter().zip(block.ids.iter()) {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Undef);
                    vars[x] = VarKind::Local(*id);
                }
            }
        }

        for &x in self.args.iter() {
            assert!(vars[x] == VarKind::Undef);
            vars[x] = VarKind::Arg;
        }

        for (x, _) in self.stack.iter() {
            assert!(vars[*x] == VarKind::Undef);
            vars[*x] = VarKind::Stack;
        }

        self.vars = vars;
        self.ssa = true;
    }

    /// Generate a fresh block and return it's associated label
    pub fn fresh_label(&mut self) -> Label {
        let new =
            self.blocks.insert(Block{stmt: vec![], ids: vec![]});
        self.preds.insert(new, BTreeSet::new());
        new
    }

    /// Garbadge collect a variable from the control flow graph
    pub fn remove_var(&mut self, var: Var) {
        assert!(self.vars[var] == VarKind::Undef);
        self.vars.remove(var);
    }

    pub fn remove_block(&mut self, block: Label) {
        self.set_block_stmt(block, vec![]);
        self.blocks.remove(block);
        self.preds.remove(block);
    }

    /// GC all the unreachable blocks, variable from the control flow graph
    pub fn gc(&mut self) {
        let mut seen = BTreeSet::new();

        self.mk_preorder(self.entry, &mut vec![], &mut seen);

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

    /// Update the body of a block
    pub fn set_block_stmt(&mut self, block: Label, stmt: Vec<Instr>) {
        // Free all the instructions in the block
        for i in std::mem::take(&mut self.blocks[block].ids) {
            self.instructions.remove(i);
        }

        // Insert new instructions for the block
        let instrs: Vec<InstrId> =
            stmt.iter()
            .map(|i| {
                self.instructions.insert((block, i.clone(), None, None))
            }).collect();

        // Compute the predecessors, successors of the added instructions
        for i in 0..stmt.len() {
            let prev = if i != 0 {Some(instrs[i-1])} else {None};
            let next = if i != stmt.len() - 1 {Some(instrs[i+1])} else {None};
            self.instructions[instrs[i]].2 = prev;
            self.instructions[instrs[i]].3 = next;
        }

        if self.ssa {
            let mut vars = std::mem::take(&mut self.vars);

            for instr in self[block].stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(matches!(vars[x], VarKind::Local(..)));
                    vars[x] = VarKind::Undef;
                }
            }

            for (instr, id) in stmt.iter().zip(instrs.iter()) {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Undef);
                    vars[x] = VarKind::Local(*id);
                }
            }

            self.vars = vars;
        }

        // A non-empty block must exit at (and only at) it's last instruction
        for (i, instr) in stmt.iter().enumerate() {
            let last: bool = i == stmt.len() - 1;
            assert!(last == instr.exit_block());
        }

        // Remove the block from the predecessors of it's old successors
        for succ in self[block].succs() {
            if self.preds.contains_key(succ) {
                self.preds[succ].remove(&block);
            }
        }

        self.blocks[block].stmt = stmt;
        self.blocks[block].ids = instrs;

        // Add the block as predecessor of it successors
        for succ in self[block].succs() {
            self.preds[succ].insert(block);
        }
    }

    pub fn ssa(&self) -> bool { self.ssa }

    /// Return the list of instructions that use each variables
    pub fn ssa_graph(&self) -> SparseSecondaryMap<Var, BTreeSet<InstrId>> {
        let mut graph = SparseSecondaryMap::new();

        for (var, _) in self.iter_vars() {
            graph.insert(var, BTreeSet::new());
        }

        for (_, block) in self.iter_blocks() {
            for (instr, id) in block.stmt.iter().zip(block.ids.iter().cloned()) {
                for op in instr.operands() {
                    graph[op].insert(id);
                }
            }
        }

        graph
    }
}

pub enum Word {
    Addr(String, i32),
    Int(i32),
}

pub enum Section {
    Text(Cfg),
    Data(Vec<Word>),
}

impl Section {
    pub fn as_text(&self) -> Option<&Cfg> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_text_mut(&mut self) -> Option<&mut Cfg> {
        if let Self::Text(cfg) = self {return Some(cfg);}
        return None;
    }

    pub fn as_data_mut(&mut self) -> Option<&mut Vec<Word>> {
        if let Self::Data(vec) = self {return Some(vec);}
        return None;
    }
}

pub struct SymbolTable {
    pub symbols: HashMap<String, Section>,
}

impl std::fmt::Display for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Addr(s, offset) =>  write!(f, "&{s}+{offset}"),
            Self::Int(i) => write!(f, "{i}"),
        }
    }
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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

impl std::fmt::Display for SymbolTable {
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
        }
    }
}

impl std::fmt::Display for Phi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := phi", self.dest)?;
        for (var, label) in self.args.iter() {
            write!(f, " ({}, {})", var, label)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for GInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binop(dest, op, src1, src2) =>
                write!(f, "{} := {} {} {}", dest, src1, op, src2),
            Self::Unop(dest, op, src1) =>
                write!(f, "{} := {} {}", dest, op, src1),
            Self::Load{dest, addr, volatile: true} =>
                write!(f, "{} := [volatile {}]", dest, addr),
            Self::Load{dest, addr, volatile: false} =>
                write!(f, "{} := [{}]", dest, addr),
            Self::Store{addr, val, ..} =>
                write!(f, "[{}] := {}", addr, val),
            Self::Move(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Self::Branch(cond, l1, l2) =>
                write!(f, "branch {}, {}, {}", cond, l1, l2),
            Self::Jump(l) =>
                write!(f, "jump {}", l),
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
        }
    }
}

impl std::fmt::Display for Cfg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "entry: {} args:", self.entry())?;
        for arg in self.args.iter() {
            write!(f, " {}", arg)?;
        }

        write!(f, "\nstack:")?;
        for (arg, size) in self.stack.iter() {
            write!(f, " [{arg}; {size}]")?;
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
