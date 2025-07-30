use slotmap::*;
use std::collections::BTreeSet;
use crate::ast::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Instr {
    /// Binary operations
    Binop(Var, Binop, Var, Var),

    /// Unary operations
    Unop(Var, Unop, Var),

    /// Allocate a stack slot and return a pointer to it
    Alloca(Var),

    /// Return a value from a function call
    Return(Var),

    /// Move a value from one position to another
    Move(Var, Var),

    /// Load a value from a pointer
    Load(Var, Var),

    /// Store a value from a pointer
    Store(Var, Var),

    /// Call a function with a given number of arguments
    Call(Var, String, Vec<Var>),

    /// Jump to a label
    Jump(Label),

    /// Branch to a lable depending of a value
    Branch(Var, Label, Label),

    /// Assign a value to a pointer to a label
    La(Var, String),

    /// Assign a value to a constant integer
    Li(Var, isize),

    /// Phi expression, contains the assignation for the predecessors in order
    Phi(Var, Vec<(Var, Label)>),
}

impl Instr {
    pub fn destination(&self) -> Option<Var> {
        match self {
            Self::Binop(ret, ..)
                | Self::Unop(ret, ..)
                | Self::Alloca(ret)
                | Self::Move(ret, ..)
                | Self::Load(ret, ..)
                | Self::Call(ret, ..)
                | Self::La(ret, ..)
                | Self::Li(ret, ..)
                | Self::Phi(ret, ..)
                => Some(*ret),
            _ => None
        }
    }

    pub fn destination_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Binop(ret, ..)
                | Self::Unop(ret, ..)
                | Self::Alloca(ret)
                | Self::Move(ret, ..)
                | Self::Load(ret, ..)
                | Self::Call(ret, ..)
                | Self::La(ret, ..)
                | Self::Li(ret, ..)
                | Self::Phi(ret, ..)
                => Some(ret),
            _ => None
        }
    }

    /// Return the list of operands the instruction depends on,
    /// including all the operands in the label arguments
    pub fn operands(&self) -> Vec<Var> {
        match self {
            Self::Binop(_, _, x, y) => vec![*x,*y],
            Self::Unop(_, _, x)=> vec![*x],
            Self::Move(_, x) => vec![*x],
            Self::Load(_, y) => vec![*y],
            Self::Store(x, y) => vec![*x,*y],
            Self::Branch(x, _, _) => vec![*x],
            Self::Return(x) => vec![*x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Alloca(_) => vec![],
            Self::Call(_, _, args) => args.clone(),
            Self::Phi(_, vars) => vars.iter().map(|(v, _)| *v).collect()
        }
    }

    /// Return the list of operands the instruction depends on,
    /// including all the operands in the label arguments
    pub fn operands_mut(&mut self) -> Vec<&mut Var> {
        match self {
            Self::Binop(_, _, x, y) => vec![x,y],
            Self::Unop(_, _, x)=> vec![x],
            Self::Move(_, x) => vec![x],
            Self::Load(_, y) => vec![y],
            Self::Store(x, y) => vec![x,y],
            Self::Branch(x, _, _) => vec![x],
            Self::Return(x) => vec![x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Alloca(_) => vec![],
            Self::Call(_, _, args) => args.iter_mut().collect(),
            Self::Phi(_, vars) =>
                vars.iter_mut().map(|(v,_)| v).collect()
        }
    }

    pub fn labels(&self) -> Vec<Label> {
        match self {
            Self::Jump(label) => vec![*label],
            Self::Branch(_, l1, l2) => vec![*l1, *l2],
            _ => vec![]
        }
    }

    pub fn labels_mut(&mut self) -> Vec<&mut Label> {
        match self {
            Self::Jump(label) => vec![label],
            Self::Branch(_, l1, l2) => vec![l1, l2],
            _ => vec![]
        }
    }
}


new_key_type!{
    pub struct Var;
}

new_key_type!{
    pub struct Label;
}


pub struct Block {
    pub stmt: Vec<Instr>,
}

impl Block {
    pub fn callees(&self) -> Vec<Label> {
        for instr in self.stmt.iter() {
            if instr.labels().is_empty() { continue; }
            return instr.labels();
        }

        vec![]
    }
}

/// A control flow graph, each control flow graph is specific to one function or procedure,
/// it contains an entry point, a set of blocks, and a set of variables
pub struct Cfg {
    /// Associate for each label the definition of it's
    /// associated block
    blocks: SlotMap<Label, Block>,

    /// Entry of the control flow graph
    entry: Label,

    /// Associate a type to each variable
    vars: SlotMap<Var, Option<Label>>,

    /// Callers of each block
    callers: SecondaryMap<Label, BTreeSet<Label>>,

    /// If true, then the graph is expected to be in SSA form
    ssa: bool,
}

impl std::ops::Index<Label> for Cfg {
    type Output = Block;

    fn index(&self, name: Label) -> &Block {
        &self.blocks[name]
    }
}

impl std::ops::Index<Var> for Cfg {
    type Output = Option<Label>;

    fn index(&self, name: Var) -> &Option<Label> {
        &self.vars[name]
    }
}

impl Cfg {
    /// If ssa is set, then the SSA form will be checked during modification of the arguments
    /// and content of a block
    pub fn new(ssa: bool) -> Self {
        let mut blocks =
            SlotMap::with_key();
        let entry =
            blocks.insert(Block{stmt: vec![]});
        let mut callers = SecondaryMap::new();
        callers.insert(entry, BTreeSet::new());
        Self {
            ssa,
            entry,
            blocks,
            callers,
            vars: SlotMap::with_key(),
        }
    }

    pub fn entry(&self) -> Label {
        self.entry
    }

    pub fn callers(&self, block: Label) -> &BTreeSet<Label> {
        &self.callers[block]
    }

    fn mk_preorder(&self, block: Label, order: &mut Vec<Label>, seen: &mut BTreeSet<Label>) {
        if seen.contains(&block) { return; }
        seen.insert(block);
        order.push(block);

        for callee in self[block].callees() {
            self.mk_preorder(callee, order, seen);
        }
    }

    /// Return the preorder (also called reverse postorder) of a control flow graph
    pub fn preorder(&self) -> Vec<Label> {
        let mut seen = BTreeSet::new();
        let mut order = vec![];

        self.mk_preorder(self.entry, &mut order, &mut seen);
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

    pub fn iter_vars(&self) -> slotmap::basic::Iter<'_, Var, Option<Label>> {
        self.vars.iter()
    }

    pub fn fresh_var(&mut self) -> Var {
        self.vars.insert(None)
    }

    pub fn start_ssa(&mut self) {
        let mut vars = std::mem::take(&mut self.vars);
        assert!(!self.ssa);

        for (label, block) in self.blocks.iter() {
            for instr in block.stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == None);
                    vars[x] = Some(label);
                }
            }
        }

        self.vars = vars;
        self.ssa = true;
    }

    pub fn fresh_label(&mut self) -> Label {
        let new =
            self.blocks.insert(Block{stmt: vec![]});
        self.callers.insert(new, BTreeSet::new());
        new
    }

    pub fn set_block_stmt(&mut self, block: Label, stmt: Vec<Instr>) {
        if self.ssa {
            let mut vars = std::mem::take(&mut self.vars);

            for instr in self[block].stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == Some(block));
                    vars[x] = None;
                }
            }

            for instr in stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == None);
                    vars[x] = Some(block);
                }
            }

            self.vars = vars;
        }

        for callee in self[block].callees() {
            self.callers[callee].remove(&block);
        }

        self.blocks[block].stmt = stmt;

        for callee in self[block].callees() {
            self.callers[callee].insert(block);
        }
    }

    pub fn ssa(&self) -> bool { self.ssa }
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


impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Binop(dest, op, src1, src2) =>
                write!(f, "{} := {} {} {}", dest, src1, op, src2),
            Instr::Unop(dest, op, src1) =>
                write!(f, "{} := {} {}", dest, op, src1),
            Instr::Load(dest, src1) =>
                write!(f, "{} := [{}]", dest, src1),
            Instr::Store(addr, val) =>
                write!(f, "[{}] := {}", addr, val),
            Instr::Move(dest, src1) =>
                write!(f, "{} := {}", dest, src1),
            Instr::Branch(cond, l1, l2) =>
                write!(f, "branch {}, {}, {}", cond, l1, l2),
            Instr::Jump(l) =>
                write!(f, "jump {}", l),
            Instr::Return(cond) =>
                write!(f, "return {}", cond),
            Instr::Li(x, i) =>
                write!(f, "li {}, {}", x, i),
            Instr::La(x, l) =>
                write!(f, "la {}, {}", x, l),
            Instr::Alloca(dest) =>
                write!(f, "{} := alloca", dest),
            Instr::Call(dest, name, args) => {
                write!(f, "{} := {}(", dest, name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                } write!(f, ")")
            }
            Instr::Phi(dest, args) => {
                write!(f, "{} := phi", dest)?;
                for (var, label) in args.iter() {
                    write!(f, " ({}, {})", var, label)?;
                }
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for Cfg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "entry: {}\n", self.entry())?;

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
