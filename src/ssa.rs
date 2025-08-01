use slotmap::*;
use std::collections::BTreeSet;
use crate::ast::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Instr {
    /// Binary operations
    Binop(Var, Binop, Var, Var),

    /// Unary operations
    Unop(Var, Unop, Var),

    /// Return a value from a function call
    Return(Var),

    /// Move a value from one position to another
    Move(Var, Var),

    /// Load a value from a pointer
    Load{dest: Var, addr: Var, volatile: bool, size: u8},

    /// Store a value from a pointer
    Store{val: Var, addr: Var, volatile: bool, size: u8},

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
                | Self::Move(ret, ..)
                | Self::Load{dest:ret, ..}
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
                | Self::Move(ret, ..)
                | Self::Load{dest: ret, ..}
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
            Self::Load{addr: y, ..} => vec![*y],
            Self::Store{addr: x, val: y, ..} => vec![*x,*y],
            Self::Branch(x, _, _) => vec![*x],
            Self::Return(x) => vec![*x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
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
            Self::Load{addr: y, ..} => vec![y],
            Self::Store{addr: x, val: y, ..} => vec![x,y],
            Self::Branch(x, _, _) => vec![x],
            Self::Return(x) => vec![x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
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
    /// A local variable defined in a block
    Local(Label),

    /// A stack variable defined at the function entry point
    Stack,

    /// A function argument
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

    /// Associate a type to each variable
    vars: SlotMap<Var, VarKind>,

    /// preds of each block
    preds: SecondaryMap<Label, BTreeSet<Label>>,

    /// If true, then the graph is expected to be in SSA form
    ssa: bool,

    /// A set of variables representing stack locations with a size and alignment constraint
    pub stack: Vec<(Var, usize, u8)>,

    /// Represent arguments of the function
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

impl Cfg {
    /// If ssa is set, then the SSA form will be checked during modification of the arguments
    /// and content of a block
    pub fn new(ssa: bool, args: Vec<Var>) -> Self {
        let mut blocks =
            SlotMap::with_key();
        let entry =
            blocks.insert(Block{stmt: vec![]});
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

    pub fn iter_vars(&self) -> slotmap::basic::Iter<'_, Var, VarKind> {
        self.vars.iter()
    }

    pub fn fresh_var(&mut self) -> Var {
        self.vars.insert(VarKind::Undef)
    }

    pub fn fresh_stack_var(&mut self, size: usize, align: u8) -> Var {
        let var = self.vars.insert(VarKind::Stack);
        self.stack.push((var, size, align));
        var
    }

    pub fn fresh_arg(&mut self) -> Var {
        let var = self.vars.insert(VarKind::Arg);
        self.args.push(var);
        var
    }

    pub fn start_ssa(&mut self) {
        let mut vars = std::mem::take(&mut self.vars);
        assert!(!self.ssa);

        for (_, kind) in vars.iter_mut() {
            *kind = VarKind::Undef;
        }

        for (label, block) in self.blocks.iter() {
            for instr in block.stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Undef);
                    vars[x] = VarKind::Local(label);
                }
            }
        }

        for &x in self.args.iter() {
            assert!(vars[x] == VarKind::Undef);
            vars[x] = VarKind::Arg;
        }

        for (x, _, _) in self.stack.iter() {
            assert!(vars[*x] == VarKind::Undef);
            vars[*x] = VarKind::Stack;
        }

        self.vars = vars;
        self.ssa = true;
    }

    pub fn fresh_label(&mut self) -> Label {
        let new =
            self.blocks.insert(Block{stmt: vec![]});
        self.preds.insert(new, BTreeSet::new());
        new
    }

    pub fn remove_var(&mut self, var: Var) {
        assert!(self.vars[var] == VarKind::Undef);
        self.vars.remove(var);
    }

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

    pub fn set_block_stmt(&mut self, block: Label, stmt: Vec<Instr>) {
        if self.ssa {
            let mut vars = std::mem::take(&mut self.vars);

            for instr in self[block].stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Local(block));
                    vars[x] = VarKind::Undef;
                }
            }

            for instr in stmt.iter() {
                if let Some(x) = instr.destination() {
                    assert!(vars[x] == VarKind::Undef);
                    vars[x] = VarKind::Local(block);
                }
            }

            self.vars = vars;
        }

        for succ in self[block].succs() {
            self.preds[succ].remove(&block);
        }

        self.blocks[block].stmt = stmt;

        for succ in self[block].succs() {
            self.preds[succ].insert(block);
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
            Instr::Load{dest, addr, volatile: true, size} =>
                write!(f, "{} := [volatile {}] as u{}", dest, addr, (8 as usize) << size),
            Instr::Load{dest, addr, volatile: false, size} =>
                write!(f, "{} := [{}] as u{}", dest, addr, (8 as usize) << size),
            Instr::Store{addr, val, ..} =>
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
