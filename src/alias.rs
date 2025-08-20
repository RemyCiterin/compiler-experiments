use crate::ssa::*;

use slotmap::*;

use std::collections::{BTreeSet, HashSet};


/// The lattice of finite sets of size ones, representing either a singleton of one element,
/// either the top element.
#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub enum Maybe<T> {
    All,
    Just(T),
}

impl<T: std::fmt::Display> std::fmt::Display for Maybe<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::All => write!(f, "⊤"),
            Self::Just(x) => write!(f, "{x}"),
        }
    }
}

impl<T: Eq + Clone> Maybe<T> {
    pub fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::All, _) => Self::All,
            (_, Self::All) => Self::All,
            (Self::Just(x), Self::Just(y)) =>
                if x == y {Self::Just(x.clone())} else {Self::All},
        }
    }

    pub fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::All, _) => false,
            (Self::Just(_), Self::All) => true,
            (Self::Just(_), Self::Just(_)) => false,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Alias {
    All,

    /// The location represent a pointer to a local variable, plus an offset
    Slot(Maybe<Slot>, Maybe<i32>),

    /// The location represent a pointer to a global variable, plus an offset
    Global(Maybe<String>, Maybe<i32>),

    /// Null pointer
    Null,

    /// Represent the empty set
    Undef,
}

impl std::fmt::Display for Alias {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::All => write!(f, "⊤"),
            Self::Slot(x, o) => write!(f, "slot({x} + {o})"),
            Self::Global(x, o) => write!(f, "global({x} + {o})"),
            Self::Null => write!(f, "NULL"),
            Self::Undef => write!(f, "⊥"),
        }
    }
}

impl Alias {
    pub fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::All, _) => Self::All,
            (_, Self::All) => Self::All,
            (Self::Slot(..), Self::Global(..)) => Self::All,
            (Self::Global(..), Self::Slot(..)) => Self::All,
            (Self::Slot(x, a), Self::Slot(y, b)) =>
                Self::Slot(x.join(y), a.join(b)),
            (Self::Global(x, a), Self::Global(y, b)) =>
                Self::Global(x.join(y), a.join(b)),
            (Self::Null, x) => if x == &Self::Null {Self::Null} else {Self::All},
            (x, Self::Null) => if x == &Self::Null {Self::Null} else {Self::All},
            (Self::Undef, x) => x.clone(),
            (x, Self::Undef) => x.clone(),
        }
    }
}

pub struct BasicAA {
    /// SSA graph of the associated CFG
    graph: SparseSecondaryMap<Var, BTreeSet<InstrId>>,

    /// Current evaluation of each variable, this map is strictly decreasing during search
    /// (not point to point), ensuring convergence
    alias: SparseSecondaryMap<Var, Alias>,

    /// Set of visited CFG nodes, we only propagate informations once from the CFG, then we use the
    /// SSA graph to propagate information
    visited: HashSet<Label>,

    /// Worklist of SSA nodes to propagate
    ssa_work_list: Vec<InstrId>,

    /// Worklist of CFG nodes to propagate
    cfg_work_list: Vec<Label>,
}

impl BasicAA {
    pub fn new(cfg: &Cfg<COp, CCond>) -> Self {
        let mut alias = SparseSecondaryMap::new();

        for (var, kind) in cfg.iter_vars() {
            match kind {
                VarKind::Arg =>
                    _ = alias.insert(var, Alias::All),
                _ =>
                    _ = alias.insert(var, Alias::Undef),
            }


        }

        Self {
            alias,
            visited: HashSet::new(),
            ssa_work_list: Vec::new(),
            cfg_work_list: vec![cfg.entry()],
            graph: cfg.ssa_graph(),
        }
    }

    pub fn show(&self) {
        for (var, alias) in self.alias.iter() {
            println!("alias({var}) = {alias}");
        }
    }

    pub fn search(&mut self, cfg: &Cfg<COp, CCond>) {
        loop {
            if let Some(instr) = self.ssa_work_list.pop() {
                self.visit_instr(cfg, instr);
                continue;
            }

            if let Some(block) = self.cfg_work_list.pop() {
                self.visit_label(cfg, block);
                continue;
            }

            break;
        }
    }

    pub fn visit_label(&mut self, cfg: &Cfg<COp, CCond>, block: Label) {
        if self.visited.contains(&block) { return; }
        self.visited.insert(block);

        for id in 0..cfg[block].stmt.len() {
            self.visit_instr(cfg, (block, id));
        }
    }

    /// Visit an instruction it it's associated CFG node is already visited. If the visit of
    /// the current instruction introduce any change in the current map, then we propagate the
    /// search by adding all the childs of the instruction in the SSA graph to the SSA worklist
    pub fn visit_instr(&mut self, cfg: &Cfg<COp, CCond>, id: InstrId) {
        if !self.visited.contains(&cfg.label_instr(id)) { return; }

        let instr = cfg[id].clone();
        let old = instr.destination().map(|x| self.alias[x].clone());

        match instr.clone() {
            Instr::Phi(dest, args) =>
                self.visit_phi(dest, args),
            Instr::Move(v, l) =>
                self.visit_move(v, l),
            Instr::Branch(_, _, l1, l2) =>
                self.visit_branch(l1, l2),
            Instr::Jump(label) =>
                self.visit_jump(label),
            Instr::Operation(dest, COp::PtrAdd, args) =>
                self.visit_add(dest, args[0], args[1]),
            _ => {
                if let Some(dest) = instr.destination() {
                    self.alias[dest] = Alias::All;
                }
            }
        }

        if let Some(dest) = instr.destination() && Some(self.alias[dest].clone()) != old {
            for &id in self.graph[dest].iter() {
                self.ssa_work_list.push(id);
            }
        }
    }

    pub fn visit_add(&mut self, dest: Var, v1: Var, v2: Var) {
        let l1 = self.alias[v1].clone();
        let l2 = self.alias[v2].clone();

        self.alias[dest] = match (l1, l2) {
            (x, Alias::Null) => x,
            (Alias::Null, _) => Alias::Null,
            (Alias::Slot(s,_), _) => Alias::Slot(s, Maybe::All),
            (Alias::Global(s,_), _) => Alias::Global(s, Maybe::All),
            (Alias::All, _) => Alias::All,
            (Alias::Undef, _) => Alias::All,
        };
    }

    pub fn eval_lit(&self, lit: Lit) -> Alias {
        match lit {
            Lit::Var(x) => self.alias[x].clone(),
            Lit::Stack(s) => Alias::Slot(Maybe::Just(s), Maybe::Just(0)),
            Lit::Addr(a) => Alias::Global(Maybe::Just(a), Maybe::Just(0)),
            Lit::Undef => Alias::Undef,
            Lit::Int(x) =>
                if x == 0 {Alias::Null} else {Alias::All},
        }
    }

    /// Visit a move instruction
    pub fn visit_move(&mut self, v: Var, l: Lit) {
        self.alias[v] = self.eval_lit(l);
    }

    /// Visit a jump instruction
    pub fn visit_jump(&mut self, label: Label) {
        self.cfg_work_list.push(label);
    }

    /// Visit a branch instruction
    pub fn visit_branch(&mut self, l1: Label, l2: Label) {
        self.cfg_work_list.push(l1);
        self.cfg_work_list.push(l2);
    }

    pub fn visit_phi(&mut self, dest: Var, args: Vec<(Lit, Label)>) {
        let mut ret = Alias::Undef;

        for (arg, _) in args {
            let elem = self.eval_lit(arg.clone());
            ret = ret.join(&elem);
        }

        self.alias[dest] = ret;
    }
}
