//! This module perform copy and constant propagation, it use the ssa graph and the control flow
//! graph to propagate informations until we reach a fixed point

use std::collections::{HashSet, BTreeSet};
use crate::ssa::*;
use slotmap::*;

#[derive(Clone, PartialEq)]
pub enum Lattice {
    /// We have no information about the content of this variable
    Top,

    /// This variable may contains two values
    Bot,

    /// This variable contains a constant integer
    Int(i32),

    /// This variable contains a constant address
    Addr(String),

    /// This variable is a copy of a variable previously defined in the dominance tree
    Copy(Var),

    /// A local stack pointer
    Stack(Slot),
}

impl std::fmt::Display for Lattice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lattice::Addr(s) => write!(f, "{}", s),
            Lattice::Int(i) => write!(f, "{}", i),
            Lattice::Copy(v) => write!(f, "{}", v),
            Lattice::Stack(off) => write!(f, "stack({off})"),
            Lattice::Top => write!(f, "top"),
            Lattice::Bot => write!(f, "bot"),
        }
    }
}

pub struct CopyProp {
    /// SSA graph of the associated CFG
    graph: SparseSecondaryMap<Var, BTreeSet<InstrId>>,

    /// Current evaluation of each variable, this map is strictly decreasing during search
    /// (not point to point), ensuring convergence
    lattice: SparseSecondaryMap<Var, Lattice>,

    /// Set of visited CFG nodes, we only propagate informations once from the CFG, then we use the
    /// SSA graph to propagate information
    visited: HashSet<Label>,

    /// The list of successors of each blocks, it is used to replace trivial branch instructions by
    /// jumps
    successors: SparseSecondaryMap<Label, HashSet<Label>>,

    /// Worklist of SSA nodes to propagate
    ssa_work_list: Vec<InstrId>,

    /// Worklist of CFG nodes to propagate
    cfg_work_list: Vec<Label>,
}

impl CopyProp {
    pub fn new(cfg: &Cfg<Instr>) -> Self {
        let mut lattice = SparseSecondaryMap::new();

        for (var, kind) in cfg.iter_vars() {
            match kind {
                VarKind::Arg =>
                    _ = lattice.insert(var, Lattice::Copy(var)),
                _ =>
                    _ = lattice.insert(var, Lattice::Top),
            }
        }

        Self {
            lattice,
            visited: HashSet::new(),
            ssa_work_list: Vec::new(),
            cfg_work_list: vec![cfg.entry()],
            successors: SparseSecondaryMap::new(),
            graph: cfg.ssa_graph(),
        }
    }

    pub fn search(&mut self, cfg: &Cfg<Instr>) {
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

    pub fn visit_label(&mut self, cfg: &Cfg<Instr>, block: Label) {
        if self.visited.contains(&block) { return; }
        self.visited.insert(block);

        for id in 0..cfg[block].stmt.len() {
            self.visit_instr(cfg, (block, id));
        }
    }

    /// Visit an instruction it it's associated CFG node is already visited. If the visit of
    /// the current instruction introduce any change in the current map, then we propagate the
    /// search by adding all the childs of the instruction in the SSA graph to the SSA worklist
    pub fn visit_instr(&mut self, cfg: &Cfg<Instr>, id: InstrId) {
        if !self.visited.contains(&cfg.label_instr(id)) { return; }

        let instr = cfg[id].clone();
        let old = instr.destination().map(|x| self.lattice[x].clone());

        match instr.clone() {
            Instr::Phi(dest, args) =>
                self.visit_phi(dest, args),
            Instr::Move(v, l) =>
                self.visit_move(v, l),
            Instr::Branch(cond, l1, l2) =>
                self.visit_branch(id.0, cond, l1, l2),
            Instr::Jump(label) =>
                self.visit_jump(id.0, label),
            _ => {
                if let Some(dest) = instr.destination() {
                    self.lattice[dest] = Lattice::Bot;
                }
            }
        }

        if let Some(dest) = instr.destination() && Some(self.lattice[dest].clone()) != old {
            for &id in self.graph[dest].iter() {
                self.ssa_work_list.push(id);
            }
        }
    }

    /// the lattice element assigned to a variable in case of a copy, it also prpagate informations
    /// if we try to copy from an already analysed variable
    pub fn copy(&self, v: Var) -> Lattice {
        match &self.lattice[v] {
            Lattice::Addr(s) => Lattice::Addr(s.clone()),
            Lattice::Stack(s) => Lattice::Stack(*s),
            Lattice::Copy(x) => Lattice::Copy(*x),
            Lattice::Int(i) => Lattice::Int(*i),
            _ => Lattice::Copy(v),
        }
    }

    /// Visit a move instruction
    pub fn visit_move(&mut self, v: Var, l: Lit) {
        self.lattice[v] = match l {
            Lit::Var(x) => self.copy(x),
            Lit::Stack(off) => Lattice::Stack(off),
            Lit::Addr(a) => Lattice::Addr(a.clone()),
            Lit::Int(i) => Lattice::Int(i),
        };
    }

    /// Visit a jump instruction
    pub fn visit_jump(&mut self, block: Label, label: Label) {
        self.successors.insert(block, HashSet::new());
        self.successors[block].insert(label);
        self.cfg_work_list.push(label);
    }

    /// Visit a branch instruction
    pub fn visit_branch(&mut self, block: Label, c: Lit, l1: Label, l2: Label) {

        macro_rules! push {
            ( $($e:expr),* ) =>
                {{
                     self.successors.insert(block, HashSet::new());
                     $(self.successors[block].insert($e);)*
                     $(self.cfg_work_list.push($e);)*
                 }};
        }

        match c {
            Lit::Int(i) => push!( if i != 0 {l1} else {l2} ),
            Lit::Var(x) => {
                match self.lattice[x] {
                    Lattice::Int(i) => push!( if i != 0 {l1} else {l2} ),
                    _ => push!(l1, l2),
                }
            }
            _ => push!(l1, l2)
        }
    }

    pub fn visit_phi(&mut self, dest: Var, args: Vec<(Lit, Label)>) {
        let mut ret = Lattice::Top;

        for (arg, _) in args {
            let elem = match arg.clone() {
                Lit::Var(v) => self.lattice[v].clone(),
                Lit::Int(x) => Lattice::Int(x),
                Lit::Addr(s) => Lattice::Addr(s.clone()),
                Lit::Stack(off) => Lattice::Stack(off),
            };

            ret = match (ret, elem.clone()) {
                (Lattice::Bot, Lattice::Copy(v)) => {
                    if v == dest { self.copy(dest) }
                    else { Lattice::Bot }
                }
                (Lattice::Copy(v), Lattice::Bot) => {
                    if Lattice::Copy(v) == elem { self.copy(v) }
                    else { Lattice::Bot }
                }
                (Lattice::Bot, _) => Lattice::Bot,
                (_, Lattice::Bot) => Lattice::Bot,
                (Lattice::Top, x) => x,
                (x, Lattice::Top) => x,
                (x, y) => {
                    if x == y { x } else { Lattice::Bot }
                }
            };
        }

        self.lattice[dest] = ret;
    }

    pub fn run(&mut self, cfg: &mut Cfg<Instr>) {
        self.search(cfg);

        for block in cfg.labels() {
            if !self.visited.contains(&block) {
                cfg.remove_block(block);
                continue;
            }

            let mut stmt: Vec<Instr> = vec![];

            for mut instr in cfg[block].stmt.iter().cloned() {
                for lit in instr.literals_mut() {
                    if let Lit::Var(x) = lit.clone() {
                        match self.lattice[x].clone() {
                            Lattice::Stack(s) => *lit = Lit::Stack(s),
                            Lattice::Addr(s) => *lit = Lit::Addr(s),
                            Lattice::Int(i) => *lit = Lit::Int(i),
                            Lattice::Copy(v) => *lit = Lit::Var(v),
                            _ => {}
                        }
                    }
                }

                if let Instr::Phi(dest, vars) = &mut instr {
                    let mut i: usize = 0;

                    while i < vars.len() {
                        if !self.visited.contains(&vars[i].1)
                            || !self.successors[vars[i].1].contains(&block) {
                            vars.remove(i);
                            continue;
                        }

                        i += 1;
                    }

                    if vars.len() == 1 {
                        instr = Instr::Move(*dest, vars[0].0.clone());
                    }
                }

                if let Instr::Branch(_, l1, l2) = &instr
                    && !self.successors[block].contains(l1) {
                    instr = Instr::Jump(*l2);
                }

                if let Instr::Branch(_, l1, l2) = &instr
                    && !self.successors[block].contains(l2) {
                    instr = Instr::Jump(*l1);
                }

                let used: bool =
                    if let Some(d) = instr.destination()
                        {self.graph[d].len() > 0 && self.lattice[d] == Lattice::Bot}
                    else {false};

                if instr.may_have_side_effect() || used {
                    stmt.push(instr);
                }
            }

            cfg.set_block_stmt(block, stmt);
        }
    }
}
