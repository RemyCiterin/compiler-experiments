use std::collections::{BTreeSet, HashMap};
use crate::identifiers::*;
use std::fmt;
use crate::ast::{Binop, Unop};
use crate::ast;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Label{
    name: usize,
    args: Vec<Id>,

}

impl Label {
    pub fn replace(&mut self, x: Id, y: Id) {
        for arg in self.args.iter_mut() {
            if *arg == x {
                *arg = y;
            }
        }
    }

    pub fn rename(&mut self, x: &Vec<Id>, y: &Vec<Id>) -> () {
        for (&a, &b) in x.iter().zip(y) {
            self.replace(a, b);
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".label{}", self.name)?;

        if self.args.len() > 0 {
            write!(f, "(")?;
            for (i, arg) in self.args.iter().enumerate() {
                write!(f, "v{}", arg)?;
                if i+1 != self.args.len() {
                    write!(f, ", ")?;
                }
            }

            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Instr {
    Binop(Id, Binop, Id, Id),
    Unop(Id, Unop, Id),
    Move(Id, Id),
    Load(Id, Id),
    LoadStack(Id, usize),
    StoreStack(Id, usize),
    Store(Id, Id),
    Branch(Id, Label, Label),
    Jump(Label),
    Return(Id),
    La(Id, String),
    Li(Id, isize),
}

impl Instr {
    pub fn operands(&self) -> Vec<Id> {
        match self {
            Instr::Binop(_, _, x, y) => vec![*x,*y],
            Instr::Unop(_, _, x)=> vec![*x],
            Instr::Move(_, x) => vec![*x],
            Instr::Load(_, y) => vec![*y],
            Instr::Store(x, y) => vec![*x,*y],
            Instr::LoadStack(_, _) => vec![],
            Instr::StoreStack(x, _) => vec![*x],
            Instr::Branch(x, l1, l2) =>
                vec![vec![*x], l1.args.clone(), l2.args.clone()].concat(),
            Instr::Return(x) => vec![*x],
            Instr::Jump(l) => l.args.clone(),
            Instr::Li(_, _) => vec![],
            Instr::La(_, _) => vec![],
        }
    }

    pub fn labels(&self) -> Vec<&Label> {
        match self {
            Instr::Branch(_, l1, l2) => vec![l1, l2],
            Instr::Jump(l) => vec![l],
            _ => vec![],
        }
    }

    pub fn labels_mut(&mut self) -> Vec<&mut Label> {
        match self {
            Instr::Branch(_, l1, l2) => vec![l1, l2],
            Instr::Jump(l) => vec![l],
            _ => vec![],
        }
    }

    pub fn operands_mut(&mut self) -> Vec<&mut Id> {
        match self {
            Instr::Binop(_, _, x, y) => vec![x,y],
            Instr::Unop(_, _, x)=> vec![x],
            Instr::Move(_, x) => vec![x],
            Instr::Load(_, y) => vec![y],
            Instr::Store(x, y) => vec![x,y],
            Instr::LoadStack(_, _) => vec![],
            Instr::StoreStack(x, _) => vec![x],
            Instr::Branch(x, l1, l2) =>
                std::iter::once(x).chain(l1.args.iter_mut()).chain(l2.args.iter_mut()).collect(),
            Instr::Return(x) => vec![x],
            Instr::Jump(l) => l.args.iter_mut().collect(),
            Instr::Li(_, _) => vec![],
            Instr::La(_, _) => vec![],
        }
    }


    pub fn destination(&self) -> Option<Id> {
        match self {
            Instr::Binop(x, ..)
                | Instr::Unop(x, ..)
                | Instr::Move(x, _)
                | Instr::Load(x, _)
                | Instr::LoadStack(x, _)
                | Instr::La(x, _)
                | Instr::Li(x, _) => Some(*x),
            _=> None,
        }
    }

    pub fn destination_mut(&mut self) -> Option<&mut Id> {
        match self {
            Instr::Binop(x, ..)
                | Instr::Unop(x, ..)
                | Instr::Move(x, _)
                | Instr::Load(x, _)
                | Instr::LoadStack(x, _)
                | Instr::Li(x, _)
                | Instr::La(x, _) => Some(x),
            _=> None,
        }
    }

    pub fn replace(&mut self, x: Id, y: Id) -> () {
        for v in self.operands_mut() {
            if *v == x {
                *v = y;
            }
        }
    }

    pub fn rename(&mut self, x: &Vec<Id>, y: &Vec<Id>) -> () {
        for (&a, &b) in x.iter().zip(y) {
            self.replace(a, b);
        }
    }

    /// add the set of callees of an instructions to a set (if the instruction is a move, jump...)
    pub fn callees(&self, result: &mut BTreeSet<usize>) {
        match self {
            Instr::Jump(l) =>
                _ = result.insert(l.name),
            Instr::Branch(_, l1, l2) =>
                _ = result.extend([l1.name, l2.name].iter()),
            _ => {}
        }
    }
}


impl fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Binop(dest, op, src1, src2) =>
                write!(f, "v{} := v{} {} v{}", dest, src1, op, src2),
            Instr::Unop(dest, op, src1) =>
                write!(f, "v{} := {} v{}", dest, op, src1),
            Instr::Load(dest, src1) =>
                write!(f, "v{} := [v{}]", dest, src1),
            Instr::Store(addr, val) =>
                write!(f, "[v{}] := v{}", addr, val),
            Instr::LoadStack(dest, src1) =>
                write!(f, "v{} := stack[{}]", dest, src1),
            Instr::StoreStack(val, addr) =>
                write!(f, "stack[{}] := v{}", addr, val),
            Instr::Move(dest, src1) =>
                write!(f, "v{} := v{}", dest, src1),
            Instr::Branch(cond, l1, l2) =>
                write!(f, "branch v{}, {}, {}", cond, l1, l2),
            Instr::Jump(l) =>
                write!(f, "jump {}", l),
            Instr::Return(cond) =>
                write!(f, "return v{}", cond),
            Instr::Li(x, i) =>
                write!(f, "li v{}, {}", x, i),
            Instr::La(x, l) =>
                write!(f, "la v{}, {}", x, l),
        }
    }
}

pub type Stmt = Vec<Instr>;

#[derive(Debug, Clone)]
pub struct Block {
    pub callees: BTreeSet<usize>,
    pub callers: BTreeSet<usize>,
    pub label: Label,
    pub stmt: Stmt,
}

impl Block {
    pub fn new(name: usize) -> Self {
        Block{
            label: Label{name, args: vec![]},
            stmt: vec![],
            callees: BTreeSet::new(),
            callers: BTreeSet::new(),
        }
    }

    pub fn labels(&self) -> Vec<&Label> {
        for instr in self.stmt.iter() {
            let vec = instr.labels();
            if vec.len() != 0 { return vec; }
        }

        return vec![];
    }

    pub fn labels_mut(&mut self) -> Vec<&mut Label> {
        for instr in self.stmt.iter_mut() {
            let vec = instr.labels_mut();
            if vec.len() != 0 { return vec; }
        }

        return vec![];
    }

    pub fn remove_copies(&mut self) {
        let mut i = 0;

        while i < self.stmt.len() {
            if let Instr::Move(x, y) = self.stmt[i] {
                self.stmt.remove(i);

                for _ in i..self.stmt.len() {
                    for v in self.stmt[i].operands_mut() {
                        if *v == x {
                            *v = y;
                        }
                    }
                }
            } else {
                i += 1;
            }
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        write!(f, "{}:", self.label)?;

        for instr in self.stmt.iter() {
            write!(f, "\n\t{}", instr)?;
        }

        write!(f, "\n")?;

        Ok(())
    }
}

/// Construction of CFG is done in three steps:
/// - first we construct the control flow graph without SSA form or block arguments
/// - then we perform a liveness analysis of the blocks to find the set of alive variables
///   as input and output of each block
/// - finally we build the SSA form and add all the block arguments based on the result of the
///   liveness analysis
#[derive(Debug, Clone)]
pub struct CFG {
    /// The block at index `i` use the label `i`
    blocks: Vec<Block>,
    register_counter: usize,
}

#[derive(Debug, Clone)]
pub struct Builder {
    cfg: CFG,
    stmt: Stmt,
    label: usize,
    map: HashMap<String, Id>,
}

impl Builder {
    pub fn new() -> Self {
        Builder{
            cfg: CFG::new(),
            stmt: Vec::new(),
            label: 0,
            map: HashMap::new(),
        }
    }

    pub fn into_cfg(self) -> CFG {
        return self.cfg;
    }

    pub fn gen_expr(&mut self, expr: ast::Expr) -> Id {
        match expr {
            ast::Expr::Constant(i) => {
                let id: Id = self.cfg.fresh_register();
                self.stmt.push(Instr::Li(id, i));
                id
            }
            ast::Expr::Variable(s) => {
                if !self.map.contains_key(&s) {
                    let id: Id = self.cfg.fresh_register();
                    self.map.insert(s.clone(), id);
                }

                self.map[&s]
            }
            ast::Expr::Binop(binop, lhs, rhs) => {
                let l: Id = self.gen_expr(*lhs);
                let r: Id  = self.gen_expr(*rhs);
                let id: Id = self.cfg.fresh_register();
                self.stmt.push(Instr::Binop(id, binop, l, r));
                id
            }
            ast::Expr::Unop(unop,x) => {
                let y: Id = self.gen_expr(*x);
                let id: Id = self.cfg.fresh_register();
                self.stmt.push(Instr::Unop(id, unop, y));
                id
            }
        }
    }

    pub fn gen_branch(&mut self, cond: Id, l1: usize, l2: usize) {
        let label1 = Label{name: l1, args: vec![]};
        let label2 = Label{name: l2, args: vec![]};
        self.stmt.push(Instr::Branch(cond, label1, label2));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_jump(&mut self, l: usize) {
        let label = Label{name: l, args: vec![]};
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_return(&mut self, id: Id) {
        self.stmt.push(Instr::Return(id));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_stmt(&mut self, stmt: ast::Stmt) {
        match stmt {
            ast::Stmt::Nop => {}
            ast::Stmt::Ite(cond, lhs, rhs) => {
                let id = self.gen_expr(cond);
                let l1 = self.cfg.fresh_block();
                let l2 = self.cfg.fresh_block();
                let exit = self.cfg.fresh_block();
                self.gen_branch(id, l1, l2);


                self.label = l1;
                self.gen_stmt(*lhs);
                self.gen_jump(exit);

                self.label = l2;
                self.gen_stmt(*rhs);
                self.gen_jump(exit);

                self.label = exit;
            }
            ast::Stmt::While(cond, stmt) => {
                let header = self.cfg.fresh_block();
                let body = self.cfg.fresh_block();
                let exit = self.cfg.fresh_block();

                self.gen_jump(header);

                self.label = header;
                let id = self.gen_expr(cond);
                self.gen_branch(id, body, exit);

                self.label = body;
                self.gen_stmt(*stmt);
                self.gen_jump(header);

                self.label = exit;
            }
            ast::Stmt::Seq(lhs, rhs) => {
                self.gen_stmt(*lhs);
                self.gen_stmt(*rhs);
            }
            ast::Stmt::Return(e) => {
                let id = self.gen_expr(e);
                self.gen_return(id);
            }
            ast::Stmt::Assign(s, e) => {
                let id = self.gen_expr(e);

                if !self.map.contains_key(&s) {
                    let id: Id = self.cfg.fresh_register();
                    self.map.insert(s.clone(), id);
                }

                self.stmt.push(Instr::Move(self.map[&s], id));
            }
        }
    }
}

pub trait ForwardAnalysis {
    // Update the analysis state on a block, and return if their is any change
    fn update_forward(&mut self, cfg: &CFG, block: usize) -> bool;
}

pub trait BackwardAnalysis {
    // Update the analysis state on a block, and return if their is any change
    fn update_backward(&mut self, cfg: &CFG, block: usize) -> bool;
}


impl BackwardAnalysis for Liveness {
    fn update_backward(&mut self, cfg: &CFG, block: usize) -> bool {
        self.update_live(cfg, block)
    }
}

impl CFG {
    /// Return an empty control flow graph
    pub fn new() -> Self {
        CFG{
            blocks: vec![Block::new(0)],
            register_counter: 0,
        }
    }

    /// Return the list of basic block in topological sort
    pub fn topo_sort(&self) -> Vec<usize> {
        let mut topo: TopoSort = TopoSort::new(self);

        for i in 0..self.len() {
            topo.visit(self, i);
        }

        return topo.result;
    }

    pub fn backward_analisys<A>(&self, analysis: &mut A) where A: BackwardAnalysis {
        let mut dirty: Vec<usize> = (0..self.len()).collect();

        while let Some(&block) = dirty.last() {
            dirty.pop();

            let progress: bool = analysis.update_backward(self, block);

            if progress {
                for &caller in self[block].callers.iter() {
                    dirty.push(caller);
                }
            }
        }
    }

    pub fn forward_analisys<A>(&self, analysis: &mut A) where A: ForwardAnalysis {
        let mut dirty: Vec<usize> = (0..self.len()).collect();

        while let Some(&block) = dirty.last() {
            dirty.pop();


            let progress: bool = analysis.update_forward(self, block);

            if progress {
                for &callee in self[block].callees.iter() {
                    dirty.push(callee);
                }
            }
        }
    }

    /// Update the set of `callees` of a block, and the `callers` set of it's callees
    fn update_callees(&mut self, block: usize) {
        let old_callees: BTreeSet<usize> = std::mem::take(&mut self[block].callees);
        let mut new_callees: BTreeSet<usize> = BTreeSet::new();

        for instr in self[block].stmt.iter() {
            instr.callees(&mut new_callees);
        }

        for b in old_callees.iter() {
            if !new_callees.contains(b) {
                self[*b].callers.remove(&block);
            }
        }

        for b in new_callees.iter() {
            if !old_callees.contains(b) {
                self[*b].callers.insert(block);
            }
        }

        self[block].callees = new_callees;
    }

    /// Update the instructions inside a block, and perform liveness/data-flow analysis directly
    pub fn set_block_stmt(&mut self, block: usize, stmt: Vec<Instr>) {
        self[block].stmt = stmt;
        self.update_callees(block);
    }

    /// Generate a fresh register name
    pub fn fresh_register(&mut self) -> Id {
        let result: Id = Id::from(self.register_counter);
        self.register_counter += 1;
        return result;
    }

    /// Generate a fresh block name
    pub fn fresh_block(&mut self) -> usize {
        let result: usize = self.len();
        self.blocks.push(Block{
            label: Label{name: result, args: vec![]},
            stmt: vec![],
            callees: BTreeSet::new(),
            callers: BTreeSet::new(),
        });

        return result;
    }

    /// Generate SSA representation
    pub fn into_ssa(&mut self) {
        let mut liveness: Liveness = Liveness::new(&self);
        self.backward_analisys(&mut liveness);

        for block in 0..self.len() {
            let mut map: HashMap<Id, Id> = HashMap::new();

            let mut args: Vec<Id> = vec![];

            // Restore live_in after the loop (we need to keep it unchanged)
            let inputs: &BTreeSet<Id> = &liveness.live_in[block];
            for &old_v in inputs.iter() {
                let new_v = self.fresh_register();
                map.insert(old_v, new_v);
                args.push(new_v);
            }

            self[block].label = Label{name: block, args};

            let mut stmt: Stmt = std::mem::take(&mut self[block].stmt);

            for instr in stmt.iter_mut() {
                // Firty rename arguments
                for src in instr.operands_mut() {
                    *src = map[src];
                }

                for label in instr.labels_mut() {
                    let mut args: Vec<Id> = vec![];

                    for src in liveness.live_in[label.name].iter() {
                        args.push(map[src]);
                    }

                    *label = Label{name: label.name, args};
                }

                if let Some(old_dst) = instr.destination_mut() {
                    let new_dst = self.fresh_register();
                    map.insert(*old_dst, new_dst);
                    *old_dst = new_dst;
                }
            }

            // Use lazy update because we need to keep liveness result unchanded
            self.set_block_stmt(block, stmt);
        }
    }

    /// Get out of SSA form, prepare for register allocation,
    /// use the interference matrix to ensure we doesn't perform
    /// useless copies
    pub fn out_of_ssa(&mut self) {
        let mut liveness: Liveness = Liveness::new(&self);
        self.backward_analisys(&mut liveness);

        for block in 0..self.len() {
            let mut stmt: Stmt = Vec::new();

            for instr in self[block].stmt.iter() {
                let mut add_move = |x, y| {
                    stmt.push(Instr::Move(x, y));
                };

                match instr {
                    Instr::Jump(l) => {
                        let args: &Vec<Id> = &self[l.name].label.args;

                        for (&x, &y) in l.args.iter().zip(args.iter()) {
                            add_move(y, x);
                        }
                    }
                    Instr::Branch(_, l1 ,l2) => {
                        assert!(l1.name != l2.name);

                        let args1: &Vec<Id> = &self[l1.name].label.args;
                        let args2: &Vec<Id> = &self[l2.name].label.args;

                        for (&x, &y) in l1.args.iter().zip(args1.iter()) {
                            add_move(y, x);
                        }

                        for (&x, &y) in l2.args.iter().zip(args2.iter()) {
                            add_move(y, x);
                        }
                    }
                    _ => {}
                }

                stmt.push(instr.clone());
            }

            self.set_block_stmt(block, stmt);
        }
    }

    fn can_remove_argument(&self, block: usize, arg: usize) -> Option<Id> {
        println!("remove block: {} arg: {}", block, arg);
        let x = self[block].label.args[arg];
        let mut y: Option<Id> = None;

        for &b in self[block].callers.iter() {
            for l in self[b].labels() {
                if l.name == block && l.args[arg] != x {
                    if let Some(z) = y && l.args[arg] != z { return None; }
                    y = Some(l.args[arg]);
                }
            }
        }

        return y;
    }

    fn try_remove_argument(&mut self, block: usize, arg: usize) {
        if let Some(new_var) = self.can_remove_argument(block, arg) {
            let x = self[block].label.args[arg];

            self[block].label.args.remove(arg);
            let callers = std::mem::take(&mut self[block].callers);

            for &b in callers.iter() {
                for l in self[b].labels_mut() {
                    if l.name == block {
                        l.args.remove(arg);
                    }
                }
            }

            self[block].callers = callers;

            for instr in self[block].stmt.iter_mut() {
                for var in instr.operands_mut() {
                    if *var == x {
                        *var = new_var;
                    }
                }
            }
        }
    }

    pub fn remove_useless_arguments(&mut self) {
        for b in 0..self.len() {
            for a in 0..self[b].label.args.len() {
                self.try_remove_argument(b, a);
            }
        }
    }

    // Remove all the move instructions
    pub fn remove_copies(&mut self) {
        for block in self.blocks.iter_mut() {
            block.remove_copies();
        }
    }

    // Remove all the blocks made of a single jump
    pub fn remove_jumps(&mut self) {
        for b in 0..self.len() {
            if let Some(Instr::Jump(dest)) = self[b].stmt.first().cloned() {
                let callers = std::mem::take(&mut self[b].callers);
                let args = self[b].label.args.clone();

                for &caller in callers.iter() {
                    for instr in self[caller].stmt.iter_mut() {
                        for old_call in instr.labels_mut() {
                            if old_call.name == b {
                                let mut new_call = dest.clone();
                                new_call.rename(&args, &old_call.args);
                                *old_call = new_call.clone();
                            }
                        }
                    }
                }

                self[b].callers = callers;
                self.set_block_stmt(b, vec![]);
            }
        }

        for b in 0..self.len() {
            self.update_callees(b);
        }
    }

    pub fn remove_no_predecessor(&mut self) {
        for b in 0..self.len() {
            if self[b].label.name != 0 && self[b].callers.len() == 0 {
                self.set_block_stmt(b, vec![]);
                self[b].label.args.clear();
                self.update_callees(b);
            }
        }
    }

    pub fn len(&self) -> usize {
        self.blocks.len()
    }
}

impl std::ops::Index<usize> for CFG {
    type Output = Block;

    fn index(&self, id: usize) -> &Block {
        &self.blocks[id]
    }
}

impl std::ops::IndexMut<usize> for CFG {
    fn index_mut(&mut self, id: usize) -> &mut Block {
        &mut self.blocks[id]
    }
}

impl fmt::Display for CFG {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "entry: {}\n", self[0].label)?;
        write!(f, "register counter: {}\n", self.register_counter)?;

        for block in self.blocks.iter() {
            // Use an empty line between each block
            write!(f, "\n{}", block)?;
        }

        Ok(())
    }
}

pub struct Dominance {
    /// index of each block in postorder
    index: Vec<usize>,

    /// reverse postorder of the blocks
    reverse_postorder: Vec<usize>,

    /// dominance relation
    pub dom: Vec<Option<usize>>,
}

impl Dominance {
    pub fn new(cfg: &CFG) -> Self {
        // Generate a postorder using a topo sort
        let mut order: Vec<usize> = cfg.topo_sort();
        let mut index: Vec<usize> = (0..cfg.len()).collect();

        for i in 0..cfg.len() {
            index[order[i]] = i;
        }

        order.reverse();

        Self {
            index,
            reverse_postorder: order,
            dom: (0..cfg.len()).map(|_| None).collect(),
        }
    }

    pub fn run(&mut self, cfg: &CFG) {
        self.dom[0] = Some(0);

        loop {
            let mut progress: bool = false;

            for &block in self.reverse_postorder.iter() {
                let mut new_idom: Option<usize> = None;

                // assign the intersection of the processed predecessors
                for &pred in cfg[block].callers.iter() {
                    if self.dom[pred].is_none() { continue; }

                    if let Some(old) = new_idom {
                        new_idom = Some(self.intersect(pred, old));
                    } else {
                        new_idom = Some(pred);
                    }
                }

                if new_idom.is_some() && self.dom[block] != new_idom {
                    self.dom[block] = new_idom;
                    progress = true;
                }
            }

            if !progress { break; }
        }
    }

    /// Return the least commun ancestor of two nodes in the current dominance tree
    pub fn intersect(&self, mut x: usize, mut y: usize) -> usize {
        while x != y {
            while self.index[x] < self.index[y] {
                x = self.dom[x].unwrap();
            }

            while self.index[x] > self.index[y] {
                y = self.dom[y].unwrap();
            }
        }

        return x;
    }

    /// Return if a block is reachable from the entry point,
    /// if so, idiom is not defined on this node (so it panic)
    pub fn reachable(&self, block: usize) -> bool {
        self.dom[block].is_some()
    }

    /// return the immediate dominator of a block
    pub fn idom(&self, block: usize) -> usize {
        self.dom[block].unwrap()
    }
}

pub struct TopoSort {
    visited: Vec<bool>,
    result: Vec<usize>,
}

impl TopoSort {
    pub fn new(cfg: &CFG) -> Self {
        Self {
            visited: (0..cfg.len()).map(|_| false).collect(),
            result: vec![],
        }
    }

    pub fn visit(&mut self, cfg: &CFG, block: usize) {
        if self.visited[block] { return; }
        self.visited[block] = true;

        for &b in cfg[block].callees.iter() {
            self.visit(cfg, b);
        }

        self.result.push(block);
    }
}

pub struct Liveness {
    live_in: Vec<BTreeSet<Id>>,
    live_out: Vec<BTreeSet<Id>>,
}

impl Liveness {
    pub fn new(cfg: &CFG) -> Self {
        Self {
            live_in: (0..cfg.len()).map(|_| BTreeSet::new()).collect(),
            live_out: (0..cfg.len()).map(|_| BTreeSet::new()).collect(),
        }
    }

    pub fn compute_live_out(&self, cfg: &CFG, block: usize) -> BTreeSet<Id> {
        let mut results: BTreeSet<Id> = BTreeSet::new();

        for label in cfg[block].labels() {
            let mut lives = self.live_in[label.name].clone();
            let renamed = &cfg[label.name].label.args;

            // Rename label arguments
            for i in 0..label.args.len() {
                if lives.contains(&renamed[i]) {
                    lives.remove(&renamed[i]);
                    lives.insert(label.args[i]);
                }
            }

            results.extend(lives);
        }

        return results;
    }

    /// Compute the set of live variables at the input of a block
    fn compute_live_in(&mut self, cfg: &CFG, block: usize) -> BTreeSet<Id> {
        let mut results: BTreeSet<Id> = self.live_out[block].clone();

        for instr in cfg[block].stmt.iter().rev() {
            if let Some(dest) = instr.destination() {
                results.remove(&dest);
            }

            for src in instr.operands() {
                results.insert(src);
            }
        }

        return results;
    }

    /// Update the live variables of a node and propagate the analysis until we reach a fixed-point
    pub fn update_live(&mut self, cfg: &CFG, block: usize) -> bool {
        self.live_out[block] = self.compute_live_out(cfg, block);

        let new_live_in = self.compute_live_in(cfg, block);
        let old_live_in = &self.live_in[block];
        let change: bool = old_live_in != &new_live_in;

        self.live_in[block] = new_live_in;

        return change;
    }
}

pub struct InterferenceMatrix {
    pub matrix: HashMap<Id, BTreeSet<Id>>,
}

impl InterferenceMatrix {
    pub fn add(&mut self, x: Id, y: Id) {
        if x == y { return; }
        if !self.matrix.contains_key(&x) { self.matrix.insert(x, BTreeSet::new()); }
        if !self.matrix.contains_key(&y) { self.matrix.insert(y, BTreeSet::new()); }

        self.matrix.get_mut(&x).unwrap().insert(y);
        self.matrix.get_mut(&y).unwrap().insert(x);
    }

    pub fn new() -> Self {
        Self { matrix: HashMap::new() }
    }

    pub fn build(&mut self, cfg: &CFG, liveness: &Liveness) {

        for block in 0..cfg.len() {
            let mut lives = liveness.live_out[block].clone();

            for instr in cfg[block].stmt.iter().rev() {

                if let Some(dest) = instr.destination() {
                    lives.iter().for_each(|x| self.add(*x, dest));
                    lives.remove(&dest);
                }

                lives.extend(instr.operands());
            }
        }
    }

    pub fn conflict(&self, x: Id, y: Id) -> bool {
        if !self.matrix.contains_key(&x) { return false; }
        return self.matrix[&x].contains(&y);
    }
}
