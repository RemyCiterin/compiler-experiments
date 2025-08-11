use crate::ast::*;
use crate::ssa::*;
use slotmap::*;

use crate::*;


use std::collections::{BTreeSet, HashMap};

pub struct InstCombine {
    /// SSA graph of the current RTL representation
    graph: SparseSecondaryMap<Var, BTreeSet<InstrId>>,

    /// Worklist
    worklist: Vec<InstrId>,
}

impl InstCombine {
    pub fn new<I: Instruction>(cfg: &Cfg<I>) -> Self {
        let mut worklist = vec![];

        for block in cfg.preorder() {
            for i in 0..cfg[block].stmt.len() {
                worklist.push((block,i));
            }
        }

        Self {
            worklist,
            graph: cfg.ssa_graph(),
        }
    }

    pub fn run<I: Instruction, F>(&mut self, cfg: &mut Cfg<I>, mut f: F)
    where F: FnMut(&Cfg<I>, &mut I) {

        while let Some(id) = self.worklist.pop() {
            let mut instr: I = cfg[id].clone();

            // Simplify the current instruction
            f(cfg, &mut instr);
            let progress = &instr != &cfg[id];
            if !progress {continue;}


            // Remove the old definition of the instruction from the SSA graph
            for x in cfg[id].operands() {
                self.graph[x].remove(&id);
            }

            // Add the new definition of the instruction to the SSA graph
            for x in instr.operands() {
                self.graph[x].insert(id);
            }

            // Propagate the instruction using the worklist
            if let Some(dest) = instr.destination() {
                for &x in self.graph[dest].iter() {
                    self.worklist.push(x);
                }
            }

            // Update the control flow graph with the new instruction
            cfg.set_instr(id.0, id.1, instr);
        }
    }
}

/// A type of pattern to detect a pattern in a control from graph:
/// let `instr` and instruction in a control flow graph `cfg`, then we can construct a pattern as a
/// s-expression to detect if `instr` match the pattern in the context f the cfg, this pattern can
/// use operations like:
///     - a binop with a pattern of the form `(binop p1 p2)`
///     - an unop with a pattern of the form `(unop p)`
///     - a literal of any kind with `i` where `i` is an integer representing a variable in the
///     pattern, then it is possible to extract the value of `i` in the current expression using
///     the field `lits` in the resulting `Occurence` map
///     - an integer variable represented using `(int i)` where `i` is an integer representing a
///     variable in the pattern, then it is possible to extract the value of `i` in the current
///     expression using the field `ints` in the resulting `Occurence` map
///     - an integer constant using `(cst i)`
///     - a stack slot variable using `(slot i)` (we get `i` using the `slots` field)
///     - a symbol address variable using `(addr i)` (we get `i` using the `addrs` field)
///
/// As example with the pattern `pattern!( ( Sub (cst 0) (Add (int 0) 0) ) )` and the following
/// C.F.G.:
///     ```asm
///         v0 := ...
///         v1 := 7 + v0
///         v2 := 0 - v1
///     ```
/// `search_pattern(p, &cfg, &Instr::Binop(_, Binop::Sub, Lit::Int(0), Lit::Var(v0)))` must return
/// the following occurence map:
/// ```rust
/// Some(Occurence{ lits: { 0 : v0 }, ints: { 0: 7 }, slots: {}, addrs: {} })
/// ```
///
pub enum Pattern {
    /// An variable of kind integer
    Int(usize),

    /// An variable of kind address
    Addr(usize),

    /// A variable of kind stack slot
    Stack(usize),

    /// An unary operation
    Unop(Unop, Box<Pattern>),

    /// A binary operation
    Binop(Binop, Box<Pattern>, Box<Pattern>),

    /// A variable of any kind
    Leaf(usize),

    /// A constant integer
    Const(i32),
}

#[macro_export]
macro_rules! pattern {
    ( ( cst $e:expr ) ) => {
        crate::instcombine::Pattern::Const($e)
    };
    ( (int $i:literal)  ) => { crate::instcombine::Pattern::Int( $i ) };
    ( (addr $i:literal)  ) => { crate::instcombine::Pattern::Addr( $i ) };
    ( (slot $i:literal)  ) => { crate::instcombine::Pattern::Stack( $i ) };
    ( ($u:ident $p:tt) ) => {
        crate::instcombine::Pattern::Unop( unop!($u), Box::new(pattern!($p)) )
    };
    ( ($b:ident $p1:tt $p2:tt ) ) => {
        crate::instcombine::Pattern::Binop(
            binop!($b),
            Box::new(pattern!($p1)),
            Box::new(pattern!($p2))
        )
    };
    ( $i:literal ) => { crate::instcombine::Pattern::Leaf( $i ) };
}

pub struct Occurence {
    pub lits: HashMap<usize, Lit>,
    pub ints: HashMap<usize, i32>,
    pub slots: HashMap<usize, Slot>,
    pub addrs: HashMap<usize, String>,
}

pub fn search_pattern(pat: Pattern, cfg: &Cfg<Instr>, instr: &Instr) -> Option<Occurence> {
    let mut occ = Occurence::new();
    if occ.search_instr(cfg, &pat, instr) {
        Some(occ)
    } else {
        None
    }
}

impl Occurence {
    pub fn new() -> Self {
        Self {
            lits: HashMap::new(),
            ints: HashMap::new(),
            slots: HashMap::new(),
            addrs: HashMap::new(),
        }
    }

    pub fn search_lit(&mut self, cfg: &Cfg<Instr>, pattern: &Pattern, lit: &Lit) -> bool {
        match (pattern, lit) {
            (Pattern::Const(c), Lit::Int(x)) => {
                x == c
            }
            (Pattern::Int(i), Lit::Int(x)) => {
                if self.ints.contains_key(&i) && &self.ints[&i] != x { return false; }
                self.ints.insert(*i, *x);
                true
            }
            (Pattern::Addr(i), Lit::Addr(x)) => {
                if self.addrs.contains_key(&i) && &self.addrs[&i] != x { return false; }
                self.addrs.insert(*i, x.clone());
                true
            }
            (Pattern::Stack(i), Lit::Stack(x)) => {
                if self.slots.contains_key(&i) && &self.slots[&i] != x { return false; }
                self.slots.insert(*i, *x);
                true
            }
            (Pattern::Leaf(i), _) => {
                if self.lits.contains_key(&i) && &self.lits[&i] != lit { return false; }
                self.lits.insert(*i, lit.clone());
                true
            }
            (_, Lit::Var(var)) => {
                let VarKind::Local(label, pos) = cfg[*var] else { return false; };
                let instr = &cfg[(label,pos)];
                self.search_instr(cfg, pattern, instr)
            }
            _ => false
        }
    }

    pub fn search_instr(&mut self, cfg: &Cfg<Instr>, pattern: &Pattern, instr: &Instr) -> bool {
        match (pattern, instr) {
            //(_, Instr::Move(_, lit)) => self.search_lit(cfg, pattern, lit),
            (Pattern::Leaf(i), _) => {
                let x = Lit::Var(instr.destination().unwrap());
                if self.lits.contains_key(&i) && self.lits[&i] != x { return false; }
                self.lits.insert(*i, x);
                true
            }
            (Pattern::Unop(unop1, p), Instr::Unop(_, unop2, lit)) => {
                if unop1 != unop2 { return false; }
                self.search_lit(cfg, p, lit)
            }
            (Pattern::Binop(b1, p1, p2), Instr::Binop(_, b2, l1, l2)) => {
                if b1 != b2 { return false; }
                self.search_lit(cfg, p1, l1) && self.search_lit(cfg, p2, l2)
            }
            _ => false
        }
    }
}


pub fn combine_generic_instr(cfg: &Cfg<Instr>, instr: &mut Instr) {
    let def_var = |x: Var| {
        let VarKind::Local(label, pos) = cfg[x] else { return None; };
        Some(&cfg[(label, pos)])
    };

    let def_lit = |lit: &Lit| {
        let Lit::Var(x) = lit else { return None; };
        def_var(*x)
    };

    // Constant propagation
    for lit in instr.literals_mut() {
        let Some(Instr::Move(_, l)) = def_lit(lit) else { continue; };
        *lit = l.clone();
    }

    // Canonicalize commutative binop
    if let Instr::Binop(dest, binop, l1, l2) = &instr
        && binop.commutative() {
        match (l1, l2) {
            (Lit::Var(_), _) => {}
            (_, Lit::Var(_)) => *instr = Instr::Binop(*dest, *binop, l2.clone(), l1.clone()),
            _ => {}
        }
    }

    // Constant folding on binary operation
    if let Instr::Binop(dest, binop, Lit::Int(i1), Lit::Int(i2)) = &instr {
        *instr = Instr::Move(*dest, Lit::Int(binop.eval(*i1, *i2)));
    }

    let mut pat = pattern!( ( Sub 0 0 ) );
    if let Some(_) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), Lit::Int(0));
    }

    pat = pattern!( ( Xor 0 0 ) );
    if let Some(_) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), Lit::Int(0));
    }

    pat = pattern!( ( Xor 0 (cst 0) ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Unop(instr.destination().unwrap(), Unop::Not, occ.lits[&0].clone());
    }

    pat = pattern!( ( Xor (cst 0) 0 ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Unop(instr.destination().unwrap(), Unop::Not, occ.lits[&0].clone());
    }

    pat = pattern!( ( Sub (cst 0) 0 ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Unop(instr.destination().unwrap(), Unop::Neg, occ.lits[&0].clone());
    }

    pat = pattern!( ( Add 0 (cst 0) ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), occ.lits[&0].clone());
    }

    pat = pattern!( ( Add (cst 0) 0 ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), occ.lits[&0].clone());
    }

    pat = pattern!( ( Sub 0 (cst 0) ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), occ.lits[&0].clone());
    }

    pat = pattern!( ( Neg ( Neg 0 ) ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), occ.lits[&0].clone());
    }

    pat = pattern!( ( Not ( Not 0 ) ) );
    if let Some(occ) = search_pattern(pat, cfg, &instr) {
        *instr = Instr::Move(instr.destination().unwrap(), occ.lits[&0].clone());
    }

    // Constant folding on unary operation
    if let Instr::Unop(dest, unop, Lit::Int(i)) = &instr {
        *instr = Instr::Move(*dest, Lit::Int(unop.eval(*i)));
    }

    // Change `if x != 0 ...` into `if x`
    if let Instr::Branch(x, l1, l2) = instr.clone() {
        let Some(ins) = def_lit(&x) else { return; };

        pat = pattern!( ( NotEqual 0 (cst 0) ) );
        if let Some(occ) = search_pattern(pat, cfg, &ins) {
            *instr = Instr::Branch(occ.lits[&0].clone(), l1, l2);
        }

        pat = pattern!( ( Equal 0 (cst 0) ) );
        if let Some(occ) = search_pattern(pat, cfg, &ins) {
            *instr = Instr::Branch(occ.lits[&0].clone(), l2, l1);
        }
    }
}
