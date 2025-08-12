use crate::ast::*;
use crate::ssa::*;
use std::collections::HashMap;

/// A type to "pattern-match" a control from graph. Those patterns ca be built using a
/// language of s-expressions with rust macros:
/// ```
/// sexpr =
///     variable
///     | literal
///     | (int variable)
///     | (addr variable)
///     | (slot variable)
///     | (unop sexpr)
///     | (binop sexpr sexpr)
/// variable = x, y, z...
/// literal = 0,1,2,...
/// binop = And | Add | Xor | Or ...
/// unop = Neg | Not
/// ```
///
/// We can test it using the following program:
/// ```
/// let mut cfg = Cfg::new(true);
/// let v0 = cfg.fresh_arg();
/// let v1 = cfg.fresh_var();
/// let v2 = cfg.fresh_var();
///
/// let entry = vec![
///     Instr::Binop(v1, Binop::Add, Lit::Int(7), Lit::Var(v0)),
///     Instr::Return(Lit::Int(0)),
/// ];
///
/// cfg.set_block_stmt(cfg.entry(), entry);
///
/// let p = pattern!( ( Sub 0 (Add (int x) y) ) );
/// let res = search_pattern(p, &cfg, &Instr::Binop(v2, Binop::Sub, Lit::Int(0), Lit::Var(v1)));
///
/// let Some(occ) = res else { unreachable!() };
///
/// assert!( occ.lits["y"] == Lit::Var(v0) );
/// assert!( occ.ints["x"] == 7 );
/// ```
#[derive(Clone)]
pub enum Pattern {
    /// An variable of kind `integer`
    Int(String),

    /// An variable of kind `address`
    Addr(String),

    /// A variable of kind `stack slot`
    Stack(String),

    /// An unary operation
    Unop(Unop, Box<Pattern>),

    /// A binary operation
    Binop(Binop, Box<Pattern>, Box<Pattern>),

    /// A variable of any kind
    Leaf(String),

    /// A constant integer
    Const(i32),
}

#[macro_export]
macro_rules! declare_pattern_vars {
    ( $occ:ident, $e:literal ) => {};
    ( $occ:ident, ( int $i:ident) ) => {
        #[allow(unused_variables)]
        let $i: i32 = $occ.ints[stringify!($i)].clone();
    };
    ( $occ:ident, ( addr $i:ident) ) => {
        #[allow(unused_variables)]
        let $i: String = $occ.addrs[stringify!($i)].clone();
    };
    ( $occ:ident, ( slot $i:ident) ) => {
        #[allow(unused_variables)]
        let $i: Slot = $occ.slots[stringify!($i)].clone();
    };
    ( $occ:ident, $i:ident ) => {
        #[allow(unused_variables)]
        let $i: Lit = $occ.lits[stringify!($i)].clone();
    };
    ( $occ:ident, ( $u:ident $p:tt ) ) => {
        declare_pattern_vars!($occ, $p);
    };
    ( $occ:ident, ( $b:ident $p1:tt $p2:tt ) ) => {
        declare_pattern_vars!($occ, $p1);
        declare_pattern_vars!($occ, $p2);
    };
}

#[macro_export]
macro_rules! eval_pattern_vars {
    ( $eval:ident, $occ:ident, $e:literal ) => {};
    ( $eval:ident, $occ:ident, ( int $i:ident) ) => {
        #[allow(unused_variables)]
        let $i: i32 = $occ.ints[stringify!($i)].clone();
    };
    ( $eval:ident, $occ:ident, ( addr $i:ident) ) => {
        #[allow(unused_variables)]
        let $i: String = $occ.addrs[stringify!($i)].clone();
    };
    ( $eval:ident, $occ:ident, ( slot $i:ident) ) => {
        #[allow(unused_variables)]
        let $i: Var = $eval.eval_lit(Int::Stack($occ.slots[stringify!($i)].clone()));
    };
    ( $eval:ident, $occ:ident, $i:ident ) => {
        #[allow(unused_variables)]
        let $i: Var = $eval.eval_lit($occ.lits[stringify!($i)].clone());
    };
    ( $eval:ident, $occ:ident, ( $u:ident $p:tt ) ) => {
        eval_pattern_vars!($eval, $occ, $p);
    };
    ( $eval:ident, $occ:ident, ( $b:ident $p1:tt $p2:tt ) ) => {
        eval_pattern_vars!($eval, $occ, $p1);
        eval_pattern_vars!($eval, $occ, $p2);
    };
}

#[macro_export]
macro_rules! pattern {
    ( $e:literal ) => {
        crate::pattern::Pattern::Const($e)
    };
    ( (int $i:ident) ) => { crate::pattern::Pattern::Int( stringify!($i).to_string() ) };
    ( (addr $i:ident) ) => { crate::pattern::Pattern::Addr( stringify!($i).to_string() ) };
    ( (slot $i:ident) ) => { crate::pattern::Pattern::Stack( stringify!($i).to_string() ) };
    ( ($u:ident $p:tt) ) => {
        crate::pattern::Pattern::Unop( unop!($u), Box::new(pattern!($p)) )
    };
    ( ($b:ident $p1:tt $p2:tt ) ) => {
        crate::pattern::Pattern::Binop(
            binop!($b),
            Box::new(pattern!($p1)),
            Box::new(pattern!($p2))
        )
    };
    ( $i:ident ) => { crate::pattern::Pattern::Leaf( stringify!($i).to_string() ) };
}

/// A map from variables in a pattern to their concrete value if the C.F.G. in case of a match
pub struct Occurence {
    pub lits: HashMap<String, Lit>,
    pub ints: HashMap<String, i32>,
    pub slots: HashMap<String, Slot>,
    pub addrs: HashMap<String, String>,
}

/// Search a given pattern in a control flow graph, and return it's occurence (map from variables
/// to concrete values) in case of a match
pub fn search_pattern(pat: Pattern, cfg: &Cfg<Instr>, instr: &Instr) -> Option<Occurence> {
    let mut occ = Occurence::new();
    if occ.search_instr(cfg, &pat, instr) {
        Some(occ)
    } else {
        None
    }
}

/// Search a given pattern in a control flow graph, and return it's occurence (map from variables
/// to concrete values) in case of a match
pub fn search_pattern_in_lit(pat: Pattern, cfg: &Cfg<Instr>, lit: Lit) -> Option<Occurence> {
    let mut occ = Occurence::new();
    if occ.search_lit(cfg, &pat, &lit) {
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

    /// Try to match a literal with a given pattern, propagate the search in the control flow graph
    /// if the literal is a variable, and the pattern is a node
    pub fn search_lit(&mut self, cfg: &Cfg<Instr>, pattern: &Pattern, lit: &Lit) -> bool {
        match (pattern, lit) {
            (Pattern::Const(c), Lit::Int(x)) => {
                x == c
            }
            (Pattern::Int(i), Lit::Int(x)) => {
                if self.ints.contains_key(i) && &self.ints[i] != x { return false; }
                self.ints.insert(i.clone(), *x);
                true
            }
            (Pattern::Addr(i), Lit::Addr(x)) => {
                if self.addrs.contains_key(i) && &self.addrs[i] != x { return false; }
                self.addrs.insert(i.clone(), x.clone());
                true
            }
            (Pattern::Stack(i), Lit::Stack(x)) => {
                if self.slots.contains_key(i) && &self.slots[i] != x { return false; }
                self.slots.insert(i.clone(), *x);
                true
            }
            (Pattern::Leaf(i), _) => {
                if self.lits.contains_key(i) && &self.lits[i] != lit { return false; }
                self.lits.insert(i.clone(), lit.clone());
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

    /// Try to match a pattern with an instruction
    pub fn search_instr(&mut self, cfg: &Cfg<Instr>, pattern: &Pattern, instr: &Instr) -> bool {
        match (pattern, instr) {
            //(_, Instr::Move(_, lit)) => self.search_lit(cfg, pattern, lit),
            (Pattern::Leaf(i), _) => {
                let x = Lit::Var(instr.destination().unwrap());
                if self.lits.contains_key(i) && self.lits[i] != x { return false; }
                self.lits.insert(i.clone(), x);
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

