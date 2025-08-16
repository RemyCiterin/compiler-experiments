use crate::ssa::*;
use slotmap::*;

use crate::pattern::*;
use crate::{pattern, unop, binop, declare_pattern_vars};


use std::collections::{BTreeSet};

/// Combine instructions in a control flow graph, using a user-given single-instruction
/// simplification function, it maintains a worklist of the next instructions to explore, this pass
/// doesn't perform dead-code elimination, so we must call one such pass after this one.
///
/// As example with a very simple simplification function:
///
/// ```
/// fn simplify(cfg: &Cfg<Instr>, instr: &mut Instr) {
///     let pat = pattern!( ( Sub x x ) );
///     if let Some(_) = search_pattern(pat, cfg, &instr) {
///         *instr = Instr::Move(instr.destination().unwrap(), Lit::Int(0));
///     }
/// }
///
/// let mut cfg = Cfg::new(true);
/// let v0 = cfg.fresh_arg();
/// let v1 = cfg.fresh_var();
///
/// let entry = vec![
///     Instr::Binop(v1, Binop::Sub, v0, v0),
///     Instr::Return(v1),
/// ];
///
/// cfg.set_block_stmt(cfg.entry(), entry);
///
/// let mut combine = InstCombine::new(&cfg);
/// combine.run(&mut cfg, |cfg,ins| simplify(cfg,ins));
/// ```
///
/// The simplification pass must result in the following entry block:
/// ```
/// vec![
///     Instr::Move(v1, Lit::Int(0)),
///     Instr::Return(v1),
/// ]
/// ```
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

macro_rules! op_rule {
    ( $p:tt , $test:expr , $dest:ident =>  $transform:expr ) => {
        OpRule::new(
            pattern!($p),
            |occ| {declare_pattern_vars!(occ, $p); $test},
            |occ, v| {let $dest = v; declare_pattern_vars!(occ, $p); $transform}
        )
    }
}

macro_rules! cond_rule {
    ( $p:tt , $test:expr , $l1:ident $l2:ident =>  $transform:expr ) => {
        CondRule::new(
            pattern!($p),
            |occ| {declare_pattern_vars!(occ, $p); $test},
            |occ, l1, l2| {
                let $l1 = l1;
                let $l2 = l2;
                declare_pattern_vars!(occ, $p);
                $transform
            }
        )
    }
}


pub fn combine_instructions(cfg: &mut Cfg<Instr>) {
    let mut combine = InstCombine::new(cfg);

    let op_rules = vec![
        op_rule!(
            ( Sll x 1 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::Add, x.clone(), x)
        ),
        op_rule!(
            ( Sub x x ), true,
            dest => Instr::Move(dest, Lit::Int(0))
        ),
        op_rule!(
            ( Xor x 0 ), true,
            dest => Instr::Move(dest, x)
        ),
        op_rule!(
            ( Xor 0 x ), true,
            dest => Instr::Move(dest, x)
        ),
        op_rule!(
            ( Xor x x ), true,
            dest => Instr::Move(dest, Lit::Int(0))
        ),
        op_rule!(
            ( Add 0 x ), true,
            dest => Instr::Move(dest, x)
        ),
        op_rule!(
            ( Add x 0 ), true,
            dest => Instr::Move(dest, x)
        ),
        op_rule!(
            ( Sub x 0 ), true,
            dest => Instr::Move(dest, x)
        ),
        op_rule!(
            ( Neg ( Neg x ) ), true,
            dest => Instr::Move(dest, x)
        ),
        op_rule!(
            ( Not ( Not x ) ), true,
            dest => Instr::Move(dest, x)
        ),


        op_rule!(
            ( NotEqual 0 ( LessThan x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessThan, x, y)
        ),
        op_rule!(
            ( NotEqual 0 ( ULessThan x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessThan, x, y)
        ),
        op_rule!(
            ( NotEqual ( LessThan x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessThan, x, y)
        ),
        op_rule!(
            ( NotEqual ( ULessThan x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessThan, x, y)
        ),
        op_rule!(
            ( NotEqual 0 ( LessEqual x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessEqual, x, y)
        ),
        op_rule!(
            ( NotEqual 0 ( ULessEqual x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessEqual, x, y)
        ),
        op_rule!(
            ( NotEqual ( LessEqual x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessEqual, x, y)
        ),
        op_rule!(
            ( NotEqual ( ULessEqual x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessEqual, x, y)
        ),


        op_rule!(
            ( Equal 0 ( LessThan x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessEqual, y, x)
        ),
        op_rule!(
            ( Equal 0 ( ULessThan x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessEqual, y, x)
        ),
        op_rule!(
            ( Equal ( LessThan x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessEqual, y, x)
        ),
        op_rule!(
            ( Equal ( ULessThan x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessEqual, y, x)
        ),
        op_rule!(
            ( Equal 0 ( LessEqual x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessThan, y, x)
        ),
        op_rule!(
            ( Equal 0 ( ULessEqual x y ) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessThan, y, x)
        ),
        op_rule!(
            ( Equal ( LessEqual x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessThan, y, x)
        ),
        op_rule!(
            ( Equal ( ULessEqual x y ) 0 ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessThan, y, x)
        ),

        op_rule!(
            ( Sub 1 (Equal x y) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::NotEqual, x, y)
        ),
        op_rule!(
            ( Sub 1 (NotEqual x y) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::Equal, x, y)
        ),
        op_rule!(
            ( Sub 1 (LessEqual x y) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessThan, y, x)
        ),
        op_rule!(
            ( Sub 1 (ULessEqual x y) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessThan, y, x)
        ),
        op_rule!(
            ( Sub 1 (LessThan x y) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::LessEqual, y, x)
        ),
        op_rule!(
            ( Sub 1 (ULessThan x y) ), true,
            dest => Instr::Binop(dest, crate::ast::Binop::ULessEqual, y, x)
        ),
    ];

    // We don't have the trivial branch simplification here because we need to propagate the
    // information to the non-taken Phi's instructions otherwise. This simplification is done
    // in `copy_prop` instead.
    let cond_rules = vec![
        cond_rule!(
            ( NotEqual x 0 ), true,
            l1 l2 => Instr::Branch(x, l1, l2)
        ),
        cond_rule!(
            ( NotEqual x 0 ), true,
            l1 l2 => Instr::Branch(x, l1, l2)
        ),
        cond_rule!(
            ( Equal x 0 ), true,
            l1 l2 => Instr::Branch(x, l2, l1)
        ),
        cond_rule!(
            ( Equal x 0 ), true,
            l1 l2 => Instr::Branch(x, l2, l1)
        ),
    ];

    combine.run(
        cfg,
        |cfg, instr| {
            combine_from_patterns(cfg, instr, &op_rules, &cond_rules)
        });
}

pub struct OpRule<'a> {
    pub pattern: Pattern,
    pub test: Box<dyn Fn(&Occurence) -> bool + 'a>,
    pub transform: Box<dyn Fn(Occurence, Var) -> Instr + 'a>,
}

impl<'a> OpRule<'a> {
    pub fn new<F1, F2>(pattern: Pattern, test: F1, tr: F2) -> Self
        where F1: Fn(&Occurence) -> bool + 'a,
        F2: Fn(Occurence, Var) -> Instr + 'a {
        Self {
            pattern,
            test: Box::new(test),
            transform: Box::new(tr),
        }
    }

    pub fn pattern(&self) -> Pattern {
        self.pattern.clone()
    }

    pub fn test(&self, occ: &Occurence) -> bool {
        let test = &self.test;
        test(occ)
    }

    pub fn transform(&self, occ: Occurence, dest: Var) -> Instr {
        let transform = &self.transform;
        transform(occ, dest)
    }
}

pub struct CondRule<'a> {
    pub pattern: Pattern,
    pub test: Box<dyn Fn(&Occurence) -> bool + 'a>,
    pub transform: Box<dyn Fn(Occurence, Label, Label) -> Instr + 'a>,
}

impl<'a> CondRule<'a> {
    pub fn new<F1, F2>(pattern: Pattern, test: F1, tr: F2) -> Self
        where F1: Fn(&Occurence) -> bool + 'a,
        F2: Fn(Occurence, Label, Label) -> Instr + 'a {
        Self {
            pattern,
            test: Box::new(test),
            transform: Box::new(tr),
        }
    }

    pub fn pattern(&self) -> Pattern {
        self.pattern.clone()
    }

    pub fn test(&self, occ: &Occurence) -> bool {
        let test = &self.test;
        test(occ)
    }

    pub fn transform(&self, occ: Occurence, l1: Label, l2: Label) -> Instr {
        let transform = &self.transform;
        transform(occ, l1, l2)
    }
}

pub fn combine_from_patterns<'a>(
    cfg: &Cfg<Instr>,
    instr: &mut Instr,
    op_rules: &Vec<OpRule<'a>>,
    cond_rules: &Vec<CondRule<'a>>) {

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

    // Constant folding in case of a binary operation
    if let Instr::Binop(dest, binop, Lit::Int(i1), Lit::Int(i2)) = &instr {
        *instr = Instr::Move(*dest, Lit::Int(binop.eval(*i1, *i2)));
    }

    // Constant folding in case of an unary operation
    if let Instr::Unop(dest, unop, Lit::Int(i)) = &instr {
        *instr = Instr::Move(*dest, Lit::Int(unop.eval(*i)));
    }

    // Search for a rule for an operation
    for rule in op_rules.iter() {
        let result = search_pattern(rule.pattern(), cfg, instr);
        if let Some(occ) = result && rule.test(&occ) {
            *instr = rule.transform(occ, instr.destination().unwrap());
        }
    }

    // Search for a rule for a branch
    if let Instr::Branch(x, l1, l2) = instr.clone() {
        //let Some(ins) = def_lit(&x) else { return; };

        for rule in cond_rules.iter() {
            let result =
                search_pattern_in_lit(rule.pattern(), cfg, x.clone());
            if let Some(occ) = result && rule.test(&occ) {
                *instr = rule.transform(occ, l1, l2);
            }
        }
    }
}
