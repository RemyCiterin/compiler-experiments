use crate::dominance::*;
use crate::ssa::*;

use std::collections::{HashSet, HashMap};

pub struct LoopFinder {
    seen: HashSet<Label>,
    allowed: HashSet<Label>,
    cache: HashMap<Label, bool>,
    exit: HashSet<Label>,
    header: Label,
}

impl LoopFinder {
    pub fn new(header: Label) -> Self {
        Self {
            allowed: HashSet::new(),
            cache: HashMap::new(),
            seen: HashSet::new(),
            exit: HashSet::new(),
            header,
        }
    }

    /// Return if we can reach the header from a given block, and write the result in `self.cache`
    pub fn progress<Op: Operation, Cond: Condition>
        (&mut self, cfg: &Cfg<Op, Cond>, block: Label) -> bool {
        if let Some(result) = self.cache.get(&block) { return *result; }
        if !self.allowed.contains(&block) { return false; }
        if self.seen.contains(&block) { return false; }
        self.seen.insert(block);

        let mut found = false;

        for x in cfg[block].succs() {
            if x == self.header { found = true; continue; }
            found |= self.progress(cfg, x);
        }

        self.cache.insert(block, found);
        found
    }

    pub fn search_exit<Op: Operation, Cond: Condition>(&mut self, cfg: &Cfg<Op, Cond>) {
        for block in self.allowed.iter().copied() {
            if self.cache[&block] {
                for x in cfg[block].succs() {
                    if self.cache.get(&x) != Some(&true) {
                        self.exit.insert(x);
                    }
                }
            }
        }
    }

    /// Return the loop that start by `self.header` (if one exist) as a set of labels
    pub fn run<Op: Operation, Cond: Condition>
        (&mut self, cfg: &Cfg<Op, Cond>, dom: &Dominance) -> HashSet<Label> {
        let mut worklist: Vec<Label> = vec![self.header];

        while let Some(label) = worklist.pop() {
            self.allowed.insert(label);

            for &x in dom.childrens(label).iter() {
                worklist.push(x);
            }
        }

        self.progress(cfg, self.header);
        self.search_exit(cfg);

        self.allowed.iter().cloned()
            .filter(|l| self.cache[l])
            .collect()
    }
}

pub struct LoopSimplifier {
    cycle: HashSet<Label>,
    header: Label,
}

impl LoopSimplifier {
    pub fn new<Op: Operation, Cond: Condition>
        (header: Label, cfg: &Cfg<Op, Cond>, dom: &Dominance) -> Self {
        let mut finder = LoopFinder::new(header);
        let cycle = finder.run(cfg, dom);

        Self {
            cycle,
            header
        }
    }

    pub fn run<Op: Operation, Cond: Condition>
        (&mut self, cfg: &mut Cfg<Op, Cond>, dom: &Dominance) -> bool {
        // A variable is defined in a loop if it is a destination of one of it's instruction
        let mut defined: HashSet<Var> = HashSet::new();

        for &l in self.cycle.iter() {
            for instr in cfg[l].stmt.iter() {
                if let Some(dest) = instr.destination() {
                    defined.insert(dest);
                }
            }
        }

        let mut progress: bool = false;
        let mut moved: Vec<Instr<Op, Cond>> = vec![];

        for &block in self.cycle.iter() {
            let mut stmt: Vec<Instr<Op, Cond>> = vec![];

            for instr in cfg[block].stmt.iter().cloned() {
                // Phi(...) is the only instruction that depends of variables
                // undefined at the idom of the block, so we don't add them
                let can_move =
                    instr.operands().into_iter().all(|v| !defined.contains(&v))
                    && !matches!(instr, Instr::Phi(..))
                    && !instr.may_have_side_effect();

                if can_move {
                    progress = true;
                    moved.push(instr);
                } else {
                    stmt.push(instr);
                }
            }

            if stmt.len() == cfg[block].stmt.len() { continue; }
            cfg.set_block_stmt(block, stmt);
        }

        let block = dom.idom(self.header);
        let mut stmt: Vec<Instr<Op, Cond>> = vec![];

        for instr in cfg[block].stmt.iter().cloned() {
            if instr.exit_block() {
                stmt.extend(moved.iter().cloned());
            }

            stmt.push(instr);
        }

        cfg.set_block_stmt(block, stmt);

        progress
    }
}

pub fn licm<Op: Operation, Cond: Condition>(cfg: &mut Cfg<Op, Cond>) {
    let mut dom = Dominance::new(cfg);
    dom.run(cfg);

    for header in cfg.labels() {
        if !dom.reachable(header) { continue; }

        let mut simplifier = LoopSimplifier::new(header, cfg, &dom);

        if !simplifier.cycle.is_empty() {
            while simplifier.run(cfg, &dom) {}
        }
    }
}
