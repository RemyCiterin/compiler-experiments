use std::collections::HashSet;
use crate::ssa::*;
use slotmap::*;

pub struct Dominance {
    /// index of each block in postorder
    index: SecondaryMap<Label, usize>,

    /// reverse postorder of the blocks
    preorder: Vec<Label>,

    /// dominance relation, as a map frmo node to parent
    dom: SecondaryMap<Label, Option<Label>>,

    /// dominance relation as a map from parent to childrens
    childrens: SecondaryMap<Label, HashSet<Label>>,

    frontier: SecondaryMap<Label, HashSet<Label>>,
}

impl Dominance {
    pub fn new<I: Instruction>(cfg: &Cfg<I>) -> Self {
        // Generate a postorder using a topo sort
        let mut order: Vec<Label> = cfg.postorder();
        let mut index: SecondaryMap<Label, usize> = SecondaryMap::new();
        let mut dom: SecondaryMap<Label, Option<Label>> = SecondaryMap::new();
        let mut frontier: SecondaryMap<Label, HashSet<Label>> = SecondaryMap::new();
        let mut childrens: SecondaryMap<Label, HashSet<Label>> = SecondaryMap::new();

        for i in 0..order.len() {
            frontier.insert(order[i], HashSet::new());
            childrens.insert(order[i], HashSet::new());
            index.insert(order[i], i);
            dom.insert(order[i], None);
        }

        order.reverse();

        Self {
            index,
            frontier,
            childrens,
            preorder: order,
            dom,
        }
    }

    pub fn run_dom<I: Instruction>(&mut self, cfg: &Cfg<I>) {
        self.dom[cfg.entry()] = Some(cfg.entry());

        loop {
            let mut progress: bool = false;

            for &block in self.preorder.iter() {
                let mut new_idom: Option<Label> = None;

                // assign the intersection of the processed predecessors
                for &pred in cfg.preds(block).iter() {
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

        for &block in self.preorder.iter() {
            if self.dom[block].is_some() && self.dom[block] != Some(block) {
                self.childrens[self.dom[block].unwrap()].insert(block);
            }
        }
    }

    pub fn run_frontier<I: Instruction>(&mut self, cfg: &Cfg<I>) {
        let blocks = self.preorder.clone();

        for block in blocks {
            if !self.reachable(block) { continue; }
            if cfg.preds(block).len() > 1 && self.reachable(block) {
                for &pred in cfg.preds(block).iter() {
                    let mut runner = pred;

                    while runner != self.idom(block) && self.reachable(runner) {
                        self.frontier[runner].insert(block);
                        runner = self.idom(runner);
                    }
                }
            }
        }
    }

    pub fn run<I: Instruction>(&mut self, cfg: &Cfg<I>) {
        self.run_dom(cfg);
        self.run_frontier(cfg);
    }

    /// Return the least commun ancestor of two nodes in the current dominance tree
    pub fn intersect(&self, mut x: Label, mut y: Label) -> Label {
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
    pub fn reachable(&self, block: Label) -> bool {
        self.dom.contains_key(block) && self.dom[block].is_some()
    }

    /// return the immediate dominator of a block
    pub fn idom(&self, block: Label) -> Label {
        self.dom[block].unwrap()
    }

    pub fn childrens(&self, block: Label) -> &HashSet<Label> {
        &self.childrens[block]
    }

    /// Return the dominance frontier of a block
    /// The dominance frontier of a node X is the set of nodes
    /// Y such that X dominate a predecessor of Y, but X does not
    /// strictly dominate Y
    pub fn frontier(&self, block: Label) -> &HashSet<Label> {
        &self.frontier[block]
    }
}
