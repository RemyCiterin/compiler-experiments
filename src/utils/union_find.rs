//! this file contain an implementation of union find
//! algorithm with eager path compression

// use crate::identifiers::Id;
use slotmap::*;

pub struct UnionFind<Id: Key> {
    /// contain the root of each node
    root: SecondaryMap<Id, Id>,

    /// contain the next element of in the iterator for each node
    next: SecondaryMap<Id, Id>,

    /// contain the size of the equivalence class of each root node
    size: SecondaryMap<Id, usize>,
}

impl<Id: Key> UnionFind<Id> {
    pub fn new() -> Self {
        Self {
            size: SecondaryMap::new(),
            next: SecondaryMap::new(),
            root: SecondaryMap::new(),
        }
    }

    pub fn insert(&mut self, id: Id) {
        self.next.insert(id, id);
        self.root.insert(id, id);
        self.size.insert(id, 1);
    }

    pub fn len(&self) -> usize {
        self.root.len()
    }

    pub fn find(&self, id: Id) -> Id {
        self.root[id]
    }

    pub fn next(&self, id: Id) -> Id {
        self.next[id]
    }

    pub fn size(&self, id: Id) -> usize {
        self.size[self.find(id)]
    }

    #[inline]
    fn swap_next(&mut self, lhs: Id, rhs: Id) {
        let tmp = self.next[lhs];
        self.next[lhs] = self.next[rhs];
        self.next[rhs] = tmp;
    }

    /// set `new_root` as a root of all the nodes of the equivalence class of `old_root`.
    /// Ensures that if `[new_root] ++ new_iter` and `[old_root] ++ old_iter` is the iterator
    /// of the input nodes, then `[new_root] ++ new_iter ++ [old_root] ++ old_iter` is an iterator
    /// of the equivalence class of `new_root` at output
    pub fn merge(&mut self, new_root: Id, old_root: Id) {
        assert!(self.find(new_root) == new_root);
        assert!(self.find(old_root) == old_root);
        assert!(new_root != old_root);

        let mut size = 0;
        let mut id = old_root;
        loop {
            self.root[id] = new_root;
            size += 1;

            id = self.next(id);
            if id == old_root {break;}
        }

        self.swap_next(old_root, new_root);
        self.size[new_root] += size;

        assert!(self.find(new_root) == new_root);
        assert!(self.find(old_root) == new_root);
    }

    /// unmerge the equivalence class of two nodes:
    /// If `[new_root] ++ new_iter ++ [old_root] ++ old_iter` is the iterator of the equivalence
    /// class of `new_root`, then `[new_root] ++ new_iter` and `[old_root] ++ old_iter` are the
    /// iterator of the equivalence class of `new_root` and `old_root` after calling `unmerge`.
    /// In particular apply `merge` then `unmerge` with the same arguments do nothing
    pub fn unmerge(&mut self, new_root: Id, old_root: Id) {
        assert!(self.find(new_root) == new_root);
        assert!(self.find(old_root) == new_root);
        assert!(new_root != old_root);

        self.swap_next(old_root, new_root);

        let mut size = 0;
        let mut id = old_root;
        loop {
            self.root[id] = old_root;
            size += 1;

            id = self.next(id);
            if id == old_root {break;}
        }

        self.size[new_root] -= size;
        self.size[old_root] = size;

        assert!(self.find(new_root) == new_root);
        assert!(self.find(old_root) == old_root);
    }

    pub fn iter<'a>(&'a self, id: Id) -> UFIterator<'a, Id> {
        UFIterator {
            fuel: self.size(id),
            current: id,
            uf: self,
        }
    }
}

pub struct UFIterator<'a, Id: Key> {
    fuel: usize,
    current: Id,
    uf: &'a UnionFind<Id>,
}

impl<'a, Id: Key> Iterator for UFIterator<'a, Id> {
    type Item = Id;

    fn next(&mut self) -> Option<Id> {
        if self.fuel == 0 {return None;}

        let id = self.current;
        self.current = self.uf.next(id);
        self.fuel -= 1;
        return Some(id);
    }
}
