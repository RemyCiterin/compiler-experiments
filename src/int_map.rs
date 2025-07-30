#[derive(Clone)]
pub struct IntMap<T, V> where T: Copy + Into<usize> {
    inverse: Vec<Option<usize>>,
    queue: Vec<(T, V)>
}

impl<T, V> IntMap<T, V> where T: Copy + Into<usize> {
    pub fn new() -> Self {
        Self{inverse: vec![], queue: vec![]}
    }

    pub fn len(&self) -> usize {
        return self.queue.len();
    }

    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, (T, V)> {
        self.queue.iter()
    }

    pub fn swap(&mut self, x: usize, y: usize) {
        self.queue.swap(x, y);

        self.inverse[self.queue[x].0.into()] = Some(x);
        self.inverse[self.queue[y].0.into()] = Some(y);
    }

    pub fn contain(&self, x: T) -> bool {
        x.into() < self.inverse.len() && self.inverse[x.into()].is_some()
    }

    pub fn insert(&mut self, x: T, v: V) {
        if self.contain(x) {
            self.queue[self.inverse[x.into()].unwrap()].1 = v;
            return;
        }

        if x.into() >= self.inverse.len() {
            self.inverse.resize(x.into()+1, None);
        }

        self.inverse[x.into()] = Some(self.queue.len());
        self.queue.push((x, v));
    }

    pub fn remove(&mut self, x: T) {
        if !self.contain(x) {return;}

        let end = self.queue.len() - 1;
        self.swap(end, self.inverse[x.into()].unwrap());
        self.inverse[x.into()] = None;
        self.queue.pop();
    }
}

use std::ops::Index;
use std::ops::IndexMut;

impl<T, V> Index<T> for IntMap<T, V> where T: Copy + Into<usize> {
    type Output = V;

    fn index(&self, x: T) -> &V {
        &self.queue[self.inverse[x.into()].unwrap()].1
    }
}

impl<T, V> IndexMut<T> for IntMap<T, V> where T: Copy + Into<usize> {
    fn index_mut(&mut self, x: T) -> &mut V {
        &mut self.queue[self.inverse[x.into()].unwrap()].1
    }
}

#[cfg(test)]
mod test_int_map {
    use crate::utils::int_map::*;

    #[test]
    fn test_int_map() {
        let mut map: IntMap<usize, usize> = IntMap::new();

        for i in 0..1000 {
            map.insert(i, i);
        }

        for i in 0..500 {
            assert!(map[i] == i);
            map.remove(i);
        }

        for i in 0..500 {
            map.insert(i, i);
        }

        for i in 0..1000 {
            assert!(map[i] == i);
            map.remove(i);
        }
    }
}
