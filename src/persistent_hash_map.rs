//! Define a type of (pseudo) persistent hash map, such that we can insert/remove/lookup the map
//! like a basic one, but we register all the changes such that we can `push` and `pop` new levels.
//!
//! The only way to update an element is via insert/remove, this map doesn't have a `get_mut` or
//! `iter_mut` method...

use std::collections::HashMap;

pub struct PHashMap<K, V> {
    /// Map allowing to lookup for any key
    map: HashMap<K, V>,

    /// Contains all the changes from at the current level,
    /// each times we insert/remove something in the map
    /// we save the old value to undo the changes at the
    /// corresponding `pop` call
    added: Vec<(K, Option<V>)>,

    /// Contains the size of the `added` queue at each level,
    /// to known where to go back in case of a `pop`
    sizes: Vec<usize>,
}

impl<K: std::hash::Hash+std::cmp::Eq,V> std::ops::Index<&K> for PHashMap<K,V> {
    type Output = V;

    fn index(&self, idx: &K) -> &V {
        &self.map[idx]
    }
}

impl<K: std::hash::Hash+std::cmp::Eq+Clone,V: Clone + std::cmp::Eq> PHashMap<K,V> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            sizes: vec![],
            added: vec![],
        }
    }

    pub fn push(&mut self) {
        self.sizes.push(self.added.len());
    }

    pub fn pop(&mut self) {
        let len = self.sizes.pop().unwrap();

        while self.added.len() > len {
            let (key, old_value) = self.added.pop().unwrap();

            match old_value {
                Some(v) => _ = self.map.insert(key, v),
                None => _ = self.map.remove(&key),
            }
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        let result = self.map.insert(key.clone(), value.clone());

        if result != Some(value) {
            self.added.push((key, result.clone()));
        }

        result
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let result = self.map.remove(key);

        if result.is_some() {
            self.added.push((key.clone(), result.clone()));
        }

        result
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<K, V> {
        self.map.iter()
    }
}
