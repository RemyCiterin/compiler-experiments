//! this module define a type of Trie and some tool to use it

pub type Map<K, V> = std::collections::HashMap<K, V>;
pub type MapIter<'a, K, V> = std::collections::hash_map::Iter<'a, K, V>;

#[derive(Debug, Clone)]
struct TrieUnmanaged {
    /// the length of the sequences in the trie
    length: usize,

    /// the number of sequences in the trie, must be strictly positive
    size: usize,

    /// the prefix of all the sequences in the trie
    prefix: Vec<usize>,

    /// the set of sequences in the trie,
    /// `seq in self <--> seq == prefix ++ [x] ++ next && next in children[x]`
    /// must be of size different to one
    children: Map<usize, Self>,
}

impl TrieUnmanaged {
    fn new(seq: &[usize]) -> Self {
        Self {
            size: 1,
            length: seq.len(),
            prefix: Vec::from(seq),
            children: Map::new(),
        }
    }

    /// return the contatenation of a prefix and the list of sequences represented by a trie
    fn all_sequences(&self, prefix: &Vec<usize>, out: &mut Vec<Vec<usize>>) {
        let mut prefix = prefix.clone();
        prefix.extend_from_slice(&self.prefix);

        if self.children.len() == 0 {
            out.push(prefix);
            return;
        }

        for (&item, next) in &self.children {
            prefix.push(item);
            next.all_sequences(&prefix, out);
            prefix.pop();
        }
    }

    /// return if a trie contain a sequence that start by a given prefix
    pub fn contains(&self, prefix: &[usize]) -> bool {
        let len = self.prefix.len();

        for idx in 0..len {
            if idx == prefix.len() {return true;}
            if prefix[idx] != self.prefix[idx] {return false;}
        }

        if prefix.len() == len {return true;}

        if let Some(next) = self.children.get(&prefix[len]) {
            return next.contains(&prefix[len+1..]);
        }

        return false;
    }

    /// insert a sequence in a trie, return true if an insertion is done
    pub fn insert(&mut self, key: &[usize]) -> bool {
        assert!(key.len() == self.length);
        let len = self.prefix.len();

        for idx in 0..len {
            if key[idx] == self.prefix[idx] {continue;}

            let node1 = Self {
                size: 1,
                length:self.length - idx - 1,
                prefix: Vec::from(&key[idx+1..]),
                children: Map::new(),
            };


            let node2 = Self {
                size: self.size,
                length: self.length - idx - 1,
                prefix: Vec::from(&self.prefix[idx+1..]),
                children: std::mem::take(&mut self.children),
            };


            self.children.insert(key[idx], node1);
            self.children.insert(self.prefix[idx], node2);
            self.prefix.resize(idx, 0);
            self.size += 1;
            return true;
        }

        if key.len() == len {return false;}

        if let Some(next) = self.children.get_mut(&key[len]) {
            let is_insert = next.insert(&key[len+1..]);
            if is_insert {self.size += 1;}
            return is_insert;
        }

        self.children.insert(
            key[len],
            Self::new(&key[len+1..]),
        );
        self.size += 1;
        return true;
    }

    /// remove all the sequences that start by a given prefix and return the number of removed
    /// sequences
    pub fn remove(&mut self, prefix: &[usize]) -> usize {
        let len = self.prefix.len();

        for idx in 0..len {
            if prefix.len() == idx {return self.size;}

            if prefix[idx] != self.prefix[idx] {return 0;}
        }

        if prefix.len() == len {return self.size;}

        if let Some(next) = self.children.get_mut(&prefix[len]) {
            let init_next_size = next.size;
            let removed_size = next.remove(&prefix[len+1..]);

            self.size -= removed_size;
            if removed_size == init_next_size {
                self.children.remove(&prefix[len]);
            }

            if self.children.len() == 1 {
                let (&id, _) = self.children.iter().last().unwrap();
                let mut next = self.children.remove(&id).unwrap();

                self.prefix.push(id);
                self.prefix.append(&mut next.prefix);
                self.children = next.children;
            }

            return removed_size;
        }

        return 0;
    }

    fn view<'a>(&'a self) -> ViewUnmanaged<'a> {
        ViewUnmanaged{
            prefix: &self.prefix[0..],
            children: &self.children,
            length: self.length,
            size: self.size,
        }
    }
}

#[derive(Debug, Clone)]
struct ViewUnmanaged<'a> {
    /// the length of the sequences in the trie
    length: usize,

    /// the number of sequences in the trie, must be strictly positive
    size: usize,

    /// the prefix of all the sequences in the trie
    prefix: &'a [usize],

    /// the set of sequences in the trie,
    /// `seq in self <--> seq == prefix ++ [x] ++ next && next in children[x]`
    /// must be of size different to one
    children: &'a Map<usize, TrieUnmanaged>,
}

#[derive(Debug, Clone)]
enum ViewIteratorUnamanaged<'a> {
    Singleton(bool, usize, ViewUnmanaged<'a>),
    Multiple(MapIter<'a, usize, TrieUnmanaged>),
}

impl<'a> Iterator for ViewIteratorUnamanaged<'a> {
    type Item = (usize, ViewUnmanaged<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ViewIteratorUnamanaged::Singleton(seen, item, next) => {
                if *seen {return None;}
                *seen = true;

                Some((*item, next.clone()))
            },
            ViewIteratorUnamanaged::Multiple(iterator) => {
                match iterator.next() {
                    Some((&item, next)) => Some((item, next.view())),
                    None => None
                }
            }
        }
    }
}

impl<'a> ViewUnmanaged<'a> {
    /// return if the view contain a sequence that start by a given prefix
    fn contains(&self, prefix: &[usize]) -> bool {
        let len = self.prefix.len();

        for idx in 0..len {
            if idx == prefix.len() {return true;}
            if prefix[idx] != self.prefix[idx] {return false;}
        }

        if prefix.len() == len {return true;}

        if let Some(next) = self.children.get(&prefix[len]) {
            return next.contains(&prefix[len+1..]);
        }

        return false;
    }

    /// move the view by a given sequence
    fn look(&self, seq: &[usize]) -> Option<Self> {
        let len = self.prefix.len();

        for idx in 0..len {
            if idx == seq.len() {
                return Some(Self {
                    size: self.size,
                    length: self.length-idx,
                    children: self.children,
                    prefix: &self.prefix[idx..],
                });
            }

            if seq[idx] != self.prefix[idx] {return None;}
        }

        if seq.len() == len {
            return Some(Self {
                size: self.size,
                length: self.length-len,
                children: self.children,
                prefix: &self.prefix[len..],
            });
        }

        if let Some(next) = self.children.get(&seq[len]) {
            return next.view().look(&seq[len+1..]);
        }

        return None;
    }

    pub fn iter(&self) -> ViewIteratorUnamanaged<'a> {
        if self.prefix.len() > 0 {
            return ViewIteratorUnamanaged::Singleton(
                false,
                self.prefix[0],
                Self {
                    size: self.size,
                    length: self.length-1,
                    prefix: &self.prefix[1..],
                    children: self.children,
                },
            );
        }

        return ViewIteratorUnamanaged::Multiple(self.children.iter());
    }
}

#[derive(Debug, Clone)]
pub struct Trie {
    trie: Option<TrieUnmanaged>,
    length: usize,
}

#[derive(Debug, Clone)]
pub struct View<'a> {
    view: Option<ViewUnmanaged<'a>>,
    length: usize,
}

#[derive(Clone)]
pub struct ViewIterator<'a> {
    iterator: Option<ViewIteratorUnamanaged<'a>>,
}

impl<'a> Iterator for ViewIterator<'a> {
    type Item = (usize, View<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(iterator) = &mut self.iterator {
            if let Some((item, view)) = iterator.next() {
                Some((item, View{
                    length: view.length,
                    view: Some(view),
                }))
            } else {None}
        } else {None}
    }
}

impl Trie {
    /// construct a new trie
    pub fn new(length: usize) -> Self {
        Self {
            trie: None,
            length,
        }
    }

    /// return the list of sequences represented by a trie
    pub fn all_sequences(&self) -> Vec<Vec<usize>> {
        let mut out = vec![];
        if let Some(trie) = &self.trie {
            trie.all_sequences(&vec![], &mut out);
        }

        return out;
    }

    /// return the size of a trie
    pub fn len(&self) -> usize {
        if let Some(trie) = &self.trie {
            trie.size
        } else {0}
    }

    /// return the length of the sequences in a trie
    pub fn length(&self) -> usize {
        return self.length;
    }

    /// return if a trie contain a sequence that start by a given prefix
    pub fn contains(&self, prefix: &[usize]) -> bool {
        if let Some(trie) = &self.trie {
            trie.contains(prefix)
        } else {false}
    }

    /// insert a sequence in a trie
    pub fn insert(&mut self, key: &[usize]) {
        assert!(key.len() == self.length());
        if let Some(trie) = &mut self.trie {
            trie.insert(key);
            return;
        }

        self.trie = Some(TrieUnmanaged::new(key));
    }

    /// remove all the sequences of a trie that start by a given prefix
    pub fn remove(&mut self, prefix: &[usize]) {
        if let Some(trie) = &mut self.trie {
            let init_trie_size = trie.size;
            let removed_size = trie.remove(prefix);

            if init_trie_size == removed_size {self.trie = None;}
        }
    }

    /// return a view to a trie
    pub fn view<'a>(&'a self) -> View<'a> {
        if let Some(trie) = &self.trie {
            View {
                length: self.length,
                view: Some(trie.view()),
            }
        } else {
            return View{ length: self.length, view: None };
        }
    }
}

impl<'a> View<'a> {
    /// return if a view contain a sequence that start by a given prefix
    pub fn contains(&self, prefix: &[usize]) -> bool {
        if let Some(view) = &self.view {
            view.contains(prefix)
        } else {false}
    }

    /// return the size of a view
    pub fn len(&self) -> usize {
        if let Some(view) = &self.view {
            view.size
        } else {0}
    }

    /// return the length of the sequences in a trie
    pub fn length(&self) -> usize {
        return self.length;
    }

    /// move into a view by a given sequence
    pub fn look(&self, prefix: &[usize]) -> Self {
        if let Some(view) = &self.view {
            Self {
                view: view.look(prefix),
                length: self.length - prefix.len(),
            }
        } else {
            Self {
                view: None,
                length: self.length - prefix.len(),
            }
        }
    }

    /// iterate over a view
    pub fn iter(&self) -> ViewIterator<'a> {
        if let Some(view) = &self.view {
            ViewIterator{iterator: Some(view.iter())}
        } else {
            ViewIterator{iterator: None}
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::trie::*;

    #[test]
    fn unit_test() {
        let mut trie = Trie::new(4);
        trie.insert(&[1, 2, 3, 4]);
        trie.insert(&[1, 2, 3, 5]);
        trie.insert(&[1, 2, 3, 6]);
        trie.insert(&[1, 2, 4, 4]);
        trie.insert(&[1, 3, 3, 4]);

        assert!(trie.contains(&[1, 2, 3]));
        assert!(trie.contains(&[1, 2, 3, 4]));
        assert!(trie.contains(&[1, 2, 3, 5]));
        assert!(trie.contains(&[1, 2, 3, 6]));
        assert!(trie.contains(&[1, 2, 4, 4]));
        assert!(trie.contains(&[1, 3, 3, 4]));

        trie.remove(&[1, 2, 4]);
        assert!(trie.contains(&[1, 2, 3]));
        assert!(trie.contains(&[1, 2, 3, 4]));
        assert!(trie.contains(&[1, 2, 3, 5]));
        assert!(trie.contains(&[1, 2, 3, 6]));
        assert!(!trie.contains(&[1, 2, 4, 4]));
        assert!(trie.contains(&[1, 3, 3, 4]));

        trie.remove(&[1, 2, 3, 5]);
        assert!(trie.contains(&[1, 2, 3]));
        assert!(trie.contains(&[1, 2, 3, 4]));
        assert!(!trie.contains(&[1, 2, 3, 5]));
        assert!(trie.contains(&[1, 2, 3, 6]));
        assert!(!trie.contains(&[1, 2, 4, 4]));
        assert!(trie.contains(&[1, 3, 3, 4]));

        trie.remove(&[1, 3, 3, 4]);
        assert!(trie.contains(&[1, 2, 3]));
        assert!(trie.contains(&[1, 2, 3, 4]));
        assert!(!trie.contains(&[1, 2, 3, 5]));
        assert!(trie.contains(&[1, 2, 3, 6]));
        assert!(!trie.contains(&[1, 2, 4, 4]));
        assert!(!trie.contains(&[1, 3, 3, 4]));

        let mut view = trie.view();
        view = view.look(&[1, 2]);

        for (item, next) in view.iter() {
            println!("{:?} {:?}",item, next);
        }
    }

    #[test]
    fn bench() {
        use rand::prelude::*;

        let mut rng = rand::thread_rng();

        let num_seq = 100000;
        let seq_len = 10;
        let max_int = 10;

        //let mut map = std::collections::HashSet::<Vec<usize>>::new();
        let mut trie = Trie::new(seq_len);
        let mut sequences = vec![];

        for _ in 0..num_seq {
            let mut seq = vec![];
            for _ in 0..seq_len {
                seq.push(rng.r#gen::<usize>() % max_int);
            }

            trie.insert(&seq);
            //map.insert(seq.clone());
            sequences.push(seq);
        }

        for seq in sequences.iter() {
            assert!(trie.contains(seq));
            //assert!(map.contains(seq));
        }


    }
}
