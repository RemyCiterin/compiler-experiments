
#[derive(Clone)]
pub struct IntSet<T> where T: Into<usize> + Copy {
    queue: Vec<T>,
    inverse: Vec<Option<usize>>
}

impl<T> IntSet<T> where T: Into<usize> + Copy {
    pub fn new() -> Self {
        IntSet{ queue: vec![], inverse: vec![] }
    }

    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, T> {
        return self.queue.iter();
    }

    pub fn len(&self) -> usize {
        self.queue.len()
    }

    pub fn clear(&mut self) {
        for x in self.queue.iter() {
            self.inverse[x.clone().into()] = None;
        }

        self.queue.clear();
    }

    fn swap(&mut self, i: usize, j: usize) {
        self.queue.swap(i, j);

        self.inverse[self.queue[i].into()] = Some(i);
        self.inverse[self.queue[j].into()] = Some(j);
    }

    pub fn contains(&self, x: T) -> bool {
        x.into() < self.inverse.len() && self.inverse[x.into()].is_some()
    }

    pub fn choose(&self) -> Option<&T> {
        return self.queue.last();
    }

    pub fn insert(&mut self, x: T) {
        if self.contains(x) {return;}

        if self.inverse.len() <= x.into() {
            self.inverse.resize(x.into()+1, None);
        }

        self.inverse[x.into()] = Some(self.queue.len());
        self.queue.push(x);
    }

    pub fn remove(&mut self, x: T) {
        if !self.contains(x) {return;}

        let end = self.queue.len() - 1;
        self.swap(end, self.inverse[x.into()].unwrap());
        self.inverse[x.into()] = None;
        self.queue.pop();
    }

}
