use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Id(usize);

impl From<usize> for Id {
    fn from(x: usize) -> Id {
        return Id(x);
    }
}

impl From<Id> for usize {
    fn from(x: Id) -> usize {
        return x.0;
    }
}



impl fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
