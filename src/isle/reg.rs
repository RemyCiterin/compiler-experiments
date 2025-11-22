use crate::ssa::{Var};

/// Physical register definition
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct Phys(pub usize);

pub const LEN: usize = 4;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub struct PhysSet(pub [u64; LEN]);

impl PhysSet {
    pub fn new() -> Self {
        Self([0;LEN])
    }

    pub fn empty() -> Self {Self::new()}

    pub fn insert(&mut self, phys: Phys) {
        self.0[phys.0 / 64] |= 1 << (phys.0 % 64);
    }

    pub fn singleton(phys: Phys) -> Self {
        let mut set = Self::empty();
        set.0[phys.0 / 64] = 1 << (phys.0 % 64);
        set
    }

    pub fn contains(&self, phys: Phys) -> bool {
        let b = self.0[phys.0 / 64] >> (phys.0 % 64);
        (b & 1) != 0
    }

    pub fn union(&mut self, other: Self) {
        for i in 0..LEN {
            self.0[i] |= other.0[i];
        }
    }

    pub fn intersection(&mut self, other: Self) {
        for i in 0..LEN {
            self.0[i] &= other.0[i];
        }
    }

    pub fn difference(&mut self, other: Self) {
        for i in 0..LEN {
            self.0[i] &= !other.0[i];
        }
    }
}

impl FromIterator<Phys> for PhysSet {
    fn from_iter<T>(iter: T) -> Self where T: IntoIterator<Item=Phys> {
        let mut set = Self::new();
        for x in iter { set.insert(x); }
        set
    }
}

impl std::iter::Iterator for PhysSet {
    type Item = Phys;

    fn next(&mut self) -> Option<Phys> {
        for i in 0..LEN {
            if self.0[i] != 0 {
                let x = self.0[i].ilog2() as u64;
                self.0[i] &= !(1 << x);
                return Some(Phys(x as usize + 64*i));
            }
        }

        None
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub enum Reg {
    Phys(Phys),
    Virt(Var),
}

impl Reg {
    pub fn as_virt(&self) -> Option<Var> {
        match self {
            Self::Virt(virt) => Some(*virt),
            _ => None
        }
    }

    pub fn as_virt_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Virt(virt) => Some(virt),
            _ => None
        }
    }

    pub fn unwrap_virt(&self) -> Var {
        self.as_virt().unwrap()
    }

    pub fn as_phys(&self) -> Option<Phys> {
        match self {
            Self::Phys(phys) => Some(*phys),
            _ => None
        }
    }

    pub fn as_phys_mut(&mut self) -> Option<&mut Phys> {
        match self {
            Self::Phys(phys) => Some(phys),
            _ => None
        }
    }

    pub fn unwrap_phys(&self) -> Phys {
        self.as_phys().unwrap()
    }
}

impl std::fmt::Display for Phys {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.0)
    }
}
