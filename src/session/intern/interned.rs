use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use super::{Intern, InternPoolRef};

#[derive(Clone)]
pub struct Interned<T: Intern> {
    pub(super) id: T::Id,
    pub(super) pool: InternPoolRef<T>,
}

impl<T: Intern + Display> Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use super::InternPoolExt;
        write!(f, "`{}`", self.pool.get(self.id).unwrap())
    }
}

impl<T: Intern + Display> Display for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use super::InternPoolExt;
        write!(f, "{}", self.pool.get(self.id).unwrap())
    }
}

impl<T: Intern> Interned<T> {
    pub fn id(&self) -> T::Id {
        self.id
    }
}

impl<T: Intern> Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: Intern> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T: Intern> Eq for Interned<T> {}
