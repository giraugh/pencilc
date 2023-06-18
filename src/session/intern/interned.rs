use super::{Intern, InternPoolExt, InternPoolRef};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Clone)]
pub struct Interned<T: Intern> {
    pub(super) id: T::Id,
    pub(super) pool: InternPoolRef<T>,
}

impl<T: Intern + Display> Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}`", self.get())
    }
}

impl<T: Intern + Display> Display for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl<T: Intern> Interned<T> {
    pub fn id(&self) -> T::Id {
        self.id
    }

    pub fn get(&self) -> T {
        self.pool.get(self.id).unwrap()
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
