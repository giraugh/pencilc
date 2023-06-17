mod interned;

use crate::id::{Idx, StringId};
pub use interned::Interned;
use std::{collections::HashMap, hash::Hash, rc::Rc, sync::RwLock};

pub trait Intern: Hash + Eq + Clone {
    type Id: Idx + std::fmt::Debug;
}

impl Intern for String {
    type Id = StringId;
}

pub type InternPoolRef<T> = Rc<RwLock<InternPool<T>>>;

#[derive(Debug)]
pub struct InternPool<T: Intern> {
    ids: HashMap<T, T::Id>,
    interns: HashMap<T::Id, T>,
    next_id: T::Id,
}

impl<T: Intern> Default for InternPool<T> {
    fn default() -> Self {
        Self {
            ids: Default::default(),
            interns: Default::default(),
            next_id: Default::default(),
        }
    }
}

pub trait InternPoolExt<T: Intern> {
    fn intern_reserved(&mut self, value: T, id: T::Id);
    fn intern(&mut self, value: T) -> Interned<T>;
    fn get(&self, id: T::Id) -> Option<T>;
}

impl<T: Intern> InternPoolExt<T> for InternPoolRef<T> {
    fn intern_reserved(&mut self, value: T, id: T::Id) {
        let mut w = self.try_write().unwrap();
        w.ids.insert(value.clone(), id);
        w.interns.insert(id, value);
        w.next_id = id.plus(1);
    }

    fn intern(&mut self, value: T) -> Interned<T> {
        let existing_id = self.try_read().unwrap().ids.get(&value).map(|i| *i);
        let id = match existing_id {
            Some(id) => id,
            None => {
                // Get next id
                let mut w = self.try_write().unwrap();
                let id = w.next_id.next();

                // Update
                w.ids.insert(value.clone(), id);

                // Save backwards-mapping
                w.interns.insert(id, value);

                // Return id
                id
            }
        };

        Interned {
            id,
            pool: self.clone(),
        }
    }

    #[allow(unused)]
    fn get(&self, id: T::Id) -> Option<T> {
        self.try_write().unwrap().interns.get(&id).cloned()
    }
}
