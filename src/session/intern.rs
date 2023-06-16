use std::{collections::HashMap, hash::Hash};

use crate::id::Idx;

pub trait InternValue: Hash + Eq + Clone {}
impl InternValue for String {}

pub struct InternPool<ID: Idx, Intern: InternValue> {
    ids: HashMap<Intern, ID>,
    interns: HashMap<ID, Intern>,
    next_id: ID,
}

impl<ID: Idx, Intern: InternValue> Default for InternPool<ID, Intern> {
    fn default() -> Self {
        Self {
            ids: Default::default(),
            interns: Default::default(),
            next_id: Default::default(),
        }
    }
}

impl<ID: Idx, Intern: InternValue> InternPool<ID, Intern> {
    pub fn intern_reserved(&mut self, value: Intern, id: ID) {
        self.ids.insert(value.clone(), id);
        self.interns.insert(id, value);
        self.next_id = id.plus(1);
    }

    pub fn intern(&mut self, value: Intern) -> ID {
        let entry = self.ids.entry(value.clone()).or_insert_with(|| {
            // Get next id
            let id = self.next_id.next();

            // Save backwards-mapping
            self.interns.insert(id, value);

            // Return id
            id
        });
        *entry
    }

    #[allow(unused)]
    pub fn get(&self, id: ID) -> Option<&Intern> {
        self.interns.get(&id)
    }
}
