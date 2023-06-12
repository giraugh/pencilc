use std::{collections::HashMap, hash::Hash};

/* Concrete Interns (unpaid) */

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct SymbolID(u64);

impl InternID for SymbolID {
    fn next_id(&self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default, Hash)]
pub struct StringID(u64);

impl InternID for StringID {
    fn next_id(&self) -> Self {
        Self(self.0 + 1)
    }
}

impl InternValue for String {}

/* Intern Abstract */

pub trait InternValue: Hash + Eq + Clone {}
pub trait InternID: Default + Clone + Copy + Hash + Eq {
    fn next_id(&self) -> Self;
}

pub struct InternPool<ID: InternID, Intern: InternValue> {
    ids: HashMap<Intern, ID>,
    interns: HashMap<ID, Intern>,
    next_id: ID,
}

impl<ID: InternID, Intern: InternValue> Default for InternPool<ID, Intern> {
    fn default() -> Self {
        Self {
            ids: Default::default(),
            interns: Default::default(),
            next_id: Default::default(),
        }
    }
}

impl<ID: InternID, Intern: InternValue> InternPool<ID, Intern> {
    pub fn intern(&mut self, value: Intern) -> ID {
        let entry = self.ids.entry(value.clone()).or_insert_with(|| {
            // Get next id
            let id = self.next_id.clone();
            self.next_id = id.next_id();

            // Save backwards-mapping
            self.interns.insert(id, value);

            // Return id
            id
        });
        *entry
    }

    #[allow(unused)]
    pub fn get(&mut self, id: ID) -> Option<&Intern> {
        self.interns.get(&id)
    }
}
