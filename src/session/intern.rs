use std::{collections::HashMap, hash::Hash};

/* Concrete Interns (unpaid) */

#[derive(Debug, Eq, Clone, Copy, PartialEq, Default)]
pub struct SymbolID(u64);

impl InternID for SymbolID {
    fn next_id(&self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Debug, Eq, Clone, Copy, PartialEq, Default)]
pub struct StringID(u64);

impl InternID for StringID {
    fn next_id(&self) -> Self {
        Self(self.0 + 1)
    }
}

impl InternValue for String {}

/* Intern Abstract */

pub trait InternValue: Hash + Eq {}
pub trait InternID: Default + Clone + Copy {
    fn next_id(&self) -> Self;
}

pub struct InternPool<ID: InternID, Intern: InternValue> {
    interns: HashMap<Intern, ID>,
    next_id: ID,
}

impl<ID: InternID, Intern: InternValue> Default for InternPool<ID, Intern> {
    fn default() -> Self {
        Self {
            interns: Default::default(),
            next_id: Default::default(),
        }
    }
}

impl<ID: InternID, Intern: InternValue> InternPool<ID, Intern> {
    pub fn intern(&mut self, value: Intern) -> ID {
        let entry = self.interns.entry(value).or_insert_with(|| {
            let id = self.next_id.clone();
            self.next_id = id.next_id();
            id
        });
        *entry
    }

    pub fn get(&mut self, id: ID) -> Option<Intern> {
        todo!()
    }
}
