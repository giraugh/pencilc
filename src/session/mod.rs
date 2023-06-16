use std::{rc::Rc, sync::RwLock};

use crate::id::{StringId, SymbolId};

use self::intern::InternPool;

mod intern;

pub type SessionRef<'a> = Rc<RwLock<Session<'a>>>;

pub struct Session<'a> {
    pub input: &'a str,
    symbols: InternPool<SymbolId, String>,
    strings: InternPool<StringId, String>,
}

impl<'a> Session<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            symbols: Default::default(),
            strings: Default::default(),
        }
    }

    pub fn get_string(&self, id: StringId) -> Option<&String> {
        self.strings.get(id)
    }

    pub fn get_symbol(&self, id: SymbolId) -> Option<&String> {
        self.symbols.get(id)
    }

    pub fn intern_kw(&mut self, id: SymbolId, value: String) {
        self.symbols.intern_reserved(value, id);
    }

    pub fn intern_string(&mut self, value: String) -> StringId {
        self.strings.intern(value)
    }

    pub fn intern_symbol(&mut self, value: String) -> SymbolId {
        self.symbols.intern(value)
    }
}
