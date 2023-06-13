use std::{rc::Rc, sync::RwLock};

use self::intern::InternPool;

mod intern;
pub use intern::{StringID, SymbolID};

pub type SessionRef<'a> = Rc<RwLock<Session<'a>>>;

pub struct Session<'a> {
    pub input: &'a str,
    symbols: InternPool<intern::SymbolID, String>,
    strings: InternPool<intern::StringID, String>,
}

impl<'a> Session<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            symbols: Default::default(),
            strings: Default::default(),
        }
    }

    pub fn get_string(&self, id: StringID) -> Option<&String> {
        self.strings.get(id)
    }

    pub fn get_symbol(&self, id: SymbolID) -> Option<&String> {
        self.symbols.get(id)
    }

    pub fn intern_kw(&mut self, id: SymbolID, value: String) {
        self.symbols.intern_reserved(value, id);
    }

    pub fn intern_string(&mut self, value: String) -> StringID {
        self.strings.intern(value)
    }

    pub fn intern_symbol(&mut self, value: String) -> SymbolID {
        self.symbols.intern(value)
    }
}
