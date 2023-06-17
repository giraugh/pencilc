use std::{rc::Rc, sync::RwLock};

use crate::id::{StringId, SymbolId};

use self::{
    intern::{InternPoolExt, InternPoolRef, Interned},
    symbol::{Symbol, SymbolStr},
};

pub mod intern;
pub mod symbol;

pub type SessionRef<'a> = Rc<RwLock<Session<'a>>>;

pub struct Session<'a> {
    pub input: &'a str,
    symbols: InternPoolRef<SymbolStr>,
    strings: InternPoolRef<String>,
}

impl<'a> Session<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            symbols: Default::default(),
            strings: Default::default(),
        }
    }

    #[allow(unused)]
    pub fn get_string(&self, id: StringId) -> Option<String> {
        self.strings.get(id)
    }

    #[allow(unused)]
    pub fn get_symbol(&self, id: SymbolId) -> Option<SymbolStr> {
        self.symbols.get(id)
    }

    pub fn intern_kw(&mut self, id: SymbolId, value: String) {
        self.symbols.intern_reserved(value.into(), id);
    }

    pub fn intern_string(&mut self, value: String) -> Interned<String> {
        self.strings.intern(value)
    }

    pub fn intern_symbol(&mut self, value: String) -> Symbol {
        self.symbols.intern(value.into())
    }
}
