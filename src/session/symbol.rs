use crate::{id::SymbolId, lex::Kw};

use super::intern::{Intern, Interned};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SymbolStr(String);

pub type Symbol = Interned<SymbolStr>;

impl Symbol {
    pub fn is_kw(&self, kw: Kw) -> bool {
        let symb_id = kw.into();
        self.id() == symb_id
    }
}

impl Intern for SymbolStr {
    type Id = SymbolId;
}

impl From<String> for SymbolStr {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl std::fmt::Display for SymbolStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
