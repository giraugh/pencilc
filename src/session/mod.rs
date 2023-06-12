use self::intern::InternPool;

mod intern;
pub use intern::{StringID, SymbolID};

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

    pub fn intern_string(&mut self, value: String) -> StringID {
        self.strings.intern(value)
    }

    pub fn intern_symbol(&mut self, value: String) -> SymbolID {
        self.symbols.intern(value)
    }
}
