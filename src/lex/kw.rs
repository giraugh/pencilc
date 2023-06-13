use strum_macros::{AsRefStr, EnumIter};

use crate::session::SymbolID;

#[derive(Clone, Debug, PartialEq, Eq, EnumIter, AsRefStr)]
pub enum Kw {
    #[strum(serialize = "fn")]
    Fn,

    #[strum(serialize = "let")]
    Let,

    #[strum(serialize = "return")]
    Return,
}

impl From<Kw> for SymbolID {
    fn from(value: Kw) -> Self {
        SymbolID::new((value as isize) as u64)
    }
}
