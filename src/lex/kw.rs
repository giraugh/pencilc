use strum_macros::{AsRefStr, EnumIter};

use crate::id::SymbolId;

#[derive(Clone, Debug, PartialEq, Eq, EnumIter, AsRefStr)]
pub enum Kw {
    #[strum(serialize = "fn")]
    Fn,

    #[strum(serialize = "let")]
    Let,

    #[strum(serialize = "return")]
    Return,

    #[strum(serialize = "int")]
    Int,

    #[strum(serialize = "uint")]
    UInt,

    #[strum(serialize = "float")]
    Float,

    #[strum(serialize = "str")]
    Str,

    #[strum(serialize = "bool")]
    Bool,

    #[strum(serialize = "true")]
    True,

    #[strum(serialize = "false")]
    False,
}

impl From<Kw> for SymbolId {
    fn from(value: Kw) -> Self {
        ((value as isize) as usize).into()
    }
}
