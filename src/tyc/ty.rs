use std::fmt::Display;

use crate::{lex::Kw, session::symbol::Symbol};

use super::unify::TyInferVar;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// A primitive built-in type identified with a keyword
    Primitive(PrimitiveTy),

    /// An inference (type) variable that represents an unknown type
    Infer(TyInferVar),

    /// A value used only for function returns that indicates it won't return
    Never,
}

#[derive(Debug, strum_macros::Display, Clone, PartialEq, Eq)]
pub enum PrimitiveTy {
    /// A signed 32 bit integer
    #[strum(serialize = "int")]
    Int,

    /// An unsigned 32 bit integer
    #[strum(serialize = "uint")]
    UInt,

    /// A 32 bit float
    #[strum(serialize = "float")]
    Float,

    /// A boolean value
    #[strum(serialize = "bool")]
    Bool,

    /// An owned string
    #[strum(serialize = "str")]
    Str,
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::id::Idx;
        use Ty::*;

        match self {
            Primitive(p) => write!(f, "{}", p),
            Infer(var) => write!(f, "Infer<{}>", var.index()),
            Never => write!(f, "!"),
        }
    }
}

impl From<Symbol> for Option<PrimitiveTy> {
    fn from(value: Symbol) -> Self {
        match value {
            i if i.is_kw(Kw::Int) => Some(PrimitiveTy::Int),
            i if i.is_kw(Kw::UInt) => Some(PrimitiveTy::UInt),
            i if i.is_kw(Kw::Float) => Some(PrimitiveTy::Float),
            i if i.is_kw(Kw::Str) => Some(PrimitiveTy::Str),

            _ => None,
        }
    }
}
