use crate::{id::SymbolId, lex::Kw};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveTy {
    /// A signed 32 bit integer
    Int,

    /// An unsigned 32 bit integer
    UInt,

    /// A 32 bit float
    Float,

    /// A boolean value
    Bool,

    /// An owned string
    Str,
}

impl From<SymbolId> for Option<PrimitiveTy> {
    fn from(value: SymbolId) -> Self {
        match value {
            i if i == Kw::Int.into() => Some(PrimitiveTy::Int),
            i if i == Kw::UInt.into() => Some(PrimitiveTy::UInt),
            i if i == Kw::Float.into() => Some(PrimitiveTy::Float),
            i if i == Kw::Str.into() => Some(PrimitiveTy::Str),

            _ => None,
        }
    }
}
