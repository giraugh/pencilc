use crate::{id::SymbolId, lex::Kw};

use super::unify::TyInferVar;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// A primitive built-in type identified with a keyword
    Primitive(PrimitiveTy),

    /// An inference (type) variable that represents an unknown type
    Infer(TyInferVar, InferenceTyKind),

    /// A value used only for function returns that indicates it won't return
    Never,
}

/// A type variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferenceTyKind {
    /// A type variable unifiable with anything
    General,

    /// A type variable unifiable with integer types
    Integral,
}

impl InferenceTyKind {
    pub fn can_unify_with(&self, prim: &PrimitiveTy) -> bool {
        match (self, prim) {
            (&InferenceTyKind::General, _) => true,
            (&InferenceTyKind::Integral, PrimitiveTy::Int | PrimitiveTy::UInt) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralTy {
    /// "some number type"
    /// Could resolve to PrimitiveTy::Int or PrimitiveTy::Uint
    Number,

    /// "some string type"
    /// in reality, this can only be PrimitiveTy::Str
    Str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveTy {
    /// A signed 32 bit integer
    Int,

    /// An unsigned 32 bit integer
    UInt,

    /// A 32 bit float
    Float,

    /// An owned string
    Str,
}

impl From<SymbolId> for Option<PrimitiveTy> {
    fn from(value: SymbolId) -> Self {
        match value {
            i if i == Kw::Int.into() => Some(PrimitiveTy::Int),
            i if i == Kw::Float.into() => Some(PrimitiveTy::Float),
            i if i == Kw::Str.into() => Some(PrimitiveTy::Str),

            _ => None,
        }
    }
}
