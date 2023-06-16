use crate::{
    ast,
    id::SymbolId,
    tyc::ty::{InferenceTyKind, PrimitiveTy},
};

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Unknown type name {0:?}")]
    UnknownType(SymbolId),

    #[error("Parameter name {0:?} is repeated in function signature")]
    RepeatedParameterName(ast::Binding),

    #[error("Expected parameter to have a type indication")]
    MissingParameterType(ast::Binding),

    #[error("The type {1:?} is not assignable to an {0:?} type.")]
    CannotUnifyPrimitive(InferenceTyKind, PrimitiveTy),
}
