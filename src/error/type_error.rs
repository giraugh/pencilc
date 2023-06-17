use crate::{
    ast,
    id::SymbolId,
    tyc::{ty::Ty, InferValueKind},
};

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Unknown type name {0:?}")]
    UnknownType(SymbolId),

    #[error("Unknown identifier {0:?}")]
    UnknownIdent(SymbolId),

    #[error("Parameter name {0:?} is repeated in function signature")]
    RepeatedParameterName(ast::Binding),

    #[error("Expected parameter to have a type indication")]
    MissingParameterType(ast::Binding),

    #[error("The type {1:?} is not assignable to an {0:?} type.")]
    CannotUnifyPrimitive(InferValueKind, Ty),

    #[error("The type {0:?} is not assignable to an {1:?} type.")]
    CannotUnify(Ty, Ty),

    #[error("Expected a type annotation here")]
    AnnotationRequired,

    #[error("Cant determine type")]
    AmbiguousType,
}
