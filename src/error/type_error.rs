use crate::{
    ast,
    session::symbol::Symbol,
    tyc::{ty::Ty, InferValueKind},
};

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Unknown type name {0:?}")]
    UnknownType(Symbol),

    #[error("Unknown identifier {0:?}")]
    UnknownIdent(Symbol),

    #[error("Parameter name {0:?} is repeated in function signature")]
    RepeatedParameterName(ast::Binding),

    #[error("Expected parameter to have a type indication")]
    MissingParameterType(ast::Binding),

    #[error("The type {1:?} is not assignable to an {0:?} type variable.")]
    CannotUnifyPrimitive(InferValueKind, Ty),

    #[error("The type {0} is not unifiable with a {1} type.")]
    CannotUnify(Ty, Ty),

    #[error("Expected function {0:?} to return {1} rather than {2}")]
    ExpectedReturnType(Symbol, Ty, Ty),

    #[error("Cant determine type")]
    AmbiguousType,
}
