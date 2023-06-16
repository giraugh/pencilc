use crate::{ast, id::SymbolId};

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Unknown type name {0:?}")]
    UnknownType(SymbolId),

    #[error("Parameter name {0:?} is repeated in function signature")]
    RepeatedParameterName(ast::Binding),

    #[error("Expected parameter to have a type indication")]
    MissingParameterType(ast::Binding),
}
