use inkwell::support::LLVMString;

use crate::id::NameId;

#[derive(Debug, thiserror::Error)]
pub enum CodegenError {
    #[error("llvm returned a verification error for module: {0}")]
    ModuleVerificationError(LLVMString),

    #[error("Cannot find ident {0:?} in named variables")]
    NoSuchNamedIdent(NameId),
}
