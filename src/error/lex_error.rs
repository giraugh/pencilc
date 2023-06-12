use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("unknown token")]
    UnknownTokenError,

    #[error("malformed string literal")]
    MalformedStringError,

    #[error("malformed int literal")]
    MalformedIntError(#[from] ParseIntError),

    #[error("malformed float literal")]
    MalformedFloatError(#[from] ParseFloatError),
}
