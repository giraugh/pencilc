use crate::lex::{Token, TokenKind};

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,

    #[error("Unexpected token {actual:?} expected {expected}")]
    ExpectedToken { expected: TokenKind, actual: Token },

    #[error("Unexpected token {0:?}")]
    UnexpectedToken(Token),
}
