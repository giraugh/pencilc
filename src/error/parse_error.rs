use crate::lex::TokenKind;

#[allow(unused)]
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,

    #[error("Unexpected token {actual} expected {expected}")]
    ExpectedToken {
        expected: TokenKind,
        actual: TokenKind,
    },

    #[error("Unexpected token {0}")]
    UnexpectedToken(TokenKind),

    #[error("Blocks may only return once")]
    MultipleReturns,
}
