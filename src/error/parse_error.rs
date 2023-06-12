use super::SyntaxError;

#[allow(unused)]
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Syntax error while parsing")]
    SyntaxError(SyntaxError),
}
