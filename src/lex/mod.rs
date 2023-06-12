mod basic;
mod cursor;
mod lexer;

use crate::{error::SyntaxError, session::Session};
pub use lexer::{Token, TokenKind, TokenLexer};

pub fn tokenize<'a>(
    session: &'a mut Session<'a>,
) -> impl Iterator<Item = Result<Token, SyntaxError>> + 'a {
    let mut lexer = TokenLexer::new(session);
    std::iter::from_fn(move || match lexer.next_token() {
        Ok(token) => {
            if token.kind == TokenKind::EOF {
                None
            } else {
                Some(Ok(token))
            }
        }
        Err(err) => Some(Err(err)),
    })
}
