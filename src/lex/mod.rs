mod basic;
mod cursor;
mod lexer;

use self::lexer::{SyntaxError, Token, TokenKind, TokenLexer};

pub fn tokenize(input: &str) -> impl Iterator<Item = Result<Token, SyntaxError>> + '_ {
    let mut lexer = TokenLexer::new(input);
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
