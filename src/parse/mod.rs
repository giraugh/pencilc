mod ast;

use crate::{error::ParseError, lex::TokenLexer};

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: TokenLexer<'a>,
}

impl Parser<'_> {
    fn parse_function(&mut self) -> Result<ast::Node> {
        // We expect a `fn` keyword
        todo!()
    }
}
