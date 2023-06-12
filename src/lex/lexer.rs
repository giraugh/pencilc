use crate::error::LexError;
use crate::session::{self, Session};
use crate::span::{CharPos, Span};
use std::fmt::Debug;

use self::Delimeter::*;
use self::TokenKind::*;
use super::{basic::BasicTokenKind, basic::LiteralKind, cursor::Cursor};

#[derive(Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.kind, self.span)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LiteralValue {
    Str(session::StringID),
    Int(u64),
    Float(f64),
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    /// End of file
    EOF,

    /// Identifier
    Ident(session::SymbolID),

    /// Literal
    Literal(LiteralValue),

    /// Opening a delimeter
    OpenDelimeter(Delimeter),

    /// Closing a delimeter
    CloseDelimeter(Delimeter),

    // Single characters
    Semi,
    Comma,
    Dot,
    Bang,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Eq,
    Lt,
    Gt,
    Minus,
    Plus,
    Ampersand,
    Pipe,
    Asterisk,
    Slash,
    Caret,
    Percent,
    Colon,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd)]
pub enum Delimeter {
    Parenthesis,
    Bracket,
    Brace,
}

pub struct TokenLexer<'a> {
    position: CharPos,
    cursor: Cursor<'a>,
    session: &'a mut Session<'a>,
}

impl<'a> TokenLexer<'a> {
    pub fn new(session: &'a mut Session<'a>) -> Self {
        let input = session.input;
        Self {
            session,
            position: CharPos(0),
            cursor: Cursor::new(input),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        loop {
            let basic_token = self.cursor.next_token();
            let start = self.position;
            self.position = self.position + CharPos(basic_token.len);
            let span = Span::new(start, self.position.clone());

            let value_str = &self.session.input[span.start.0..span.end.0];

            let token_kind = match basic_token.kind {
                // Skip comments and whitespace
                BasicTokenKind::LineComment => continue,
                BasicTokenKind::Whitespace => continue,

                // Throw error for unknown tokens
                BasicTokenKind::Unknown => return Err(LexError::UnknownTokenError),

                // Literals
                BasicTokenKind::Literal { kind } => match kind {
                    LiteralKind::Str { terminated } => {
                        if terminated {
                            let string_id = self.session.intern_string(value_str.to_owned());
                            Literal(LiteralValue::Str(string_id))
                        } else {
                            return Err(LexError::MalformedStringError);
                        }
                    }

                    // TODO: raise value error instead of this unwrap
                    LiteralKind::Int => Literal(LiteralValue::Int(value_str.parse()?)),

                    // TODO: raise value error instead of this unwrap
                    LiteralKind::Float => Literal(LiteralValue::Float(value_str.parse()?)),
                },

                // TODO: intern idents
                BasicTokenKind::Ident => Ident(self.session.intern_symbol(value_str.to_owned())),

                // We compact delimeters by the delimeter type
                BasicTokenKind::OpenParen => OpenDelimeter(Parenthesis),
                BasicTokenKind::CloseParen => CloseDelimeter(Parenthesis),
                BasicTokenKind::OpenBrace => OpenDelimeter(Brace),
                BasicTokenKind::CloseBrace => CloseDelimeter(Brace),
                BasicTokenKind::OpenBracket => OpenDelimeter(Bracket),
                BasicTokenKind::CloseBracket => CloseDelimeter(Bracket),

                // These are the same
                BasicTokenKind::EOF => EOF,
                BasicTokenKind::Semi => Semi,
                BasicTokenKind::Comma => Comma,
                BasicTokenKind::Bang => Bang,
                BasicTokenKind::Dot => Dot,
                BasicTokenKind::Colon => Colon,
                BasicTokenKind::Eq => Eq,
                BasicTokenKind::Lt => Lt,
                BasicTokenKind::Gt => Gt,
                BasicTokenKind::Minus => Minus,
                BasicTokenKind::Plus => Plus,
                BasicTokenKind::Ampersand => Ampersand,
                BasicTokenKind::Pipe => Pipe,
                BasicTokenKind::Asterisk => Asterisk,
                BasicTokenKind::Slash => Slash,
                BasicTokenKind::Caret => Caret,
                BasicTokenKind::Percent => Percent,
            };

            return Ok(Token {
                kind: token_kind,
                span,
            });
        }
    }
}
