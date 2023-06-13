use crate::error::LexError;
use crate::session::{self, SessionRef, StringID, SymbolID};
use crate::span::{CharPos, Span};
use std::fmt::Debug;
use strum::IntoEnumIterator;
use strum_macros::Display;

use self::Delimeter::*;
use self::TokenKind::*;
use super::kw::Kw;
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
    Str(StringID),
    Int(u64),
    Float(f64),
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum TokenKind {
    /// End of file
    EOF,

    /// Identifier
    Ident(SymbolID),

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
    session: SessionRef<'a>,
}

impl<'a> TokenLexer<'a> {
    pub fn new(session: SessionRef<'a>) -> Self {
        // Load keywords into session
        {
            let mut session = session.try_write().unwrap();
            for kw in Kw::iter() {
                let kw_str = kw.as_ref().to_owned();
                let kw_id = kw.into();
                session.intern_kw(kw_id, kw_str);
            }
        }

        // Get input from session
        let input = session.read().unwrap().input;

        Self {
            session,
            position: CharPos(0),
            cursor: Cursor::new(input),
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            match self.next_token() {
                Ok(token) => {
                    if token.kind == TokenKind::EOF {
                        break;
                    } else {
                        tokens.push(token)
                    }
                }
                Err(err) => return Err(err),
            }
        }
        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        loop {
            let basic_token = self.cursor.next_token();
            let start = self.position;
            self.position = self.position + CharPos(basic_token.len);
            let span = Span::new(start, self.position.clone());

            let mut session = self.session.try_write().unwrap();
            let value_str = &session.input[span.start.0..span.end.0];

            let token_kind = match basic_token.kind {
                // Skip comments and whitespace
                BasicTokenKind::LineComment => continue,
                BasicTokenKind::Whitespace => continue,

                // Throw error for unknown tokens
                BasicTokenKind::Unknown => return Err(LexError::UnknownTokenError),

                // Literals
                BasicTokenKind::Literal { kind } => match kind {
                    LiteralKind::Int => Literal(LiteralValue::Int(value_str.parse()?)),
                    LiteralKind::Float => Literal(LiteralValue::Float(value_str.parse()?)),
                    LiteralKind::Str { terminated } => {
                        if terminated {
                            let string_id = session.intern_string(value_str.to_owned());
                            Literal(LiteralValue::Str(string_id))
                        } else {
                            return Err(LexError::MalformedStringError);
                        }
                    }
                },

                BasicTokenKind::Ident => Ident(session.intern_symbol(value_str.to_owned())),

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
