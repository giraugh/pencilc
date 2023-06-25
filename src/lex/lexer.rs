use crate::error::LexError;
use crate::session::intern::Interned;
use crate::session::symbol::Symbol;
use crate::session::SessionRef;
use crate::span::{CharPos, Span};
use std::fmt::Debug;
use std::iter::Peekable;
use strum::IntoEnumIterator;
use strum_macros::Display;

use self::Delimeter::*;
use self::TokenKind::*;
use super::basic::CursorIterator;
use super::kw::Kw;
use super::{basic::BasicTokenKind, basic::LiteralKind, cursor::Cursor};

#[derive(Clone, PartialEq)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Str(Interned<String>),
    Int(u64),
    Float(f64),
    Bool(bool),
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Display)]
pub enum TokenKind {
    /// End of file
    EOF,

    /// Identifier
    Ident(Symbol),

    /// Literal
    Literal(LiteralValue),

    /// Opening a delimeter
    OpenDelimeter(Delimeter),

    /// Closing a delimeter
    CloseDelimeter(Delimeter),

    // Ligatures
    EqEq,
    GtEq,
    LtEq,
    FatArrow,
    ThinArrow,
    AmpersandAmpersand,
    PipePipe,
    BangEq,

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
    basic_tokens: Peekable<CursorIterator<'a>>,
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
            basic_tokens: Cursor::new(input).into_iter().peekable(),
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

    pub fn maybe_ligature(&mut self, ligature: Result<TokenKind, TokenKind>) -> TokenKind {
        match ligature {
            Ok(ligature) => {
                let basic_token = self.basic_tokens.next().expect("Iter is infinite");
                self.position = self.position + CharPos(basic_token.len);
                ligature
            }
            Err(token) => token,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        loop {
            let basic_token = self.basic_tokens.next().expect("Iter is infinite");
            let start = self.position;
            self.position = self.position + CharPos(basic_token.len);

            // Get kind of next basic token
            let next_basic = self
                .basic_tokens
                .peek()
                .expect("Iter is infinite")
                .kind
                .clone();

            let value_str = {
                let session = self.session.try_write().unwrap();
                session.input[start.0..start.0 + basic_token.len].to_owned()
            };

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
                            let string_id = self
                                .session
                                .try_write()
                                .unwrap()
                                .intern_string(value_str.to_owned());
                            Literal(LiteralValue::Str(string_id))
                        } else {
                            return Err(LexError::MalformedStringError);
                        }
                    }
                },

                BasicTokenKind::Ident => Ident(
                    self.session
                        .try_write()
                        .unwrap()
                        .intern_symbol(value_str.to_owned()),
                ),

                // We compact delimeters by the delimeter type
                BasicTokenKind::OpenParen => OpenDelimeter(Parenthesis),
                BasicTokenKind::CloseParen => CloseDelimeter(Parenthesis),
                BasicTokenKind::OpenBrace => OpenDelimeter(Brace),
                BasicTokenKind::CloseBrace => CloseDelimeter(Brace),
                BasicTokenKind::OpenBracket => OpenDelimeter(Bracket),
                BasicTokenKind::CloseBracket => CloseDelimeter(Bracket),

                // Ligatures
                BasicTokenKind::Minus => self.maybe_ligature(match next_basic {
                    BasicTokenKind::Gt => Ok(ThinArrow),
                    _ => Err(Minus),
                }),

                BasicTokenKind::Eq => self.maybe_ligature(match next_basic {
                    BasicTokenKind::Eq => Ok(EqEq),
                    BasicTokenKind::Gt => Ok(FatArrow),
                    _ => Err(Eq),
                }),

                BasicTokenKind::Lt => self.maybe_ligature(match next_basic {
                    BasicTokenKind::Eq => Ok(LtEq),
                    _ => Err(Lt),
                }),

                BasicTokenKind::Gt => self.maybe_ligature(match next_basic {
                    BasicTokenKind::Eq => Ok(GtEq),
                    _ => Err(Lt),
                }),

                BasicTokenKind::Bang => self.maybe_ligature(match next_basic {
                    BasicTokenKind::Eq => Ok(BangEq),
                    _ => Err(Bang),
                }),

                // These are the same
                BasicTokenKind::EOF => EOF,
                BasicTokenKind::Semi => Semi,
                BasicTokenKind::Comma => Comma,
                BasicTokenKind::Dot => Dot,
                BasicTokenKind::Colon => Colon,
                BasicTokenKind::Plus => Plus,
                BasicTokenKind::Ampersand => Ampersand,
                BasicTokenKind::Pipe => Pipe,
                BasicTokenKind::Asterisk => Asterisk,
                BasicTokenKind::Slash => Slash,
                BasicTokenKind::Caret => Caret,
                BasicTokenKind::Percent => Percent,
            };

            let span = Span::new(start, self.position.clone());
            return Ok(Token {
                kind: token_kind,
                span,
            });
        }
    }
}
