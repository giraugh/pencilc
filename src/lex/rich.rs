use std::fmt::Debug;
use std::fmt::Display;

use self::Delimeter::*;
use self::TokenKind::*;
use super::{cursor::Cursor, BasicTokenKind, LiteralKind};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, derive_more::Add)]
struct CharPos(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span {
    start: CharPos,
    end: CharPos,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start.0, self.end.0)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
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

#[allow(unused)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenKind {
    /// End of file
    EOF,

    /// Identifier
    Ident,

    /// Literal
    Literal {
        kind: LiteralKind,
    },

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

pub struct RichTokenLexer<'a> {
    input: &'a str,
    position: CharPos,
    cursor: Cursor<'a>,
}

#[allow(unused)]
pub struct SyntaxError {
    span: Span,
    message: String,
}

macro_rules! syntax_error {
    ($s: expr, $m: expr) => {
        return Err(SyntaxError {
            span: $s,
            message: ($m).into(),
        })
    };
}

impl<'a> RichTokenLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            position: CharPos(0),
            cursor: Cursor::new(input),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, SyntaxError> {
        loop {
            let basic_token = self.cursor.next_token();
            let start = self.position;
            self.position = self.position + CharPos(basic_token.len);
            let span = Span {
                start,
                end: self.position.clone(),
            };

            let token_kind = match basic_token.kind {
                // Skip comments and whitespace
                BasicTokenKind::LineComment => continue,
                BasicTokenKind::Whitespace => continue,

                // Throw error for unknown tokens
                BasicTokenKind::Unknown => syntax_error!(span, "Unknown token"),

                // Literals
                BasicTokenKind::Literal { kind } => match kind {
                    // Throw error for unterminated string literals
                    LiteralKind::Str { terminated: false } => {
                        syntax_error!(span, "Unterminated string literal")
                    }

                    // TODO: intern these
                    kind => Literal { kind },
                },

                // TODO: intern idents
                BasicTokenKind::Ident => Ident,

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
