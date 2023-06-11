mod cursor;
mod rich;

use self::rich::RichTokenLexer;
use self::rich::SyntaxError;
use self::rich::Token;
use self::rich::TokenKind;
use self::BasicTokenKind::*;
use self::LiteralKind::*;
use crate::lex::cursor::Cursor;

use std::fmt::Debug;

pub fn tokenize(input: &str) -> impl Iterator<Item = Result<Token, SyntaxError>> + '_ {
    let mut lexer = RichTokenLexer::new(input);
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

impl<'a> Cursor<'a> {
    /// Get the next token
    pub fn next_token(&mut self) -> BasicToken {
        // Get how many chars remaing before we consume this token
        let initial_len_remaining = self.remaining_chars();

        // Advance to next character (consumes it)
        let first_char = match self.next_char() {
            Some(c) => c,
            None => {
                return BasicToken::new(BasicTokenKind::EOF, 0);
            }
        };

        let token_kind = match first_char {
            // Comment
            '/' => match self.peek_first() {
                '/' => self.line_comment(),
                _ => Slash,
            },

            // Whitespace
            c if c.is_whitespace() => {
                self.eat_while(|c| c.is_whitespace());
                Whitespace
            }

            // Identifier
            c if Self::is_ident_start(c) => self.identifier(),

            // Numeric literal
            ('0'..='9') => Literal {
                kind: self.numeric_literal(),
            },

            // String literal
            '"' => Literal {
                kind: self.string_literal(),
            },

            // Single character symbols
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '!' => Bang,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            ':' => Colon,
            '=' => Eq,
            '<' => Lt,
            '>' => Gt,
            '-' => Minus,
            '+' => Plus,
            '&' => Ampersand,
            '|' => Pipe,
            '*' => Asterisk,
            '^' => Caret,
            '%' => Percent,

            // We dont know what token this is
            _ => Unknown,
        };

        // Create and return token
        let token_length = initial_len_remaining - self.remaining_chars();
        BasicToken {
            kind: token_kind,
            len: token_length,
        }
    }

    fn identifier(&mut self) -> BasicTokenKind {
        self.eat_while(|c| Self::is_ident_continue(c));
        Ident
    }

    /// Consume a line comment
    fn line_comment(&mut self) -> BasicTokenKind {
        // We eat all characters until a new line
        self.eat_while(|c| c != '\n');
        LineComment
    }

    /// Consume a numeric literal
    fn numeric_literal(&mut self) -> LiteralKind {
        // Eat digits
        self.eat_numeric_digits();

        // Eat fractional part?
        // TODO: probably consider cases where this doesn't indicate a fractional part
        match self.peek_first() {
            '.' => {
                self.eat_numeric_digits();
                Float
            }
            _ => Int,
        }
    }

    /// Consume a string literal
    fn string_literal(&mut self) -> LiteralKind {
        let mut terminated = false;
        while let Some(c) = self.next_char() {
            match c {
                '\\' => {
                    self.next_char();
                }
                '\n' => {
                    break;
                }
                '"' => {
                    terminated = true;
                    break;
                }
                _ => {}
            }
        }

        Str { terminated }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BasicToken {
    kind: BasicTokenKind,
    len: usize,
}

impl BasicToken {
    pub fn new(kind: BasicTokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

impl Debug for BasicToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.kind, self.len)?;
        Ok(())
    }
}

#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BasicTokenKind {
    /// End of file
    EOF,

    /// Unknown token
    Unknown,

    /// Single line comment
    /// (using //)
    LineComment,

    /// Literal of any kind
    Literal { kind: LiteralKind },

    /// Identifiers and keywords
    Ident,

    /// Any whitespace character
    Whitespace,

    /// ;
    Semi,
    /// ,
    Comma,
    /// !
    Bang,
    /// .
    Dot,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// :
    Colon,
    /// =
    Eq,
    /// <
    Lt,
    /// >
    Gt,
    /// -
    Minus,
    /// +
    Plus,
    /// &
    Ampersand,
    /// |
    Pipe,
    /// *
    Asterisk,
    /// /
    Slash,
    /// ^
    Caret,
    /// %
    Percent,
}

#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    /// Integer e.g 0 or 200
    /// For now, only base 10
    Int,

    /// Float e,g 1.0 or 0.3
    /// For now, only base 10
    Float,

    /// String
    Str { terminated: bool },
}
