use multipeek::{IteratorExt, MultiPeek};
use std::vec;

use crate::{
    error::ParseError,
    id::{BlockId, ExprId, StatementId},
    lex::{Kw, Token, TokenKind},
    session::{symbol::Symbol, SessionRef},
    span::{CharPos, Span},
};

pub type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    #[allow(unused)]
    session: SessionRef<'a>,
    tokens: MultiPeek<vec::IntoIter<Token>>,
    previous_token: Option<Token>,
    span_start_stack: Vec<CharPos>,

    pub current_block_id: BlockId,
    pub current_statement_id: StatementId,
    pub current_expr_id: ExprId,
}

impl<'a> Parser<'a> {
    pub fn new(session: SessionRef<'a>, tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().multipeek();
        Self {
            session,
            tokens,
            previous_token: None,
            span_start_stack: Default::default(),
            current_block_id: Default::default(),
            current_statement_id: Default::default(),
            current_expr_id: Default::default(),
        }
    }

    pub fn parse_ident(&mut self) -> Result<Symbol> {
        // TODO: this should probably error if the ident is reserved
        match self.bump() {
            Some(token) => match token.kind {
                TokenKind::Ident(symbol) => Ok(symbol),
                _ => Err(ParseError::UnexpectedToken(token)),
            },
            None => Err(ParseError::UnexpectedEndOfInput),
        }
    }

    pub fn push_start(&mut self) {
        let start = self
            .tokens
            .peek()
            .map_or(CharPos(0), |token| token.span.start);
        self.span_start_stack.push(start)
    }

    pub fn pop_span(&mut self) -> Span {
        let start = self.span_start_stack.pop().unwrap();
        let end = self.previous_token.clone().unwrap().span.end;
        Span::new(start, end)
    }

    pub fn bump(&mut self) -> Option<Token> {
        match self.tokens.next() {
            Some(token) => {
                self.previous_token = Some(token.clone());
                Some(token)
            }
            None => None,
        }
    }

    pub fn peek_kind(&mut self) -> TokenKind {
        match self.tokens.peek() {
            Some(token) => token.kind.clone(),
            None => TokenKind::EOF,
        }
    }

    pub fn peek_kind_two(&mut self) -> (TokenKind, TokenKind) {
        let kind_1 = self
            .tokens
            .peek_nth(0)
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::EOF);
        let kind_2 = self
            .tokens
            .peek_nth(1)
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::EOF);
        (kind_1, kind_2)
    }

    pub fn expect_kw(&mut self, kw: Kw) -> Result<()> {
        match self.bump() {
            None => Err(ParseError::UnexpectedEndOfInput),
            Some(token) => match token.kind {
                TokenKind::Ident(symbol) if symbol.is_kw(kw.clone()) => Ok(()),
                _ => Err(ParseError::ExpectedKw {
                    expected: kw.clone(),
                    actual: token,
                }),
            },
        }
    }

    pub fn expect_next(&mut self, expected_token: TokenKind) -> Result<()> {
        match self.bump() {
            None => Err(ParseError::UnexpectedEndOfInput),
            Some(token) => {
                if token.kind == expected_token {
                    Ok(())
                } else {
                    Err(ParseError::ExpectedToken {
                        expected: expected_token,
                        actual: token,
                    })
                }
            }
        }
    }

    pub fn maybe_eat(&mut self, possible_token: TokenKind) -> bool {
        if self.peek_kind() == possible_token {
            self.bump();
            true
        } else {
            false
        }
    }
}
