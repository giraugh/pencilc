mod ast;

use std::{iter::Peekable, vec};

use crate::{
    error::ParseError,
    lex::{Delimeter, Kw, Token, TokenKind},
    session::{SessionRef, SymbolID},
};

use self::ast::{BinOpt, UnaOpt};

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    session: SessionRef<'a>,
    tokens: Peekable<vec::IntoIter<Token>>,
    previous_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(session: SessionRef<'a>, tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().peekable();
        Self {
            session,
            tokens,
            previous_token: None,
        }
    }

    pub fn bump(&mut self) -> Option<Token> {
        match self.tokens.next() {
            Some(token) => {
                self.previous_token = Some(token);
                Some(token)
            }
            None => None,
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    pub fn peek_kind(&mut self) -> TokenKind {
        match self.tokens.peek() {
            Some(token) => token.kind,
            None => TokenKind::EOF,
        }
    }

    pub fn expect_next(&mut self, expected_token: TokenKind) -> Result<()> {
        match self.tokens.next() {
            None => Err(ParseError::UnexpectedEndOfInput),
            Some(token) => {
                if token.kind == expected_token {
                    Ok(())
                } else {
                    Err(ParseError::ExpectedToken {
                        expected: expected_token,
                        actual: token.kind,
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

    /**
     * Grammar for modules
     * M -> {function}
     */

    pub fn parse_module(&mut self) -> Result<ast::Node> {
        // Parse declarations
        let mut declarations = Vec::new();
        loop {
            match self.peek_kind() {
                TokenKind::Ident(ident) if ident == Kw::Fn.into() => {
                    let function = self.parse_function()?;
                    declarations.push(Box::new(function));
                }

                TokenKind::EOF => break,

                t => return Err(ParseError::UnexpectedToken(t)),
            }
        }

        Ok(ast::Node::Module { declarations })
    }

    pub fn parse_function(&mut self) -> Result<ast::Node> {
        // Eat a fn keyword
        self.expect_next(TokenKind::Ident(Kw::Fn.into()))?;

        // Parse the function identifier
        let ident = self.expect_ident()?;

        // Parse the function arguments
        self.expect_next(TokenKind::OpenDelimeter(Delimeter::Parenthesis))?;
        let mut arguments = Vec::new();
        loop {
            match self.peek_kind() {
                TokenKind::CloseDelimeter(Delimeter::Parenthesis) => {
                    // Eat delimeter and break
                    self.bump();
                    break;
                }
                _ => {
                    let argument = self.parse_variable_declaration()?;
                    self.maybe_eat(TokenKind::Comma);
                    arguments.push(Box::new(argument));
                }
            }
        }

        // TODO: optionally parse a return type

        // Parse the body of the function
        let body = self.parse_block()?;

        // create function node
        Ok(ast::Node::FunctionDefinition {
            ident,
            arguments,
            body: Box::new(body),
        })
    }

    /**
     * Grammar for variable declaration
     *
     * D -> ident : ident
     */

    pub fn parse_variable_declaration(&mut self) -> Result<ast::Node> {
        // Parse name
        let name = self.expect_ident()?;

        // Parse colon
        self.expect_next(TokenKind::Colon)?;

        // Parse type
        // TODO: this should be a type expression
        let ty = self.expect_ident()?;

        Ok(ast::Node::VariableDeclaration { name, ty })
    }

    pub fn expect_ident(&mut self) -> Result<SymbolID> {
        match self.bump() {
            Some(token) => match token.kind {
                TokenKind::Ident(symbol_id) => Ok(symbol_id),
                kind => Err(ParseError::UnexpectedToken(kind)),
            },
            None => Err(ParseError::UnexpectedEndOfInput),
        }
    }

    pub fn parse_block(&mut self) -> Result<ast::Node> {
        // Eat an opening brace
        self.expect_next(TokenKind::OpenDelimeter(Delimeter::Brace))?;

        // Parse several statements
        let mut statements = Vec::new();
        loop {
            match self.peek_kind() {
                TokenKind::CloseDelimeter(Delimeter::Brace) => {
                    // Eat the brace
                    self.bump();
                    break;
                }
                _ => {
                    // Parse a statement
                    let statement = self.parse_statement()?;
                    statements.push(Box::new(statement));
                }
            }
        }

        Ok(ast::Node::Block { statements })
    }

    pub fn parse_statement(&mut self) -> Result<ast::Node> {
        // TODO: this!
        loop {
            match self.peek_kind() {
                TokenKind::Semi => {
                    // Eat the semi and return
                    self.bump();
                    return Ok(ast::Node::Statement);
                }
                TokenKind::CloseDelimeter(Delimeter::Brace) => {
                    return Ok(ast::Node::Statement);
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    /**
     * Grammar for expressions
     * E -> T {(+ | -) T}
     * T -> F {(* | /) F}
     * F -> P [(^) F]
     * P -> v | "(" E ")" | -T
     * v -> literal
     */
    pub fn parse_expression(&mut self) -> Result<ast::Node> {
        // Parse a term
        // its now our current expr
        let mut expr = self.parse_expression_term()?;

        // Maybe parse multiple binopt w/ another term
        loop {
            match self.peek_kind() {
                op @ (TokenKind::Plus | TokenKind::Minus) => {
                    // Eat the operator
                    self.bump();

                    // Parse a term, it will be the rhs with the current expr
                    let rhs = self.parse_expression_term()?;
                    expr = ast::Node::BinOpt {
                        operation: match op {
                            TokenKind::Plus => BinOpt::Add,
                            TokenKind::Minus => BinOpt::Subtract,
                            _ => unreachable!(),
                        },
                        operands: (Box::new(expr), Box::new(rhs)),
                    }
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_expression_term(&mut self) -> Result<ast::Node> {
        // Parse a factor
        // its now our current expr
        let mut expr = self.parse_expression_factor()?;

        // Maybe parse multiple binopt w/ another term
        loop {
            match self.peek_kind() {
                op @ (TokenKind::Asterisk | TokenKind::Slash) => {
                    // Eat the operator
                    self.bump();

                    // Parse a factor, it will be the rhs with the current expr
                    let rhs = self.parse_expression_factor()?;
                    expr = ast::Node::BinOpt {
                        operation: match op {
                            TokenKind::Asterisk => BinOpt::Multiply,
                            TokenKind::Slash => BinOpt::Divide,
                            _ => unreachable!(),
                        },
                        operands: (Box::new(expr), Box::new(rhs)),
                    }
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_expression_factor(&mut self) -> Result<ast::Node> {
        // First parse a base
        let mut expr = self.parse_expression_base()?;

        // Optionally parse an exponent operator and another factor
        if let TokenKind::Caret = self.peek_kind() {
            // eat the caret
            self.bump();

            // parse another factor to be the rhs
            let rhs = self.parse_expression_factor()?;
            expr = ast::Node::BinOpt {
                operation: BinOpt::Exponentiate,
                operands: (Box::new(expr), Box::new(rhs)),
            }
        }

        Ok(expr)
    }

    fn parse_expression_base(&mut self) -> Result<ast::Node> {
        match self.bump().unwrap().kind {
            // Parse a literal on its own
            TokenKind::Literal(value) => Ok(ast::Node::Literal(value)),

            // Parse a literal in parentheses
            TokenKind::OpenDelimeter(Delimeter::Parenthesis) => {
                let subexpr = self.parse_expression()?;
                self.expect_next(TokenKind::CloseDelimeter(Delimeter::Parenthesis))?;
                Ok(subexpr)
            }

            // Parse a negated factor
            TokenKind::Minus => {
                let subexpr = self.parse_expression_factor()?;
                Ok(ast::Node::UnaOpt {
                    operand: Box::new(subexpr),
                    operation: UnaOpt::Negate,
                })
            }

            t => Err(ParseError::UnexpectedToken(t)),
        }
    }
}

#[cfg(test)]
mod test {
    use std::{rc::Rc, sync::RwLock};

    use super::*;
    use crate::{lex::TokenLexer, session::Session};

    macro_rules! parser {
        ($s: expr) => {{
            let session = Rc::new(RwLock::new(Session::new($s)));
            let lexer = TokenLexer::new(session.clone());
            let tokens = lexer.tokenize().expect("lexing error");
            let mut parser = Parser::new(session, tokens);
            parser
        }};
    }

    #[test]
    fn test_parse_simple_expression() {
        let ast_result = parser!("10 + 20").parse_expression();
        assert!(ast_result.is_ok());
    }

    #[test]
    fn test_parse_complex_expression() {
        let ast_result = parser!("10 + (-5 * (3 ^ 2) + 1)").parse_expression();
        assert!(ast_result.is_ok());
    }

    #[test]
    fn test_parse_invalid_expression() {
        let ast_result = parser!("10 + (* (3 ^ 2) + 1)").parse_expression();
        assert!(ast_result.is_err());
    }
}
