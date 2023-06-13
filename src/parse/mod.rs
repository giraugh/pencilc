mod ast;
mod pretty_print;

use multipeek::{IteratorExt, MultiPeek};
use std::vec;

use crate::{
    error::ParseError,
    lex::{Delimeter, Kw, Token, TokenKind},
    session::{SessionRef, SymbolID},
};

use self::ast::{BinOpt, UnaOpt};
pub use self::pretty_print::PrettyPrint;

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    session: SessionRef<'a>,
    tokens: MultiPeek<vec::IntoIter<Token>>,
    previous_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(session: SessionRef<'a>, tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().multipeek();
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

    pub fn peek_kind(&mut self) -> TokenKind {
        match self.tokens.peek() {
            Some(token) => token.kind,
            None => TokenKind::EOF,
        }
    }

    pub fn peek_kind_two(&mut self) -> (TokenKind, TokenKind) {
        let kind_1 = self
            .tokens
            .peek_nth(0)
            .map(|t| t.kind)
            .unwrap_or(TokenKind::EOF);
        let kind_2 = self
            .tokens
            .peek_nth(1)
            .map(|t| t.kind)
            .unwrap_or(TokenKind::EOF);
        (kind_1, kind_2)
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
                    let function = self.parse_function_definition()?;
                    declarations.push(Box::new(function));
                }

                TokenKind::EOF => break,

                t => return Err(ParseError::UnexpectedToken(t)),
            }
        }

        Ok(ast::Node::Module { declarations })
    }

    pub fn parse_function_definition(&mut self) -> Result<ast::Node> {
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

        // Optionally parse a return type
        let return_ty = if let TokenKind::Minus = self.peek_kind() {
            // Eat the ->
            self.bump();
            self.expect_next(TokenKind::Gt)?;

            // Expect a type expression
            let expr = self.parse_type_expression()?;
            Some(Box::new(expr))
        } else {
            None
        };

        // Parse the body of the function
        let body = self.parse_block()?;

        // create function node
        Ok(ast::Node::FunctionDefinition {
            ident,
            arguments,
            return_ty,
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
        let ty = Box::new(self.parse_type_expression()?);

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
        let mut seen_return = false;
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
                    if matches!(statement, ast::Node::Return { .. }) {
                        if seen_return {
                            return Err(ParseError::MultipleReturns);
                        } else {
                            seen_return = true;
                        }
                    }
                    statements.push(Box::new(statement));
                }
            }
        }

        Ok(ast::Node::Block { statements })
    }

    /**
     * Grammar for statements
     *
     * S -> VD | S [;]
     *
     */

    pub fn parse_statement(&mut self) -> Result<ast::Node> {
        let inner = match self.peek_kind_two() {
            // Parse a variable declaration
            (TokenKind::Ident(ident), _) if ident == Kw::Let.into() => {
                // Consume the `let`
                self.bump();

                // Parse the name of the variable
                let name = self.expect_ident()?;

                // Optionally parse a type expression
                let ty = match self.peek_kind() {
                    TokenKind::Colon => {
                        // Eat the colon
                        self.bump();

                        // Parse a type expression
                        Some(Box::new(self.parse_type_expression()?))
                    }
                    _ => None,
                };

                // Parse the equals
                self.expect_next(TokenKind::Eq)?;

                // Parse the expression
                let expr = Box::new(self.parse_expression()?);

                // Create the node
                ast::Node::VariableDefinition { name, ty, expr }
            }

            // Parse return statement
            (TokenKind::Ident(ident), _) if ident == Kw::Return.into() => {
                // Eat the return keyword
                self.bump();

                // Expression?
                match self.peek_kind() {
                    // No return value
                    TokenKind::Semi => ast::Node::Return {
                        expr: None,
                        implicit: false,
                    },

                    // Has return value
                    _ => {
                        let expr = self.parse_expression()?;
                        ast::Node::Return {
                            expr: Some(Box::new(expr)),
                            implicit: false,
                        }
                    }
                }
            }

            // Parse variable assignment
            (TokenKind::Ident(_), TokenKind::Eq) => {
                // Parse the ident
                let ident = self.expect_ident()?;

                // Parse the rest of the assignment
                self.parse_variable_assignment(ident)?
            }

            // Parse an expression that starts with an ident or literal
            (TokenKind::Ident(_) | TokenKind::Literal(_), _) => {
                // Parse the expression
                let expr = self.parse_expression()?;

                // is this an unterminated expression?
                // e.g is it an implicit return?
                if let TokenKind::Semi = self.peek_kind() {
                    expr
                } else {
                    return Ok(ast::Node::Return {
                        expr: Some(Box::new(expr)),
                        implicit: true,
                    });
                }
            }

            (token, _) => return Err(ParseError::UnexpectedToken(token)),
        };

        // Eat the semi
        self.expect_next(TokenKind::Semi)?;

        // Return the statement
        Ok(inner)
    }

    /// the identifier is already parsed but the open paren is not
    pub fn parse_function_invocation(&mut self, function_ident: SymbolID) -> Result<ast::Node> {
        // Eat the open paren
        self.expect_next(TokenKind::OpenDelimeter(Delimeter::Parenthesis))?;

        // Parse the function arguments
        let mut arguments = Vec::new();
        loop {
            match self.peek_kind() {
                TokenKind::CloseDelimeter(Delimeter::Parenthesis) => {
                    // Eat the close paren and stop
                    self.bump();
                    break;
                }
                TokenKind::Comma => {
                    // Eat the comma
                    self.bump();
                }
                _ => {
                    let expression = self.parse_expression()?;
                    arguments.push(Box::new(expression));
                }
            }
        }

        Ok(ast::Node::FunctionInvocation {
            ident: function_ident,
            arguments,
        })
    }

    /// the identifier is already parsed but the = is not
    pub fn parse_variable_assignment(&mut self, variable_ident: SymbolID) -> Result<ast::Node> {
        // Eat the equals
        self.expect_next(TokenKind::Eq)?;

        // Parse a binding expression
        let expression = self.parse_expression()?;

        Ok(ast::Node::VariableAssignment {
            name: variable_ident,
            expr: Box::new(expression),
        })
    }

    /// For now a type expression is always just an identifier
    pub fn parse_type_expression(&mut self) -> Result<ast::Node> {
        let type_name = self.expect_ident()?;
        Ok(ast::Node::Ty(type_name))
    }

    /**
     * Grammar for expressions
     * E -> T {(+ | -) T}
     * T -> F {(* | /) F}
     * F -> P [(^) F]
     * P -> v | "(" E ")" | -T
     * v -> literal | func() | ident
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

            // Parse expressions starting with an ident...
            TokenKind::Ident(symbol_id) => match self.peek_kind() {
                // Parse a function invocation
                TokenKind::OpenDelimeter(Delimeter::Parenthesis) => {
                    self.parse_function_invocation(symbol_id)
                }

                // Parse an identifier on its own
                _ => Ok(ast::Node::Variable(symbol_id)),
            },

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
    fn test_parse_expression_with_variable() {
        let ast_result = parser!("10 + (a * (3 ^ 2) + 1)").parse_expression();
        assert!(ast_result.is_ok());
    }

    #[test]
    fn test_parse_invalid_expression() {
        let ast_result = parser!("10 + (* (3 ^ 2) + 1)").parse_expression();
        assert!(ast_result.is_err());
    }
}
