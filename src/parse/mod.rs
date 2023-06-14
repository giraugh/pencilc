mod parser;

use crate::{
    ast,
    error::ParseError,
    lex::{Delimeter, Kw, TokenKind},
    span::Span,
};
pub use parser::Parser;
use parser::Result;

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self) -> Result<ast::Module> {
        // Start node
        self.push_start();

        // Parse items until EOF
        let mut items = Vec::new();
        while self.peek_kind() != TokenKind::EOF {
            let item = self.parse_item()?;
            items.push(item);
        }

        // Return module
        Ok(ast::Module {
            items,
            span: self.pop_span(),
        })
    }

    fn parse_item(&mut self) -> Result<ast::Item> {
        // Start node
        self.push_start();

        // Parse the item kind
        let kind = match self.peek_kind() {
            // Parse a function def item
            TokenKind::Ident(symbol) if symbol == Kw::Fn.into() => {
                let func = self.parse_function_definition()?;
                ast::ItemKind::FnDef(Box::new(func))
            }

            k => return Err(crate::error::ParseError::UnexpectedToken(k)),
        };

        // Return item
        Ok(ast::Item {
            kind,
            span: self.pop_span(),
        })
    }

    fn parse_function_definition(&mut self) -> Result<ast::FnDef> {
        // Eat the fn keyword
        self.bump();

        // Parse the function declaration
        let decl = Box::new(self.parse_function_declaration()?);

        // Parse the function body
        let body = Box::new(self.parse_block()?);

        // Return the node
        Ok(ast::FnDef { body, decl })
    }

    fn parse_comma_delim(&mut self, end: Delimeter) -> Result<()> {
        // Do we *need* a comma or just can have one
        match self.peek_kind_two() {
            (TokenKind::Comma, TokenKind::CloseDelimeter(delim)) if delim == end => {
                self.bump();
            }
            (TokenKind::CloseDelimeter(delim), _) if delim == end => return Ok(()),
            (_, _) => {
                self.expect_next(TokenKind::Comma)?;
            }
        }
        Ok(())
    }

    fn parse_function_declaration(&mut self) -> Result<ast::FnDecl> {
        // Start node
        self.push_start();

        // Parse the name
        let name = self.parse_ident()?;

        // Eat the open paren
        self.expect_next(TokenKind::OpenDelimeter(Delimeter::Parenthesis))?;

        // Parse the parameters
        let mut params = Vec::new();
        while self.peek_kind() != TokenKind::CloseDelimeter(Delimeter::Parenthesis) {
            let param = self.parse_binding()?;
            params.push(param);
            self.parse_comma_delim(Delimeter::Parenthesis)?;
        }

        // Eat the close paren
        self.expect_next(TokenKind::CloseDelimeter(Delimeter::Parenthesis))?;

        // Return type?
        let ty = match self.peek_kind() {
            TokenKind::Minus => {
                // Eat the ->
                self.bump();
                self.expect_next(TokenKind::Gt)?;

                // parse a type expression
                let ty = Box::new(self.parse_type_expr()?);
                Some(ty)
            }
            _ => None,
        };

        // Return the node
        Ok(ast::FnDecl {
            name,
            params,
            ty,
            span: self.pop_span(),
        })
    }

    fn parse_binding(&mut self) -> Result<ast::Binding> {
        // Start node
        self.push_start();

        // Parse name
        let name = self.parse_ident()?;

        // Parse optional ty
        let ty = match self.peek_kind() {
            TokenKind::Colon => {
                // Eat the colon
                self.bump();

                // Parse the type
                Some(Box::new(self.parse_type_expr()?))
            }
            _ => None,
        };

        // Return node
        Ok(ast::Binding {
            name,
            ty,
            span: self.pop_span(),
        })
    }

    fn parse_type_expr(&mut self) -> Result<ast::TyExpr> {
        // Start node
        self.push_start();

        // Parse kind
        let kind = match self.peek_kind() {
            TokenKind::Ident(symbol) => {
                // Eat the ident
                self.bump();

                // Return kind
                ast::TyExprKind::Name(symbol)
            }
            k => return Err(ParseError::UnexpectedToken(k)),
        };

        // Return node
        Ok(ast::TyExpr {
            kind,
            span: self.pop_span(),
        })
    }

    fn parse_block(&mut self) -> Result<ast::Block> {
        // Start node
        self.push_start();

        // Eat the open brace
        self.expect_next(TokenKind::OpenDelimeter(Delimeter::Brace))?;

        // Parse statements
        let mut statements = Vec::new();
        while self.peek_kind() != TokenKind::CloseDelimeter(Delimeter::Brace) {
            // Parse statement
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        // Eat the close brace
        self.expect_next(TokenKind::CloseDelimeter(Delimeter::Brace))?;

        // Return node
        Ok(ast::Block {
            statements,
            span: self.pop_span(),
        })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        // Start node
        self.push_start();

        // Parse kind
        let kind = match self.peek_kind() {
            // Variable declaration
            TokenKind::Ident(symbol) if symbol == Kw::Let.into() => {
                // Eat the let
                self.bump();

                // Parse the binding
                let binding = Box::new(self.parse_binding()?);

                // Optionally parse an expression (if there is an =)
                let expr = match self.peek_kind() {
                    TokenKind::Eq => {
                        self.bump();
                        Some(Box::new(self.parse_expr()?))
                    }
                    _ => None,
                };

                ast::StatementKind::Let(binding, expr)
            }

            // Or an expression
            _ => {
                let expr = Box::new(self.parse_expr()?);
                ast::StatementKind::Expr(expr)
            }
        };

        // Eat the semi
        let need_semi = !matches!(kind, ast::StatementKind::Expr(_));
        let has_semi = if need_semi {
            self.expect_next(TokenKind::Semi)?;
            true
        } else {
            self.maybe_eat(TokenKind::Semi)
        };

        // Return node
        Ok(ast::Statement {
            kind,
            has_semi,
            span: self.pop_span(),
        })
    }

    /**
     * Grammar for expressions
     * E -> Assign | Let | Return | N
     * N -> T {(+ | -) T}
     * T -> F {(* | /) F}
     * F -> P [(^) F]
     * P -> v | "(" E ")" | -T
     * v -> literal | func() | ident
     */

    fn parse_expr(&mut self) -> Result<ast::Expr> {
        // Start a node
        self.push_start();

        // parse kind
        let kind = match self.peek_kind_two() {
            // Parse variable assignment
            (TokenKind::Ident(symbol), TokenKind::Eq) => {
                // Eat the ident and equals
                self.bump();
                self.bump();

                // Parse an expression
                let expr = Box::new(self.parse_expr()?);

                // Create assign
                ast::ExprKind::Assign(symbol, expr)
            }

            // Parse let assignment
            (TokenKind::Ident(symbol), _) if symbol == Kw::Let.into() => {
                // Eat the let
                self.bump();

                // Parse a binding
                let binding = Box::new(self.parse_binding()?);

                // Expect an equals
                self.expect_next(TokenKind::Eq)?;

                // Parse an expression
                let expr = Box::new(self.parse_expr()?);

                // Create let
                ast::ExprKind::Let(binding, expr)
            }

            // Parse return
            (TokenKind::Ident(symbol), _) if symbol == Kw::Return.into() => {
                // Eat the return
                self.bump();

                // Expression?
                let expr = match self.peek_kind() {
                    // No expression
                    TokenKind::Semi | TokenKind::CloseDelimeter(Delimeter::Brace) => None,

                    _ => {
                        // Parse an expression
                        let expr = Box::new(self.parse_expr()?);
                        Some(expr)
                    }
                };

                ast::ExprKind::Return(expr)
            }

            // Parse a numeric
            _ => {
                self.pop_span();
                return self.parse_numeric_expression();
            }
        };

        // Return node
        Ok(ast::Expr {
            kind,
            span: self.pop_span(),
        })
    }

    fn parse_numeric_expression(&mut self) -> Result<ast::Expr> {
        // First pass a term
        let mut expr = self.parse_term_expression()?;

        // Now parse 0 or more +/- w/ another term
        loop {
            match self.peek_kind() {
                op @ (TokenKind::Plus | TokenKind::Minus) => {
                    // Eat the operator
                    self.bump();

                    // Determine operation
                    let operation = match op {
                        TokenKind::Plus => ast::BinaryOpt::Add,
                        TokenKind::Minus => ast::BinaryOpt::Subtract,
                        _ => unreachable!(),
                    };

                    // Parse a term to be the rhs
                    let rhs = self.parse_term_expression()?;

                    // Construct binary opt as <expr> <opt> <rhs>
                    expr = ast::Expr {
                        span: Span::new(expr.span.start, rhs.span.end),
                        kind: ast::ExprKind::Binary(
                            operation,
                            (Box::new(expr), Box::new(rhs.clone())),
                        ),
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_term_expression(&mut self) -> Result<ast::Expr> {
        // First pass a term
        let mut expr = self.parse_factor_expression()?;

        // Now parse 0 or more *// w/ another term
        loop {
            match self.peek_kind() {
                op @ (TokenKind::Asterisk | TokenKind::Slash) => {
                    // Eat the operator
                    self.bump();

                    // Determine operation
                    let operation = match op {
                        TokenKind::Asterisk => ast::BinaryOpt::Multiply,
                        TokenKind::Slash => ast::BinaryOpt::Divide,
                        _ => unreachable!(),
                    };

                    // Parse a term to be the rhs
                    let rhs = self.parse_term_expression()?;

                    // Construct binary opt as <expr> <opt> <rhs>
                    expr = ast::Expr {
                        span: Span::new(expr.span.start, rhs.span.end),
                        kind: ast::ExprKind::Binary(
                            operation,
                            (Box::new(expr), Box::new(rhs.clone())),
                        ),
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_factor_expression(&mut self) -> Result<ast::Expr> {
        // First parse a base
        let mut expr = self.parse_base_expression()?;

        // Optionally parse an exponent operator and another factor
        if let TokenKind::Caret = self.peek_kind() {
            // eat the caret
            self.bump();

            // parse another factor to be the rhs
            let rhs = self.parse_factor_expression()?;
            expr = ast::Expr {
                span: Span::new(expr.span.start, rhs.span.end),
                kind: ast::ExprKind::Binary(
                    ast::BinaryOpt::Exponentiate,
                    (Box::new(expr), Box::new(rhs)),
                ),
            }
        }

        Ok(expr)
    }

    fn parse_base_expression(&mut self) -> Result<ast::Expr> {
        // Start a node
        self.push_start();

        // Parse kind
        let kind = match self.bump().unwrap().kind {
            // Parse a literal on its own
            TokenKind::Literal(value) => ast::ExprKind::Literal(value),

            // Parse expressions starting with an ident...
            TokenKind::Ident(symbol_id) => match self.peek_kind() {
                // Parse a function invocation
                TokenKind::OpenDelimeter(Delimeter::Parenthesis) => {
                    let params = self.parse_function_call_params()?;
                    ast::ExprKind::FnCall(symbol_id, params)
                }

                // Parse an identifier on its own
                _ => ast::ExprKind::Name(symbol_id),
            },

            // Parse an expression in parentheses
            TokenKind::OpenDelimeter(Delimeter::Parenthesis) => {
                let subexpr = self.parse_expr()?;
                self.expect_next(TokenKind::CloseDelimeter(Delimeter::Parenthesis))?;
                self.pop_span();
                return Ok(subexpr);
            }

            // Parse a negated factor
            TokenKind::Minus => {
                let factor = Box::new(self.parse_factor_expression()?);
                ast::ExprKind::Unary(ast::UnaryOpt::Negate, factor)
            }

            t => return Err(ParseError::UnexpectedToken(t)),
        };

        // Return node
        Ok(ast::Expr {
            kind,
            span: self.pop_span(),
        })
    }

    fn parse_function_call_params(&mut self) -> Result<Vec<ast::Expr>> {
        // Eat the open paren
        self.bump();

        // Parse params until close paren
        let mut params = Vec::new();
        while self.peek_kind() != TokenKind::CloseDelimeter(Delimeter::Parenthesis) {
            // Parse expression as param
            let param = self.parse_expr()?;
            params.push(param);
            self.parse_comma_delim(Delimeter::Parenthesis)?;
        }

        // Eat the close param
        self.expect_next(TokenKind::CloseDelimeter(Delimeter::Parenthesis))?;

        Ok(params)
    }
}
