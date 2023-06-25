mod parser;

use crate::{
    ast::{self, ComparisonOpt, LogicalOpt},
    error::ParseError,
    id::Idx,
    lex::{Delimeter, Kw, LiteralValue, TokenKind},
    span::Span,
};
pub use parser::Parser;
use parser::Result;

impl<'a> Parser<'a> {
    pub fn parse_module(&mut self, name: &str) -> Result<ast::Module> {
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
            name: name.to_owned(),
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
            TokenKind::Ident(symbol) if symbol.is_kw(Kw::Fn) => {
                let func = self.parse_function_definition()?;
                ast::ItemKind::FnDef(Box::new(func))
            }

            _ => return Err(ParseError::UnexpectedToken(self.bump().unwrap())),
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
        let body = Box::new(self.parse_block(true)?);

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
            TokenKind::ThinArrow => {
                // Eat the ->
                self.bump();

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
            _ => return Err(ParseError::UnexpectedToken(self.bump().unwrap())),
        };

        // Return node
        Ok(ast::TyExpr {
            kind,
            span: self.pop_span(),
        })
    }

    fn parse_block(&mut self, eat_open_brace: bool) -> Result<ast::Block> {
        // Start node
        self.push_start();

        // Eat the open brace
        if eat_open_brace {
            self.expect_next(TokenKind::OpenDelimeter(Delimeter::Brace))?;
        }

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
            id: self.current_block_id.next(),
            statements,
            span: self.pop_span(),
        })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        // Start node
        self.push_start();

        // Parse kind
        let kind = match self.peek_kind() {
            // Return
            TokenKind::Ident(i) if i.is_kw(Kw::Return) => {
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

                ast::StatementKind::Return(expr)
            }

            // Starts with if -> either if statement or if expr
            TokenKind::Ident(i) if i.is_kw(Kw::If) => {
                // Start a span in case we need it
                self.push_start();

                // Eat the if
                self.bump();

                // Parse an expression to be condition
                let condition = Box::new(self.parse_expr()?);

                // Parse a block
                let block = Box::new(self.parse_block(true)?);

                // Is there an else? If so this is an expression
                match self.peek_kind() {
                    TokenKind::Ident(symb) if symb.is_kw(Kw::Else) => {
                        // Eat the else
                        self.bump();

                        // Parse the else block
                        let else_block = Box::new(self.parse_block(true)?);

                        // Create if expression
                        ast::StatementKind::Expr(Box::new(ast::Expr {
                            id: self.current_expr_id.next(),
                            span: self.pop_span(),
                            kind: ast::ExprKind::If(condition, block, else_block),
                        }))
                    }
                    _ => {
                        self.pop_span(); // We dont need the span
                        ast::StatementKind::If(condition, block)
                    }
                }
            }

            // Expression
            _ => {
                let expr = Box::new(self.parse_expr()?);
                ast::StatementKind::Expr(expr)
            }
        };

        // Eat the semi
        let need_semi = !matches!(
            kind,
            ast::StatementKind::Expr(_) | ast::StatementKind::If(_, _)
        );
        let has_semi = if need_semi {
            self.expect_next(TokenKind::Semi)?;
            true
        } else {
            self.maybe_eat(TokenKind::Semi)
        };

        // Return node
        Ok(ast::Statement {
            id: self.current_statement_id.next(),
            kind,
            has_semi,
            span: self.pop_span(),
        })
    }

    /**
     * Grammar for expressions
     * E -> Assign | Let | Return | C
     * C -> N [== N | != N | > N | >= N | <= N]
     * N -> T {(+ | - | "||") T}
     * T -> F {(* | / | "and") F}
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
            (TokenKind::Ident(symbol), _) if symbol.is_kw(Kw::Let) => {
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

            // Parse a numeric
            _ => {
                self.pop_span();
                return self.parse_comp_expression();
            }
        };

        // Return node
        Ok(ast::Expr {
            id: self.current_expr_id.next(),
            kind,
            span: self.pop_span(),
        })
    }

    fn parse_comp_expression(&mut self) -> Result<ast::Expr> {
        // First pass a numeric
        let mut expr = self.parse_numeric_expression()?;

        // Now parse 0 or 1 logical operators
        match self.peek_kind() {
            op @ (TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Gt
            | TokenKind::Lt
            | TokenKind::GtEq
            | TokenKind::LtEq) => {
                // Eat the operator
                self.bump();

                // Determine operation
                let operation = match op {
                    TokenKind::EqEq => ComparisonOpt::Equal,
                    TokenKind::BangEq => ComparisonOpt::NotEqual,
                    TokenKind::Gt => ComparisonOpt::GreaterThan,
                    TokenKind::Lt => ComparisonOpt::LessThan,
                    TokenKind::GtEq => ComparisonOpt::GreaterThanOrEqual,
                    TokenKind::LtEq => ComparisonOpt::LessThanOrEqual,
                    _ => unreachable!(),
                };

                // Parse a term to be the rhs
                let rhs = self.parse_numeric_expression()?;

                // Construct comparison opt as <expr> <opt> <rhs>
                expr = ast::Expr {
                    id: self.current_expr_id.next(),
                    span: Span::new(expr.span.start, rhs.span.end),
                    kind: ast::ExprKind::Comparison(
                        operation,
                        (Box::new(expr), Box::new(rhs.clone())),
                    ),
                }
            }

            _ => {}
        }

        Ok(expr)
    }

    /// Slight misnomer, may actually be any type of literal
    /// but the point is that this is where numeric binary/unary etc operations are parsed
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
                        id: self.current_expr_id.next(),
                        span: Span::new(expr.span.start, rhs.span.end),
                        kind: ast::ExprKind::Binary(
                            operation,
                            (Box::new(expr), Box::new(rhs.clone())),
                        ),
                    }
                }

                TokenKind::PipePipe => {
                    // Eat the operator
                    self.bump();

                    // Parse a term to be rhs
                    let rhs = self.parse_term_expression()?;

                    // Construct logical opt as <expr> <opt> <rhs>
                    expr = ast::Expr {
                        id: self.current_expr_id.next(),
                        span: Span::new(expr.span.start, rhs.span.end),
                        kind: ast::ExprKind::Logical(
                            LogicalOpt::Or,
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
                        id: self.current_expr_id.next(),
                        span: Span::new(expr.span.start, rhs.span.end),
                        kind: ast::ExprKind::Binary(
                            operation,
                            (Box::new(expr), Box::new(rhs.clone())),
                        ),
                    }
                }

                TokenKind::AmpAmp => {
                    // Eat the operator
                    self.bump();

                    // Parse a term to be rhs
                    let rhs = self.parse_term_expression()?;

                    // Construct logical opt as <expr> <opt> <rhs>
                    expr = ast::Expr {
                        id: self.current_expr_id.next(),
                        span: Span::new(expr.span.start, rhs.span.end),
                        kind: ast::ExprKind::Logical(
                            LogicalOpt::And,
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
                id: self.current_expr_id.next(),
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
        let token = self.bump().unwrap();
        let kind = match token.kind {
            // Parse a literal on its own
            TokenKind::Literal(value) => ast::ExprKind::Literal(value),

            // Boolean true literal
            TokenKind::Ident(symbol_id) if symbol_id.is_kw(Kw::True) => {
                ast::ExprKind::Literal(LiteralValue::Bool(true))
            }

            // Boolean false literal
            TokenKind::Ident(symbol_id) if symbol_id.is_kw(Kw::False) => {
                ast::ExprKind::Literal(LiteralValue::Bool(false))
            }

            // Parse an if expression
            TokenKind::Ident(symbol) if symbol.is_kw(Kw::If) => {
                // parse an expression to be the condition
                let condition = Box::new(self.parse_expr()?);

                // Parse a block for when cond is true
                let block_true = Box::new(self.parse_block(true)?);

                // Expect an `else` keyword
                self.expect_kw(Kw::Else)?;

                let block_false = Box::new(self.parse_block(true)?);

                // Create node
                ast::ExprKind::If(condition, block_true, block_false)
            }

            // Parse expressions starting with an ident...
            TokenKind::Ident(symbol_id) => match self.peek_kind() {
                // Parse a function invocation
                TokenKind::OpenDelimeter(Delimeter::Parenthesis) => {
                    let params = self.parse_function_call_params()?;
                    ast::ExprKind::FnCall(symbol_id, params)
                }

                // Parse an identifier on its own
                // note: at this point, some primitive types like "true" and "false" are names
                _ => ast::ExprKind::Name(symbol_id),
            },

            // Parse a block
            TokenKind::OpenDelimeter(Delimeter::Brace) => {
                let block = Box::new(self.parse_block(false)?);
                ast::ExprKind::Block(block)
            }

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

            _ => return Err(ParseError::UnexpectedToken(token)),
        };

        // Return node
        Ok(ast::Expr {
            id: self.current_expr_id.next(),
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
