mod norm;
pub mod tir;
pub mod ty;
mod ty_env;
mod unify;

pub use unify::InferValueKind;

use crate::{ast, error::TypeError, lex::LiteralValue, session::symbol::Symbol};
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};
use ty::*;

use self::ty_env::TyEnv;

pub struct Tyc {
    /// The signature of all top level functions in the module
    /// These are collected before the body of the functions are typechecked
    /// this is so that we know the type of function calls within function bodies
    func_sigs: HashMap<Symbol, tir::FnSig>,

    /// The current type environment. Is initialised when typechecking a function
    /// contains inference info, the namespace and scope stack etc
    ty_env: Option<TyEnv>,

    /// The type of the current outer block
    /// there is only one at a time but there may not be one at a given time
    /// this is used so that return expressions deep in a function ast can
    /// inform the function return type
    /// currently the only type of outer block is a function but it could be expanded
    /// to lambdas etc
    current_outer_type: Option<Ty>,
}

type Result<T> = std::result::Result<T, TypeError>;

impl Tyc {
    pub fn new() -> Self {
        Self {
            func_sigs: Default::default(),
            ty_env: None,
            current_outer_type: None,
        }
    }

    fn ty_env(&mut self) -> &mut TyEnv {
        self.ty_env.as_mut().unwrap()
    }

    fn lookup_func_signature(&self, symbol: &Symbol) -> Result<tir::FnSig> {
        self.func_sigs
            .get(symbol)
            .ok_or(TypeError::UnknownIdent(symbol.clone()))
            .cloned()
    }

    fn equate_tys(&mut self, ty_a: &Ty, ty_b: &Ty) -> Result<()> {
        // Are one or more of them, infer types?
        self.ty_env().unifier.unify_ty_ty(ty_a, ty_b)
    }

    pub fn resolve_type_expr(&mut self, ty_expr: ast::TyExpr) -> Result<Ty> {
        match ty_expr.kind {
            ast::TyExprKind::Name(ident) => {
                // Is it a primitive?
                if let Some(primitive) = Option::<PrimitiveTy>::from(ident.clone()) {
                    Ok(Ty::Primitive(primitive))
                } else {
                    Err(TypeError::UnknownType(ident))
                }
            }
        }
    }

    /// Entry point for typechecking. Checks each item within the module
    pub fn typecheck_module(&mut self, module: ast::Module) -> Result<tir::Module> {
        // First parse the signature of all of the functions
        // We need this when evaluating each function for when they call each other
        for item in module.items.clone() {
            match item.kind {
                ast::ItemKind::FnDef(fn_def) => {
                    let decl = self.typecheck_function_declaration(fn_def.decl)?;
                    self.func_sigs.insert(decl.name, decl.sig);
                }
            }
        }

        // Now we type check each function
        let functions: Result<Vec<tir::FnDef>> = module
            .items
            .into_iter()
            .map(|item| match item.kind {
                ast::ItemKind::FnDef(fn_def) => {
                    // Get expected return type from func sigs
                    let fn_sig = self.func_sigs.get(&fn_def.decl.name).unwrap().clone();
                    let body = self.typecheck_function_body(
                        fn_def.decl.name.clone(),
                        fn_def.body,
                        fn_sig.clone(),
                    )?;

                    Ok(tir::FnDef {
                        body: Box::new(body),
                        decl: Box::new(tir::FnDecl {
                            span: fn_def.decl.span,
                            name: fn_def.decl.name,
                            sig: fn_sig,
                        }),
                    })
                }
            })
            .collect();
        let functions = functions?;

        Ok(tir::Module {
            name: module.name,
            span: module.span,
            functions,
        })
    }

    /// Typecheck a function declaration (does not include the function body)
    pub fn typecheck_function_declaration(
        &mut self,
        fn_decl: Box<ast::FnDecl>,
    ) -> Result<tir::FnDecl> {
        // Check that all params have a unique name and a type expr
        let mut uniq_param_names = HashSet::new();
        for param in fn_decl.params.iter() {
            // Unique name?
            if uniq_param_names.contains(&param.name) {
                return Err(TypeError::RepeatedParameterName(param.clone()));
            } else {
                uniq_param_names.insert(param.name.clone());
            }
        }

        // Parse the types of each param
        let params: Result<Vec<tir::Param>> = fn_decl
            .params
            .into_iter()
            .map(|param| match param.ty {
                Some(ty_expr) => Ok(tir::Param {
                    name: param.name,
                    ty: self.resolve_type_expr(*ty_expr)?,
                }),
                None => Err(TypeError::MissingParameterType(param.clone())),
            })
            .collect();
        let params = params?;

        // Parse the return type
        let ty = match fn_decl.ty {
            Some(ty_expr) => self.resolve_type_expr(*ty_expr)?,
            None => Ty::Never,
        };

        Ok(tir::FnDecl {
            name: fn_decl.name,
            span: fn_decl.span,
            sig: tir::FnSig { params, ty },
        })
    }

    /// Type check a function body
    pub fn typecheck_function_body(
        &mut self,
        name: Symbol,
        block: Box<ast::Block>,
        sig: tir::FnSig,
    ) -> Result<tir::FnBody> {
        // Create a new type environment for this body
        self.ty_env.replace(TyEnv::new());

        // Create a top level scope
        self.ty_env().new_scope();

        // Add params to scope
        let param_names = sig
            .params
            .iter()
            .map(|param| self.ty_env().declare_ident(param.name.clone(), &param.ty))
            .collect();

        // Typecheck the outer block
        let (typed_block, has_return) = self.typecheck_outer_block(*block)?;

        // Check return type
        self.equate_tys(&typed_block.ty, &sig.ty)
            .map_err(|e| match e {
                TypeError::CannotUnify(expected, actual) => {
                    TypeError::ExpectedReturnType(name, actual, expected)
                }
                e => e,
            })?;

        // Then pop the final scope to clean up
        self.ty_env().pop_scope();

        // Now we go back through the module to normalize any inferences
        let normalized_block = self.norm_block(typed_block, true)?;

        // Return type
        Ok(tir::FnBody {
            block: Box::new(normalized_block),
            has_return,
            param_names,
        })
    }

    /// Typecheck a block that doesnt exist inside a parent block scope
    pub fn typecheck_outer_block(
        &mut self,
        block: ast::Block,
    ) -> Result<(tir::Block, bool /* has return */)> {
        // Set the current outer block type to a new inference variable
        self.current_outer_type = Some(
            self.ty_env()
                .unifier
                .new_variable(InferValueKind::General)
                .to_ty(),
        );

        // Typecheck the inner block
        let mut block = self.typecheck_inner_block(block)?;

        // Equate the return types
        // (i.e the implicit value of the block needs to match any returns within the block)
        // because if the inner has `Never` then it means it doesn't have a final return
        // If the block has a type then it must equate
        if !matches!(block.ty, Ty::Never) {
            let cot = self.current_outer_type.as_ref().unwrap().clone();
            self.equate_tys(&block.ty, &cot)?;
        }

        // If the block has no explicit final return statement, then we need to create one
        // Note: if there is also no implicit return we don't create one here
        let mut has_return = false;
        let last_statement_kind = block.statements.last().map(|l| l.kind.clone());
        match last_statement_kind {
            Some(tir::StatementKind::Return(_)) => {
                has_return = true;
            }
            Some(tir::StatementKind::Expr(expr)) => {
                if !matches!(block.ty, Ty::Never) {
                    // We transform the final statement into a return
                    let last_stmnt = block.statements.last_mut().unwrap();
                    *last_stmnt = tir::Statement {
                        kind: tir::StatementKind::Return(Some(expr)),
                        ..*last_stmnt
                    };

                    has_return = true;
                }
            }
            None => {}
        }

        // Update the type of the block
        let ty = self.current_outer_type.clone().unwrap_or(Ty::Never);
        let outer_block = tir::Block { ty, ..block };

        // Discard the outer type
        self.current_outer_type = None;

        // Return the block
        Ok((outer_block, has_return))
    }

    pub fn typecheck_inner_block(&mut self, block: ast::Block) -> Result<tir::Block> {
        // Start a new scope
        self.ty_env().new_scope();

        // Create infer type for block type
        let mut block_ty = None;

        // Parse each statement
        let mut statements = Vec::new();
        for (is_last, statement) in block.statements.into_iter().mark_last() {
            // Typecheck dependent on statement type (right now theres only env expressions anyway)
            let kind = match statement.kind {
                // Typecheck an explicit return
                ast::StatementKind::Return(expr) => {
                    // Typecheck the expression
                    let expr = if let Some(expr) = expr {
                        let expr = self.typecheck_expr(*expr)?;
                        Some(Box::new(expr))
                    } else {
                        None
                    };

                    // tell current outer block that we saw a return with this type
                    let effective_ty = expr.clone().map(|e| e.ty).unwrap_or(Ty::Never);
                    if let Some(cot) = self.current_outer_type.clone() {
                        self.equate_tys(&cot, &effective_ty)?;
                    }

                    tir::StatementKind::Return(expr)
                }

                // Typecheck an expression
                ast::StatementKind::Expr(expr) => {
                    // Typecheck the expression
                    let expr = self.typecheck_expr(*expr)?;

                    // If this is the final expression and not a semi then its the type of the
                    // block
                    if is_last && !statement.has_semi {
                        block_ty = Some(expr.ty.clone());
                    }

                    tir::StatementKind::Expr(Box::new(expr))
                }
            };

            // Create and save statement
            statements.push(tir::Statement {
                kind,
                id: statement.id,
                span: statement.span,
            })
        }

        // After parsing all of them, can we infer the return type?
        let block_ty = block_ty.unwrap_or(Ty::Never);

        // Pop the scope
        self.ty_env().pop_scope();

        Ok(tir::Block {
            ty: block_ty,
            statements,
            id: block.id,
            span: block.span,
        })
    }

    pub fn typecheck_expr(&mut self, expr: ast::Expr) -> Result<tir::Expr> {
        // Typecheck kind
        let (kind, ty) = match expr.kind {
            ast::ExprKind::Binary(op, (expr1, expr2)) => {
                // Typecheck the expression on their own
                let expr1 = Box::new(self.typecheck_expr(*expr1)?);
                let expr2 = Box::new(self.typecheck_expr(*expr2)?);

                // They should be the same type
                self.equate_tys(&expr1.ty, &expr2.ty)?;

                // The result type should be the same as the input types
                let ty = expr1.ty.clone();

                (tir::ExprKind::Binary(op, (expr1, expr2)), ty)
            }

            ast::ExprKind::Comparison(op, (expr1, expr2)) => {
                // Typecheck the expression on their own
                let expr1 = Box::new(self.typecheck_expr(*expr1)?);
                let expr2 = Box::new(self.typecheck_expr(*expr2)?);

                // They should be the same type
                self.equate_tys(&expr1.ty, &expr2.ty)?;

                // The result type will be a boolean
                let ty = Ty::Primitive(PrimitiveTy::Bool);

                (tir::ExprKind::Comparison(op, (expr1, expr2)), ty)
            }

            ast::ExprKind::Logical(op, (expr1, expr2)) => {
                // Typecheck the expression on their own
                let expr1 = Box::new(self.typecheck_expr(*expr1)?);
                let expr2 = Box::new(self.typecheck_expr(*expr2)?);

                // They should be booleans
                self.equate_tys(&expr1.ty, &Ty::Primitive(PrimitiveTy::Bool))?;
                self.equate_tys(&expr2.ty, &Ty::Primitive(PrimitiveTy::Bool))?;

                // The result type will be a boolean
                let ty = Ty::Primitive(PrimitiveTy::Bool);

                (tir::ExprKind::Logical(op, (expr1, expr2)), ty)
            }

            ast::ExprKind::Unary(op, expr) => {
                // Typecheck the expression
                let expr = Box::new(self.typecheck_expr(*expr)?);

                // The result type should be the same as the input type
                let ty = expr.ty.clone();

                // In the case of negation, the argument must also be negatable
                // right now that is just a signed int
                self.equate_tys(&ty, &Ty::Primitive(PrimitiveTy::SInt))?;

                (tir::ExprKind::Unary(op, expr), ty)
            }

            ast::ExprKind::Assign(symbol_id, expr) => {
                // Get ident
                let (name_id, ident_ty) = self
                    .ty_env()
                    .get_ident(symbol_id.clone())
                    .ok_or(TypeError::UnknownIdent(symbol_id))?;

                // Typecheck expr
                let expr = Box::new(self.typecheck_expr(*expr)?);

                // Expr should match ident type
                self.equate_tys(&ident_ty, &expr.ty)?;

                (tir::ExprKind::Assign(name_id, expr), ident_ty)
            }

            ast::ExprKind::Name(symbol_id) => {
                let (name_id, ty) = self
                    .ty_env()
                    .get_ident(symbol_id.clone())
                    .ok_or(TypeError::UnknownIdent(symbol_id))?;
                (tir::ExprKind::Name(name_id), ty)
            }

            ast::ExprKind::FnCall(symbol_id, param_exprs) => {
                // Lookup function signature
                let sig = self.lookup_func_signature(&symbol_id)?;

                // Match each param up and equate the types
                let param_exprs: Result<Vec<tir::Expr>> = param_exprs
                    .into_iter()
                    .zip(sig.params)
                    .map(|(param_expr, sig_param)| {
                        let expr = self.typecheck_expr(param_expr)?;
                        self.equate_tys(&expr.ty, &sig_param.ty)?;
                        Ok(expr)
                    })
                    .collect();
                let param_exprs = param_exprs?;

                // Return fn call node
                (tir::ExprKind::FnCall(symbol_id, param_exprs), sig.ty)
            }

            // Type check a literal
            ast::ExprKind::Literal(literal) => {
                let ty = match literal {
                    LiteralValue::Str(_) => Ty::Primitive(PrimitiveTy::Str),
                    LiteralValue::Float(_) => Ty::Primitive(PrimitiveTy::Float),
                    LiteralValue::Bool(_) => Ty::Primitive(PrimitiveTy::Bool),
                    LiteralValue::Int(_) => self
                        .ty_env()
                        .unifier
                        .new_variable(InferValueKind::Integral)
                        .to_ty(),
                };

                (tir::ExprKind::Literal(literal), ty)
            }

            // Type check a let binding
            ast::ExprKind::Let(binding, expr) => {
                // First parse the expr
                let expr = self.typecheck_expr(*expr)?;

                // If the binding has a type, it must unify with the expression
                if let Some(binding_ty) = binding.ty {
                    let ty = self.resolve_type_expr(*binding_ty)?;
                    self.equate_tys(&ty, &expr.ty)?;
                }

                // Create a declaration in scope
                let name_id = self.ty_env().declare_ident(binding.name, &expr.ty);

                // Note: the "type" of this expression is Never because a let isn't
                (tir::ExprKind::Let(name_id, Box::new(expr)), Ty::Never)
            }

            // Type check a block expression
            ast::ExprKind::Block(block) => {
                let block = self.typecheck_inner_block(*block)?;
                let ty = block.ty.clone();
                (tir::ExprKind::Block(Box::new(block)), ty)
            }
        };

        // Return expression node
        Ok(tir::Expr {
            kind,
            ty,
            span: expr.span,
            id: expr.id,
        })
    }
}

trait MarkLastIterExt<T: Iterator>: Iterator<Item = T::Item> {
    fn mark_last(self) -> MarkLast<T>;
}

struct MarkLast<T: Iterator>(Peekable<T>);

impl<T: Iterator> MarkLastIterExt<T> for T {
    fn mark_last(self) -> MarkLast<T> {
        MarkLast(self.peekable())
    }
}

impl<T: Iterator> Iterator for MarkLast<T> {
    type Item = (bool, T::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.next();
        let has_next_item = self.0.peek().is_some();
        item.and_then(|item| Some((!has_next_item, item)))
    }
}
