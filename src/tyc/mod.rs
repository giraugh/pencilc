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
    func_sigs: HashMap<Symbol, tir::FnSig>,
    ty_env: Option<TyEnv>,
}

type Result<T> = std::result::Result<T, TypeError>;

impl Tyc {
    pub fn new() -> Self {
        Self {
            func_sigs: Default::default(),
            ty_env: None,
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

    pub fn resolve_type_expr(&mut self, ty_expr: Box<ast::TyExpr>) -> Result<Ty> {
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
                    ty: self.resolve_type_expr(ty_expr)?,
                }),
                None => Err(TypeError::MissingParameterType(param.clone())),
            })
            .collect();
        let params = params?;

        // Parse the return type
        let ty = match fn_decl.ty {
            Some(ty_expr) => self.resolve_type_expr(ty_expr)?,
            None => Ty::Never,
        };

        Ok(tir::FnDecl {
            name: fn_decl.name,
            span: fn_decl.span,
            sig: tir::FnSig { params, ty },
        })
    }

    /// Type check a top level body
    /// Effectively a block that has no parent block scope,
    /// (for now this is just function bodies)
    // TODO: should this return a "Body" rather than a "Block"?
    pub fn typecheck_function_body(
        &mut self,
        name: Symbol,
        block: Box<ast::Block>,
        sig: tir::FnSig,
    ) -> Result<tir::Block> {
        // Create a new type environment for this body
        self.ty_env.replace(TyEnv::new());

        // Create a top level scope
        self.ty_env().new_scope();

        // Add params to scope
        for param in sig.params {
            self.ty_env()
                .current_scope()
                .declare_ident(param.name, &param.ty);
        }

        // Typecheck the inner block
        // note: we dont let this fn call manage the scope, we do that for the top level scope
        let typed_block = self.typecheck_block(block, false)?;

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
        Ok(normalized_block)
    }

    /// Typecheck a block that *may* exist inside a parent block scope
    pub fn typecheck_block(
        &mut self,
        block: Box<ast::Block>,
        create_scope: bool,
    ) -> Result<tir::Block> {
        // Start a new scope
        if create_scope {
            self.ty_env().new_scope();
        }

        // Create infer type for block return
        let mut saw_return = false;
        let block_ty = self
            .ty_env()
            .unifier
            .new_variable(InferValueKind::General)
            .to_ty();

        // Parse each statement
        let mut statements = Vec::new();
        for (is_last, statement) in block.statements.into_iter().mark_last() {
            // Typecheck dependent on statement type (right now theres only env expressions anyway)
            let kind = match statement.kind {
                // Typecheck an expression
                ast::StatementKind::Expr(expr) => {
                    // Typecheck the expression
                    let expr = self.typecheck_expr(expr)?;

                    // Is this an explicit return?
                    match expr.kind {
                        tir::ExprKind::Return(_) => {
                            saw_return = true;
                            self.ty_env().unifier.unify_ty_ty(&expr.ty, &block_ty)?;
                        }
                        _ => {}
                    }

                    // An implicit return?
                    if is_last && !statement.has_semi {
                        saw_return = true;
                        self.ty_env().unifier.unify_ty_ty(&expr.ty, &block_ty)?;
                    }

                    // Create statement kind
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
        let block_ty = if saw_return { block_ty } else { Ty::Never };

        // Pop the scope
        if create_scope {
            self.ty_env().pop_scope();
        }

        Ok(tir::Block {
            ty: block_ty,
            statements,
            id: block.id,
            span: block.span,
        })
    }

    pub fn typecheck_expr(&mut self, expr: Box<ast::Expr>) -> Result<tir::Expr> {
        // Typecheck kind
        let (kind, ty) = match expr.kind {
            ast::ExprKind::Binary(op, (expr1, expr2)) => {
                // Typecheck the expression on their own
                let expr1 = Box::new(self.typecheck_expr(expr1)?);
                let expr2 = Box::new(self.typecheck_expr(expr2)?);

                // They should be the same type
                self.equate_tys(&expr1.ty, &expr2.ty)?;

                // The result type should be the same as the input types
                // TODO: is this right? It feels right
                let ty = expr1.ty.clone();

                (tir::ExprKind::Binary(op, (expr1, expr2)), ty)
            }

            ast::ExprKind::Unary(op, expr) => {
                // Typecheck the expression
                let expr = Box::new(self.typecheck_expr(expr)?);

                // The result type should be the same as the input type
                // TODO: is this right? It feels right
                let ty = expr.ty.clone();

                (tir::ExprKind::Unary(op, expr), ty)
            }

            ast::ExprKind::Assign(symbol_id, expr) => {
                // Get ident
                let (name_id, ident_ty) = self
                    .ty_env()
                    .get_ident(symbol_id.clone())
                    .ok_or(TypeError::UnknownIdent(symbol_id))?;

                // Typecheck expr
                let expr = Box::new(self.typecheck_expr(expr)?);

                // Expr should match ident type
                self.equate_tys(&ident_ty, &expr.ty)?;

                (tir::ExprKind::Assign(name_id, expr), ident_ty)
            }

            ast::ExprKind::Return(expr) => {
                let expr = if let Some(expr) = expr {
                    Some(Box::new(self.typecheck_expr(expr)?))
                } else {
                    None
                };

                (tir::ExprKind::Return(expr), Ty::Never)
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
                        let expr = self.typecheck_expr(Box::new(param_expr))?;
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
                let expr = self.typecheck_expr(expr)?;

                // If the binding has a type, it must unify with the expression
                if let Some(binding_ty) = binding.ty {
                    let ty = self.resolve_type_expr(binding_ty)?;
                    self.equate_tys(&ty, &expr.ty)?;
                }

                // Create a declaration in scope
                let name_id = self
                    .ty_env()
                    .current_scope()
                    .declare_ident(binding.name, &expr.ty);

                // Note: the "type" of this expression is Never because a let isn't
                (tir::ExprKind::Let(name_id, Box::new(expr)), Ty::Never)
            }

            // Type check a block expression
            ast::ExprKind::Block(block) => {
                let block = self.typecheck_block(block, true)?;
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
