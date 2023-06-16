pub mod tir;
pub mod ty;
mod ty_env;
use std::collections::{HashMap, HashSet};
use ty_env::TyEnv;

use ty::*;

use crate::{ast, error::TypeError, id::SymbolId};

struct Scope {
    bindings: Vec<(SymbolId, Ty)>,
}

pub struct Tyc {
    scope_stack: Vec<Scope>,
    func_sigs: HashMap<SymbolId, tir::FnSig>,
}

type Result<T> = std::result::Result<T, TypeError>;

impl Tyc {
    pub fn new() -> Self {
        Self {
            scope_stack: Default::default(),
            func_sigs: Default::default(),
        }
    }

    pub fn resolve_type_expr(&mut self, ty_expr: Box<ast::TyExpr>) -> Result<Ty> {
        match ty_expr.kind {
            ast::TyExprKind::Name(ident) => {
                // Is it a primitive?
                if let Some(primitive) = Option::<PrimitiveTy>::from(ident) {
                    Ok(Ty::Primitive(primitive))
                } else {
                    // todo: is this where type variables are introduced instead?
                    Err(TypeError::UnknownType(ident))
                }
            }
        }
    }

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
                    let body = self.typecheck_root_block(fn_def.body, fn_sig.ty.clone())?;

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
                uniq_param_names.insert(param.name);
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

    /// Typecheck a block that has no parent block scope,
    /// for now this is just function bodies.
    pub fn typecheck_root_block(
        &mut self,
        block: Box<ast::Block>,
        expected_ty: Ty,
    ) -> Result<tir::Block> {
        let constraints = todo!();

        todo!("tc block")
    }

    /// Typecheck a block that *may* exist inside a parent block scope
    pub fn typecheck_block(&mut self, block: Box<ast::Block>) -> Result<tir::Block> {
        todo!()
    }
}
