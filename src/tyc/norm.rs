use crate::error::TypeError;

use super::{tir, ty::PrimitiveTy, Result, Ty, Tyc};

impl Tyc {
    pub fn norm_block(&mut self, block: tir::Block, fin: bool) -> Result<tir::Block> {
        Ok(tir::Block {
            ty: self.norm_ty(block.ty, fin)?,
            statements: block
                .statements
                .into_iter()
                .map(|stmnt| self.norm_statement(stmnt, fin))
                .collect::<Result<Vec<_>>>()?,
            ..block
        })
    }

    pub fn norm_ty(&mut self, ty: Ty, fin: bool) -> Result<Ty> {
        if fin {
            self.ty_env().unifier.normalize_final(&ty)
        } else {
            Ok(self.ty_env().unifier.normalize(&ty).unwrap_or(ty))
        }
    }

    fn norm_statement(&mut self, statement: tir::Statement, fin: bool) -> Result<tir::Statement> {
        Ok(tir::Statement {
            kind: match statement.kind {
                tir::StatementKind::Expr(expr) => {
                    tir::StatementKind::Expr(Box::new(self.norm_expr(*expr, fin)?))
                }
                tir::StatementKind::Return(Some(expr)) => {
                    tir::StatementKind::Return(Some(Box::new(self.norm_expr(*expr, fin)?)))
                }
                tir::StatementKind::Return(None) => tir::StatementKind::Return(None),
            },
            ..statement
        })
    }

    fn norm_expr(&mut self, expr: tir::Expr, fin: bool) -> Result<tir::Expr> {
        Ok(tir::Expr {
            kind: match expr.kind {
                tir::ExprKind::Block(block) => {
                    tir::ExprKind::Block(Box::new(self.norm_block(*block, fin)?))
                }

                tir::ExprKind::FnCall(symbol_id, exprs) => tir::ExprKind::FnCall(
                    symbol_id,
                    exprs
                        .into_iter()
                        .map(|expr| self.norm_expr(expr, fin))
                        .collect::<Result<Vec<_>>>()?,
                ),

                tir::ExprKind::Binary(opt, (expr1, expr2)) => {
                    let expr1 = Box::new(self.norm_expr(*expr1, fin)?);
                    let expr2 = Box::new(self.norm_expr(*expr2, fin)?);
                    if !expr1.ty.can_do_binop(&opt) {
                        return Err(TypeError::InvalidBinaryOpt(opt, expr1.ty.clone()));
                    }

                    tir::ExprKind::Binary(opt, (expr1, expr2))
                }

                tir::ExprKind::Unary(opt, expr) => {
                    let expr = Box::new(self.norm_expr(*expr, fin)?);
                    if !expr.ty.can_do_unaryop(&opt) {
                        return Err(TypeError::InvalidUnaryOpt(opt, expr.ty.clone()));
                    }
                    tir::ExprKind::Unary(opt, expr)
                }

                tir::ExprKind::Assign(name, expr) => {
                    tir::ExprKind::Assign(name, Box::new(self.norm_expr(*expr, fin)?))
                }

                tir::ExprKind::Let(name, expr) => {
                    tir::ExprKind::Let(name, Box::new(self.norm_expr(*expr, fin)?))
                }

                tir::ExprKind::Comparison(opt, (expr1, expr2)) => {
                    let expr1 = Box::new(self.norm_expr(*expr1, fin)?);
                    let expr2 = Box::new(self.norm_expr(*expr2, fin)?);
                    // TODO: check whether comparisons are valid.
                    // We already know the types are the same. are there things that
                    // cant be compared?

                    tir::ExprKind::Comparison(opt, (expr1, expr2))
                }

                tir::ExprKind::Logical(opt, (expr1, expr2)) => {
                    let expr1 = Box::new(self.norm_expr(*expr1, fin)?);
                    let expr2 = Box::new(self.norm_expr(*expr2, fin)?);

                    // Check that both exprs are booleans
                    if expr1.ty != Ty::Primitive(PrimitiveTy::Bool)
                        || expr2.ty != Ty::Primitive(PrimitiveTy::Bool)
                    {
                        return Err(TypeError::InvalidLogicalOpt(expr.ty.clone()));
                    }

                    tir::ExprKind::Logical(opt, (expr1, expr2))
                }

                // Expr kinds that we don't have to normalize
                k @ tir::ExprKind::Name(_) => k,
                k @ tir::ExprKind::Literal(_) => k,
            },
            ty: self.norm_ty(expr.ty.clone(), fin)?,
            ..expr
        })
    }
}
