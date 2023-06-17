use super::{tir, Result, Ty, Tyc};

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

                tir::ExprKind::Binary(opt, (expr1, expr2)) => tir::ExprKind::Binary(
                    opt,
                    (
                        Box::new(self.norm_expr(*expr1, fin)?),
                        Box::new(self.norm_expr(*expr2, fin)?),
                    ),
                ),

                tir::ExprKind::Unary(opt, expr) => {
                    tir::ExprKind::Unary(opt, Box::new(self.norm_expr(*expr, fin)?))
                }

                tir::ExprKind::Assign(name, expr) => {
                    tir::ExprKind::Assign(name, Box::new(self.norm_expr(*expr, fin)?))
                }

                tir::ExprKind::Let(name, expr) => {
                    tir::ExprKind::Let(name, Box::new(self.norm_expr(*expr, fin)?))
                }

                tir::ExprKind::Return(Some(expr)) => {
                    tir::ExprKind::Return(Some(Box::new(self.norm_expr(*expr, fin)?)))
                }

                kind => kind,
            },
            ty: self.norm_ty(expr.ty.clone(), fin)?,
            ..expr
        })
    }
}
