use super::ty::Ty;
use crate::{
    ast,
    id::{BlockId, ExprId, NameId, StatementId},
    lex::LiteralValue,
    session::symbol::Symbol,
    span::Span,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub name: String,
    pub span: Span,
    pub functions: Vec<FnDef>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub id: BlockId,
    pub statements: Vec<Statement>,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub id: StatementId,
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Expr(Box<Expr>),
    Return(Option<Box<Expr>>),

    /// If statement (has no else block)
    If(Box<Expr>, Box<Block>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub span: Span,
    pub kind: ExprKind,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Binary(ast::BinaryOpt, (Box<Expr>, Box<Expr>)),
    Comparison(ast::ComparisonOpt, (Box<Expr>, Box<Expr>)),
    Logical(ast::LogicalOpt, (Box<Expr>, Box<Expr>)),
    Unary(ast::UnaryOpt, Box<Expr>),
    FnCall(Symbol, Vec<Expr>),
    Name(NameId),
    Literal(LiteralValue),
    Assign(NameId, Box<Expr>),
    Let(NameId, Box<Expr>),
    Block(Box<Block>),

    /// If expression (has else block)
    If(Box<Expr>, Box<Block>, Box<Block>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDef {
    pub decl: Box<FnDecl>,
    pub body: Box<FnBody>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnBody {
    pub block: Box<Block>,
    pub param_names: Vec<NameId>,
    pub has_return: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDecl {
    pub name: Symbol,
    pub sig: FnSig,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnSig {
    pub params: Vec<Param>,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Param {
    pub name: Symbol,
    pub ty: Ty,
}
