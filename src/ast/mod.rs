pub mod id;

use crate::{lex::LiteralValue, session::SymbolID, span::Span};

use self::id::{BlockId, ExprId, StatementId};

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Item<K = ItemKind> {
    pub kind: K,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ItemKind {
    FnDef(Box<FnDef>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub id: BlockId,
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub id: StatementId,
    pub kind: StatementKind,
    pub span: Span,
    pub has_semi: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding {
    pub name: SymbolID,
    pub ty: Option<Box<TyExpr>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Let(Box<Binding>, Option<Box<Expr>>),
    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Binary(BinaryOpt, (Box<Expr>, Box<Expr>)),
    Unary(UnaryOpt, Box<Expr>),
    FnCall(SymbolID, Vec<Expr>),
    Name(SymbolID),
    Literal(LiteralValue),
    Assign(SymbolID, Box<Expr>),
    Let(Box<Binding>, Box<Expr>),
    Return(Option<Box<Expr>>),
    Block(Box<Block>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDef {
    pub decl: Box<FnDecl>,
    pub body: Box<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDecl {
    pub name: SymbolID,
    pub params: Vec<Binding>,
    pub ty: Option<Box<TyExpr>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TyExpr {
    pub span: Span,
    pub kind: TyExprKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TyExprKind {
    Name(SymbolID),
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOpt {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpt {
    Negate,
}
