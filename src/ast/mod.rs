use crate::id::{BlockId, ExprId, StatementId};
use crate::session::symbol::Symbol;
use crate::{lex::LiteralValue, span::Span};
use strum_macros::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub items: Vec<Item>,
    pub span: Span,
    pub name: String,
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
    pub name: Symbol,
    pub ty: Option<Box<TyExpr>>,
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Binary(BinaryOpt, (Box<Expr>, Box<Expr>)),
    Comparison(ComparisonOpt, (Box<Expr>, Box<Expr>)),
    Logical(LogicalOpt, (Box<Expr>, Box<Expr>)),
    Unary(UnaryOpt, Box<Expr>),
    FnCall(Symbol, Vec<Expr>),
    Name(Symbol),
    Literal(LiteralValue),
    Assign(Symbol, Box<Expr>),
    Let(Box<Binding>, Box<Expr>),
    Block(Box<Block>),

    /// If expression (cond, block-if-true, block-if-false)
    If(Box<Expr>, Box<Block>, Box<Block>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDef {
    pub decl: Box<FnDecl>,
    pub body: Box<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDecl {
    pub name: Symbol,
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
    Name(Symbol),
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum LogicalOpt {
    #[strum(serialize = "&&")]
    And,

    #[strum(serialize = "||")]
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum ComparisonOpt {
    #[strum(serialize = "==")]
    Equal,

    #[strum(serialize = "!=")]
    NotEqual,

    #[strum(serialize = ">")]
    GreaterThan,

    #[strum(serialize = ">=")]
    GreaterThanOrEqual,

    #[strum(serialize = "<")]
    LessThan,

    #[strum(serialize = "<=")]
    LessThanOrEqual,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum BinaryOpt {
    #[strum(serialize = "+")]
    Add,

    #[strum(serialize = "-")]
    Subtract,

    #[strum(serialize = "*")]
    Multiply,

    #[strum(serialize = "/")]
    Divide,

    #[strum(serialize = "^")]
    Exponentiate,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum UnaryOpt {
    #[strum(serialize = "-")]
    Negate,
}
