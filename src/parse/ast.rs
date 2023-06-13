use std::{
    fmt::{Debug, Display},
    sync::RwLockReadGuard,
};

use owo_colors::OwoColorize;

use crate::{
    lex::LiteralValue,
    session::{Session, SessionRef, StringID, SymbolID},
};

type NodeRef = Box<Node>;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// A literal value (int, float, string)
    Literal(LiteralValue),

    /// A reference to a variable in the scope
    Variable(SymbolID),

    /// A reference to a type
    Ty(SymbolID),

    /// A block of statements
    Block { statements: Vec<NodeRef> },

    /// A module (top level parse item)
    Module { declarations: Vec<NodeRef> },

    /// A binary operation with two operands
    BinOpt {
        operation: BinOpt,
        operands: (NodeRef, NodeRef),
    },

    /// A unary operation with a single operand
    UnaOpt { operation: UnaOpt, operand: NodeRef },

    /// A function definition (the declaration and the body)
    FunctionDefinition {
        ident: SymbolID,
        arguments: Vec<NodeRef>,
        return_ty: Option<NodeRef>,
        body: NodeRef,
    },

    /// A function invocation, calling a function
    FunctionInvocation {
        ident: SymbolID,
        arguments: Vec<NodeRef>,
    },

    /// A variable declaration (a name and type, doesn't include an assignment)
    VariableDeclaration { name: SymbolID, ty: NodeRef },

    /// A variable definition: A name and optionally a type followed by an expression to bind to it
    /// this is prefixed with `let`
    VariableDefinition {
        name: SymbolID,
        ty: Option<NodeRef>,
        expr: NodeRef,
    },

    /// A variable assignment. Assigning a value to a variable but cannot change the type.
    /// this has no `let`
    VariableAssignment { name: SymbolID, expr: NodeRef },

    /// A return statement. Has an optional expression to return a value
    Return {
        expr: Option<NodeRef>,
        implicit: bool,
    },
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOpt {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaOpt {
    Negate,
}
