use crate::{lex::LiteralValue, session::SymbolID};

type NodeRef = Box<Node>;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Expr,
    Literal(LiteralValue),
    Statement,
    Block {
        statements: Vec<NodeRef>,
    },
    Module {
        declarations: Vec<NodeRef>,
    },
    BinOpt {
        operation: BinOpt,
        operands: (NodeRef, NodeRef),
    },
    UnaOpt {
        operation: UnaOpt,
        operand: NodeRef,
    },
    FunctionDefinition {
        ident: SymbolID,
        arguments: Vec<NodeRef>,
        body: NodeRef,
    },
    VariableDeclaration {
        name: SymbolID,
        ty: SymbolID,
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
