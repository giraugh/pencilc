type NodeRef = Box<Node>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Expr,
    Literal,
    BinOpt {
        operation: BinOpt,
        operands: (NodeRef, NodeRef),
    },
    Block,
    FunctionDeclaration {
        ident: String, /* TODO */
        body: NodeRef,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOpt {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
    Modulo,
}
