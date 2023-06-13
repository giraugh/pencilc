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

pub struct PrettyPrint<'a>(Node, SessionRef<'a>);

impl<'a> PrettyPrint<'a> {
    pub fn new(node: Node, session: SessionRef<'a>) -> Self {
        Self(node, session)
    }
}

impl Display for PrettyPrint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack = vec![(0, Box::new(self.0.clone()), "".to_owned())];
        let session = self.1.try_read().unwrap();

        while let Some((depth, node, label)) = stack.pop() {
            // Print the node name
            write!(
                f,
                "{}{} {}\n",
                "    ".repeat(depth),
                format!("<{}>", label).white().dimmed(),
                node.get_name(&session)
            )?;

            // Append its children to the stack
            for (group_name, children) in node.get_children_groups() {
                for (i, child) in children.iter().enumerate().rev() {
                    stack.push((depth + 1, child.clone(), format!("{}#{}", group_name, i)));
                }
            }
        }

        Ok(())
    }
}

trait PrettyPrintNode {
    fn get_name(&self, session: &RwLockReadGuard<Session<'_>>) -> String;
    fn get_children_groups(&self) -> Vec<(&'static str, Vec<Box<Node>>)>;
}

impl PrettyPrintNode for Node {
    fn get_name(&self, session: &RwLockReadGuard<Session>) -> String {
        match self {
            Node::Literal(LiteralValue::Str(string_id)) => {
                format!("Literal({:?})", session.string(string_id))
            }
            Node::Literal(value) => {
                format!("Literal({:?})", value)
            }
            Node::Variable(symbol_id) => {
                format!("Variable(${})", session.symbol(symbol_id))
            }
            Node::Ty(symbol_id) => format!("Ty(${})", session.symbol(symbol_id)),
            Node::Block { .. } => "Block".into(),
            Node::Module { .. } => "Module".into(),
            Node::BinOpt { operation, .. } => format!("BinOpt({:?})", operation),
            Node::UnaOpt { operation, .. } => format!("UnaOpt({:?})", operation),
            Node::FunctionDefinition { ident, .. } => {
                format!("FunctionDefinition(${})", session.symbol(ident))
            }
            Node::FunctionInvocation { ident, .. } => {
                format!("FunctionInvocation(${})", session.symbol(ident))
            }
            Node::VariableDeclaration { name, .. } => {
                format!("VariableDeclaration(${})", session.symbol(name))
            }
            Node::VariableDefinition { name, .. } => {
                format!("VariableDefinition(${})", session.symbol(name))
            }
            Node::VariableAssignment { name, .. } => {
                format!("VariableAssignment(${})", session.symbol(name))
            }
            Node::Return { implicit, .. } => if *implicit {
                "ImplicitReturn"
            } else {
                "Return"
            }
            .into(),
        }
    }

    fn get_children_groups(&self) -> Vec<(&'static str, Vec<Box<Node>>)> {
        match self {
            Node::Block { statements } => vec![("statements", statements.clone())],
            Node::Module { declarations } => vec![("declarations", declarations.clone())],
            Node::UnaOpt { operand, .. } => vec![("operand", vec![operand.clone()])],
            Node::FunctionInvocation { arguments, .. } => vec![("arguments", arguments.clone())],
            Node::VariableDeclaration { ty, .. } => vec![("ty", vec![ty.clone()])],
            Node::VariableAssignment { expr, .. } => vec![("expr", vec![expr.clone()])],
            Node::BinOpt { operands, .. } => {
                vec![
                    ("left", vec![operands.0.clone()]),
                    ("right", vec![operands.1.clone()]),
                ]
            }
            Node::FunctionDefinition {
                arguments, body, ..
            } => vec![
                ("arguments", arguments.clone()),
                ("body", vec![body.clone()]),
            ],
            Node::Return { expr, .. } => {
                if let Some(expr) = expr {
                    vec![("expr", vec![expr.clone()])]
                } else {
                    vec![]
                }
            }
            Node::VariableDefinition { ty, expr, .. } => {
                if let Some(ty) = ty {
                    vec![("ty", vec![ty.clone()]), ("expr", vec![expr.clone()])]
                } else {
                    vec![("expr", vec![expr.clone()])]
                }
            }
            _ => vec![],
        }
    }
}

trait SessionExt {
    fn symbol(&self, symbol_id: &SymbolID) -> String;
    fn string(&self, string_id: &StringID) -> String;
}

impl<'a> SessionExt for Session<'a> {
    fn symbol(&self, symbol_id: &SymbolID) -> String {
        self.get_symbol(*symbol_id).unwrap().into()
    }
    fn string(&self, string_id: &StringID) -> String {
        self.get_string(*string_id).unwrap().into()
    }
}

// pub struct PrettyPrint<'a>(Node, SessionRef<'a>);
//
// impl<'a> PrettyPrint<'a> {
//     pub fn new(node: Node, session: SessionRef<'a>) -> Self {
//         Self(node, session)
//     }
// }
//
// impl Display for PrettyPrint<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.0.pretty_fmt(&self.1.try_read().unwrap(), 0, f)?;
//         Ok(())
//     }
// }
//
// trait PrettyPrintNode: Debug {
//     fn pretty_fmt(
//         &self,
//         session: &RwLockReadGuard<Session<'_>>,
//         depth: usize,
//         f: &mut std::fmt::Formatter<'_>,
//     ) -> std::fmt::Result;
// }
//
// macro_rules! print_children {
//     ($f: expr, $depth: expr, $sess: expr, $label: expr, $children: expr) => {{
//         write!($f, "{} {{\n", $label)?;
//         for child in $children {
//             child.pretty_fmt(&$sess, $depth + 1, $f)?;
//             write!($f, "\n")?;
//         }
//         write!($f, "}}\n")?;
//     }};
// }
//
// impl PrettyPrintNode for Node {
//     fn pretty_fmt(
//         &self,
//         session: &RwLockReadGuard<Session>,
//         depth: usize,
//         f: &mut std::fmt::Formatter<'_>,
//     ) -> std::fmt::Result {
//         match self {
//             Node::Literal(LiteralValue::Str(string_id)) => {
//                 write!(
//                     f,
//                     "{}Literal({:?})",
//                     " ".repeat(depth),
//                     session.get_string(*string_id).unwrap()
//                 )?;
//             }
//
//             Node::Literal(value) => write!(f, "{}Literal({:?})", " ".repeat(depth), value)?,
//
//             Node::Variable(symbol_id) => {
//                 write!(
//                     f,
//                     "{}Variable({})",
//                     " ".repeat(depth),
//                     session.get_symbol(*symbol_id).unwrap()
//                 )?;
//             }
//
//             Node::Ty(symbol_id) => write!(
//                 f,
//                 "{}Ty({})",
//                 " ".repeat(depth),
//                 session.get_symbol(*symbol_id).unwrap()
//             )?,
//
//             Node::Block { statements } => print_children!(f, depth, session, "Block", statements),
//
//             Node::Module { declarations } => {
//                 print_children!(f, depth, session, "Module", declarations)
//             }
//
//             Node::BinOpt {
//                 operation,
//                 operands,
//             } => {
//                 let (l, r) = operands;
//                 write!(f, "{}{:?}(", " ".repeat(depth), operation);
//                 l.pretty_fmt(session, depth, f)?;
//                 write!(f, "{},", " ".repeat(depth))?;
//                 r.pretty_fmt(session, depth, f)?;
//                 write!(f, "{})", " ".repeat(depth))?;
//             }
//
//             Node::UnaOpt { operation, operand } => {
//                 write!(f, "{:?}(", operation)?;
//                 operand.pretty_fmt(session, depth, f)?;
//                 write!(f, ")")?;
//             }
//             Node::FunctionDefinition {
//                 ident,
//                 arguments,
//                 body,
//             } => {
//                 write!(f, "DefFn({:?}) ", session.get_symbol(*ident).unwrap())?;
//                 print_children!(f, depth, session, "Args", arguments);
//                 body.pretty_fmt(session, depth, f)?;
//             }
//
//             Node::FunctionInvocation { ident, arguments } => {
//                 write!(f, "FnInv({:?})(", session.get_symbol(*ident).unwrap())?;
//                 print_children!(f, depth, session, "Args", arguments);
//                 write!(f, ")");
//             }
//
//             Node::VariableDeclaration { name, ty } => {
//                 write!(f, "Declare({:?} as ", session.get_symbol(*name).unwrap())?;
//                 ty.pretty_fmt(session, depth, f);
//                 write!(f, ")")?;
//             }
//
//             Node::VariableDefinition { name, ty, expr } => {
//                 write!(f, "Define({:?}", session.get_symbol(*name).unwrap())?;
//                 if let Some(ty) = ty {
//                     write!(f, " as ")?;
//                     ty.pretty_fmt(session, depth, f);
//                 }
//                 write!(f, " to be ")?;
//                 expr.pretty_fmt(session, depth, f);
//                 write!(f, ")")?;
//             }
//
//             Node::VariableAssignment { name, expr } => {
//                 write!(f, "Assign({:?} to be ", session.get_symbol(*name).unwrap())?;
//                 expr.pretty_fmt(session, depth, f);
//                 write!(f, ")")?;
//             }
//
//             Node::Return { expr, implicit } => {
//                 write!(f, "{}", if *implicit { "ReturnImp" } else { "Return" })?;
//                 if let Some(expr) = expr {
//                     write!(f, "(")?;
//                     expr.pretty_fmt(session, depth, f);
//                     write!(f, ")")?;
//                 }
//             }
//         };
//
//         Ok(())
//     }
// }
