use std::{fmt::Display, sync::RwLockReadGuard};

use owo_colors::OwoColorize;

use crate::{
    lex::LiteralValue,
    parse::ast::Node,
    session::{Session, SessionRef, StringID, SymbolID},
};

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
                arguments,
                body,
                return_ty,
                ..
            } => vec![
                ("arguments", arguments.clone()),
                (
                    "return_ty",
                    return_ty.clone().map_or(vec![], |rty| vec![rty]),
                ),
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
