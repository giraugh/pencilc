use std::collections::HashMap;

use crate::{
    id::{Idx, NameId},
    session::symbol::Symbol,
};

use super::{ty::Ty, unify::Unifier};

#[derive(Debug)]
pub struct TyEnv {
    pub unifier: Unifier,
    scope_stack: Vec<Scope>,
    current_name_id: NameId,
}

#[derive(Debug, Default)]
pub struct Scope {
    namespace: Namespace,
    bindings: HashMap<NameId, Ty>,
}

#[derive(Debug, Default)]
struct Namespace {
    names: HashMap<Symbol, NameId>,
}

type Binding = (NameId, Ty);

impl TyEnv {
    pub fn new() -> Self {
        Self {
            // Create a unifier to store type inference stuff
            unifier: Unifier::new(),

            // We initialise an empty scope stack
            // this could eventually use like a global scope or something idk
            scope_stack: vec![],

            // Init current name to 0
            current_name_id: Default::default(),
        }
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        self.scope_stack
            .last_mut()
            .expect("Should always be a scope")
    }

    pub fn new_scope(&mut self) -> &mut Scope {
        self.scope_stack.push(Default::default());
        self.current_scope()
    }

    pub fn pop_scope(&mut self) -> Scope {
        self.scope_stack.pop().expect("Should always be a scope")
    }

    pub fn get_ident(&mut self, symbol: Symbol) -> Option<Binding> {
        // We start at the top of the scope stack and work our way down
        for scope in self.scope_stack.iter().rev() {
            if let Some(binding) = scope.get_ident(symbol.clone()) {
                return Some(binding);
            }
        }

        None
    }

    /// Declare an identifier in the current scope with the given type
    pub fn declare_ident(&mut self, symbol: Symbol, ty: &Ty) -> NameId {
        // Declare name
        let name_id = self.declare_name(symbol);

        // Add binding for type
        self.current_scope().bindings.insert(name_id, ty.clone());

        // Return name and type
        name_id
    }

    /// Declare a name in the space. Will redeclare if necessary
    fn declare_name(&mut self, symbol: Symbol) -> NameId {
        let name_id = self.current_name_id.next();
        self.current_scope().namespace.names.insert(symbol, name_id);
        name_id
    }
}

impl Scope {
    /// Get the name and type from a symbol in the current scope
    fn get_ident(&self, symbol: Symbol) -> Option<Binding> {
        let name_id = self.namespace.get_name(symbol)?;
        let ty = self.bindings.get(&name_id)?.clone();
        Some((name_id, ty))
    }
}

impl Namespace {
    fn get_name(&self, symbol: Symbol) -> Option<NameId> {
        self.names.get(&symbol).cloned()
    }
}
