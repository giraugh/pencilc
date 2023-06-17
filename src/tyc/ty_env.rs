use std::collections::HashMap;

use crate::id::{Idx, NameId, SymbolId};

use super::{ty::Ty, unify::Unifier};

#[derive(Debug)]
pub struct TyEnv {
    pub unifier: Unifier,
    scope_stack: Vec<Scope>,
}

#[derive(Debug, Default)]
pub struct Scope {
    namespace: Namespace,
    bindings: HashMap<NameId, Ty>,
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

    pub fn get_ident(&mut self, symbol_id: SymbolId) -> Option<Binding> {
        // We start at the top of the scope stack and work our way down
        for scope in self.scope_stack.iter().rev() {
            if let Some(binding) = scope.get_ident(symbol_id) {
                return Some(binding);
            }
        }

        None
    }

    /*
     * OVER HERE!!!
     * To keep going:
     * - do integral inferences have to be represented in Ty?
     * - when we declare an ident w/ a type that is infer, does that integral infer work its way up
     *   from the expr? e.g in most cases is the type actually defined anyway?
     * - when parsing an integral literal, what type is it?
     * - do we expose the unifier outside of the ty env?
     */

    /*
     *
     * Say im typechecking this
     *
     * let x = (1 + 10) * 2;
     *
     * each of these literals, when parsed will be a Ty::Infer,
     * when we parse the + binop, our two operands will be Infer,
     * in this case what is the strategy? We can create a ty variable
     * to represent the sum but does that help us. Best we can say is that it also
     * must be an integral infer. One thing we can do is unify the two infers as the operands
     * must have the same type. but what is the return type? Can we assume that binops are t -> t
     * -> t?
     *
     * in the case of + this makes sense:
     *   (int + int) -> int
     *   (flt + flt) -> flt
     *
     * in the case of product this makes sense
     *   (int * int) -> int
     *   (flt * flt) -> flt
     *
     * its slightly less clear for division as (int / int) may not be valid. UNLESS we define
     * division as requiring to be the same output type. I think this just delays the problem
     * however.
     *
     * lets work through the whole example w/ the assumption that we just create an infer when one
     * of our operands is infer.
     *
     * tc: 1 -> T?: int
     * tc: 10 -> U?: int
     * T <=> U
     * tc: (1 + 10) -> V?   (if we can say that this has to be :int then we will be fine)
     * tc: (1 + 10) * 2 -> W?: int, X?
     * (V <=> W)
     * result: x is bound to Inf<?>
     *
     * can we think of binary operations like functions? They have a type like +(int, int) -> int?
     * I guess they are kind of like polymorphic functions though.
     *
     * I think that we can't *always* make strong assumptions about the type of a binary operation
     * but we might be able to in some cases. If we have Infer<Int> + Infer<Int> then it makes sense that the
     * result is also Infer<Int> and in fact will be the same as the input types.
     */
}

impl Scope {
    /// Declare an identifier in the current scope with the given type
    pub fn declare_ident(&mut self, symbol_id: SymbolId, ty: &Ty) -> NameId {
        // Declare name
        let name_id = self.namespace.declare_name(symbol_id);

        // Add binding for type
        self.bindings.insert(name_id, ty.clone());

        // Return name and type
        name_id
    }

    /// Get the name and type from a symbol in the current scope
    fn get_ident(&self, symbol_id: SymbolId) -> Option<Binding> {
        let name_id = self.namespace.get_name(symbol_id)?;
        let ty = self.bindings.get(&name_id)?.clone();
        Some((name_id, ty))
    }
}

#[derive(Debug, Default)]
struct Namespace {
    current_name_id: NameId,
    names: HashMap<SymbolId, NameId>,
}

impl Namespace {
    fn get_name(&self, symbol: SymbolId) -> Option<NameId> {
        self.names.get(&symbol).cloned()
    }

    /// Declare a name in the space. Will redeclare if necessary
    fn declare_name(&mut self, symbol: SymbolId) -> NameId {
        let name_id = self.current_name_id.next();
        self.names.insert(symbol, name_id);
        name_id
    }
}
