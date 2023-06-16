use std::collections::HashMap;

use crate::id::{NameId, SymbolId};

use super::{
    tir,
    ty::{self, Ty},
};

#[derive(Debug, Default)]
pub struct TyEnv {
    bindings: HashMap<tir::Expr, Ty>,
    infer_env: InferEnv,
    namespace: Namespace,
}

impl TyEnv {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}

#[derive(Debug, Default)]
struct Namespace {
    names: HashMap<SymbolId, NameId>,
}

impl Namespace {
    fn get_name(symbol: SymbolId) -> NameId {
        todo!()
    }
}

#[derive(Debug, Default)]
struct InferEnv {}

#[derive(Debug)]
enum InferConstraint {
    Eq(tir::Expr, tir::Expr),
}

/*
 * new thoughts
 *
 *
 * we need both a "type environment" and an "inference context"
 *
 * the type environment is a map from expr: ty while the infctx houses inference variables
 * for reference, here are the types of inf variables in rust
 *  -> Type Variables
 *      -> General type vars (can be unified with any type)
 *      -> Integral type vars (can be unified with integral types)
 *      -> Float type vars (can be unified with float types)
 * A type variable is an actual "Ty" type. This type may be bound later as we do type checking.
 *
 *
 *
 *
 *
 */

/*
 * My notes about type environments
 *
 * type environment is a map from expr: ty
 *
 * if we had this code
 *
 * let x = foo();
 * if (x) {
 *   print("hello world");
 * }
 *
 * we can recursively "typecheck" the expr `foo()` and then assign that type to x.
 *
 */

/*
 * My notes about typechecking blocks: inference
 *
 *
 * As we look through the statements we create constraints
 * We also maintain a namespace that lets us uniquely identify names.
 * the namespace has a current name_id that inits to 0.
 *
 * When we see a declaration statement/expr that introduces a name
 * we add the name with the symbol_id to a new name in our namespace with the next
 * name_id as well. If the declaration is also a definition then we add a constraint like
 * name <=> expr
 *
 * If we see a name *in* an expr, we lookup the most recent name with that symbol and use that
 * instead.
 *
 * The namespace is on a scope which is a stack so when we go into inner blocks we end up with a
 * blank namespace in a new scope. When we lookup a name in the local scope, if not found it
 * recursively travels up through the parent scopes to find the name.
 *
 * Once we process the entire body we attempt to unify the constraints and determine the actual
 * types of each name.
 *
 * When typechecking an expr, if we see a literal, we set the type to be a "loose" version of that
 * literal. This isn't as applicable for strings but for numbers we have the loose type
 * LooseInt and LooseFloat that could be various concrete types.
 *
 * Some more constraints we can find
 *  => If we have a binary operation like `a + b` then a and b
 * must be the same type and so can be unified.
 *  => (For later) Conditions in `if` and `while` must be a `bool` so we can constrain cond <=> bool
 *
 *  Thoughts: do we constrain two types or an expr and a type?
 *
 * ----
 *
 * When we look through each statement we can create constraints from it.
 *
 * The strat may be to make the least assumptions about the type of the LHS and then RHS,
 * then add them together to a set of constraints for the local scope.
 *
 * for example in
 *
 * 1: let x = 10;
 * 2: foo(x);
 *
 * x could either be an `int` or a `uint` so after (1) we have this constraint
 * { x <=> someint }
 *
 * then, lets say that foo() has the signature (int) -> !
 * then we add this constraint to x
 * { x <=> int }
 *
 * now we can unify it with the previous set of constraints
 *
 * idk....
 *
 * not sure how this can play nicely with redeclarations though. What tells apart an early `x` from
 * a late `x`? Do we need to assign each declaration to `x` a unique number? That could work
 * maybe... Then when we parse each statement we can look at the current highest id of `x`?
 */

/*
 *  My notes about typechecking blocks: redeclaration
 *
 *  we want to allow declarations with the same name that shadow the previous variable
 *  for example
 *  ```
 *  let x = 10;
 *  let x = "hi";
 *  ```
 *
 *  There are extra considerations though. Take this example,
 *  ```
 *  1: let x = 10;
 *  2: print(x + 5);
 *  3: let x = "hi";
 *  4: print(x);
 *  ```
 *  The `x` at 2 needs to reference the `x` at 1 but the `x` at 4 needs to reference the
 *  `x` declared at 3.
 *
 *  This also raises questions for the type environment. What is `x` bound to? Or do we
 *  need some way of uniquely identifying each `x`? One solution might be to have a new
 *  scope created after every let declaration. That would create scopes like this
 *
 *  ```
 *  {
 *    1: let x = 10;
 *    2: print(x + y);
 *    {
 *       3: let x = "hi";
 *       4: print(x);
 *    }
 *  }
 *  ```
 *
 *  this makes it unambiguous which `x` is being referred to without needing to change the
 *  identifier of `x`. Does it create undue overhead though? Is this only necessary if `x`
 *  is redeclared? Obviously the scopes don't have to be while blocks and can just be
 *  scopes pushed onto the scope stack. In that case though, how do we know when to end the
 *  scope? Does it just continue until the end of the current block?
 *
 *  lets look at this example. We start with this actual source code
 *  ```
 *  1: let x = 10;
 *  2: let y = {
 *    3: let z = 20;
 *    4: let z = "hi";
 *  };
 *  ```
 *
 *  giving a new scope to all declarations we get
 *  ```
 *  {
 *    1: let x = 10;
 *    2: let y = {
 *      {
 *        3: let z = 20;
 *        {
 *          4: let z = "hi";
 *        }
 *      }
 *    }
 *  }
 *  ```
 *
 *  perhaps we can just push onto the scope stack whenever we see a declaration?
 *  then whenever we end a block we pop it?
 *
 *  although, maybe we can just replace the binding in the scope stack *when* we see it?
 *  lets walk it through
 *
 *  ```
 *  1: let x = 10;
 *  2: print(x + 5);
 *  3: let x = "hi";
 *  4: print(x);
 *  5: {
 *    6: let x = 20;
 *    7: print(x);
 *  }
 *  ```
 *
 *  Initially our scope stack is {{}}
 *  Then we visit the first statement. Now we have {{ x: int }}
 *  Then we use x as int in the second statement
 *  Then after we visit the third statement, we have {{ x: string }}
 *  Then we can use that on 4.
 *  For five we push a new scope to get {{ x: string }, {}}
 *  For 6 we can add x to the top scope to get {{ x: string }, { x: int }}
 *
 */
