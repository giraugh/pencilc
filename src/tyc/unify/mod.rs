mod var;

use ena::unify as ena;

use crate::error::TypeError;

use super::{
    ty::{self, Ty},
    Result,
};
pub use var::{InferValueKind, TyInferValue, TyInferVar};

#[derive(Debug)]
pub struct Unifier {
    table: ena::UnificationTable<ena::InPlace<TyInferVar>>, // TODO: should this be a ref?
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            table: ena::UnificationTable::new(),
        }
    }

    /// Create a new type variable for inference
    pub fn new_variable(&mut self, kind: InferValueKind) -> TyInferVar {
        self.table.new_key(TyInferValue::Unbound(kind))
    }

    pub fn unify_ty_ty(&mut self, a: &Ty, b: &Ty) -> Result<()> {
        // Normalize types (first norm a then b, then call recursive)
        if let Some(norm_a) = self.normalize(a) {
            return self.unify_ty_ty(&norm_a, b);
        } else if let Some(norm_b) = self.normalize(b) {
            return self.unify_ty_ty(a, &norm_b);
        }

        // Unify
        match (a, b) {
            // In the case they are both general, we just do a standard unification
            (&Ty::Infer(a), &Ty::Infer(b)) => Ok(self
                .table
                .unify_var_var(a, b)
                .expect("Unifying two unbound values shouldnt fail")),

            // This handles unifying with non infer types
            (&Ty::Infer(inf), concrete) | (concrete, &Ty::Infer(inf))
                if !matches!(concrete, &Ty::Infer(_)) =>
            {
                Ok(self
                    .table
                    .unify_var_value(inf, TyInferValue::Bound(concrete.clone()))?)
            }

            // If they aren't infer then they must be the same after normalization
            (a, b) if a == b => Ok(()),

            // Failed to normalize
            _ => Err(TypeError::CannotUnify(a.clone(), b.clone())),
        }
    }

    /// Normalize a type
    /// This applies all of the current substitutions to the type if its a type variable
    pub fn normalize(&mut self, ty: &Ty) -> Option<Ty> {
        match ty {
            Ty::Infer(id) => {
                let var = TyInferVar::from(*id);
                match self.table.probe_value(var) {
                    TyInferValue::Bound(ref ty) => Some(ty.clone()),
                    TyInferValue::Unbound(_) => None,
                }
            }
            _ => None,
        }
    }

    /// Normalize a type, attempt to recover non-general unbound types and error if not possible
    pub fn normalize_final(&mut self, ty: &Ty) -> Result<Ty> {
        match ty {
            Ty::Infer(id) => {
                let var = TyInferVar::from(*id);
                match self.table.probe_value(var) {
                    // We copy out the bound types
                    TyInferValue::Bound(ref ty) => Ok(ty.clone()),

                    // If unbound but integral, try falling back to an integer
                    TyInferValue::Unbound(InferValueKind::Integral) => {
                        // If we can unify it with an int we will do that
                        let prim_int = Ty::Primitive(ty::PrimitiveTy::SInt);
                        self.unify_ty_ty(ty, &prim_int).and_then(|_| Ok(prim_int))
                    }

                    // If unbound and general we cant do much
                    TyInferValue::Unbound(InferValueKind::General) => Err(TypeError::AmbiguousType),
                }
            }
            _ => Ok(ty.clone()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_normalize_final_valid() {
        /* Imagine this scenario
         *
         * {
         *   let x = 10;
         *   x
         * }
         *
         * what is the type of the block?
         */

        let mut unifier = Unifier::new();

        // We create a type variable for x and mark it as an integral because of the literal type
        let tv_x = unifier.new_variable(InferValueKind::Integral).to_ty();

        // We have nothing left to do now so we call normalize_final on all of our remaining type
        // values
        let tv_x = unifier.normalize_final(&tv_x).unwrap();

        // We should get back a concrete int type
        assert_eq!(tv_x, ty::Ty::Primitive(ty::PrimitiveTy::SInt));
    }

    #[test]
    fn test_unify_idents_valid() {
        /* Lets imagine this scenario
         *
         * let x;
         * let y: int = 10;
         * x = y;
         */

        let mut unifier = Unifier::new();

        // When we get to x we won't know its type
        // So we will create a type variable for it
        let tv_x = unifier.new_variable(InferValueKind::General).to_ty();

        // When we get to y we have a binding so we know its type
        // we dont have to create a type variable here because all the types are known
        let ty_y = Ty::Primitive(ty::PrimitiveTy::SInt);

        // Then when we get to x = y
        // We can find the type of y in our type environment and then we can unify x with the type
        // of y
        unifier.unify_ty_ty(&tv_x, &ty_y).unwrap();

        // Now if we normalize x we should get the concrete type
        let ty_x = unifier.normalize(&tv_x).unwrap();
        assert_eq!(ty_x, ty_y);
    }

    #[test]
    fn test_unify_primitive_valid() {
        // Imagine we are typechecking something like this and want to infer x
        // let x = 10;

        // To start with, create a unifier
        let mut unifier = Unifier::new();

        // Create a type variable to represent the type of x
        let tv_x = unifier.new_variable(InferValueKind::General);
        let tv_x_ty = tv_x.to_ty();

        // At this point we know that tv_x has to be an integral
        dbg!(unifier.table.probe_value(tv_x));

        // We also create a type variable to represent the literal 10
        // we know its an integral type but not which exact one
        let tv_e: Ty = unifier.new_variable(InferValueKind::Integral).to_ty();

        unifier.unify_ty_ty(&tv_x_ty, &tv_e).unwrap();

        // At this point we know that tv_x has to be an integral
        dbg!(unifier.table.probe_value(tv_x));

        // Now say we see some code like this
        // foo(x)
        // and we happen to know that foo has signature foo(int) -> int
        // now we can unify x with int

        // Pretend we tc'd the expression to get this type
        let int_ty = Ty::Primitive(ty::PrimitiveTy::SInt);
        unifier.unify_ty_ty(&tv_x_ty, &int_ty).unwrap();

        // Get normalized tv_x (it should be an int)
        dbg!(unifier.normalize(&tv_x_ty));
    }

    #[test]
    fn test_unify_primitive_invalid() {
        // Same as above but we try to unify with a non-integral towards the end
        let mut unifier = Unifier::new();
        let tv_x = unifier.new_variable(InferValueKind::General);
        let tv_x_ty = tv_x.to_ty();
        let tv_e: Ty = unifier.new_variable(InferValueKind::Integral).to_ty();
        unifier.unify_ty_ty(&tv_x_ty, &tv_e).unwrap();

        // What if we now try and unify it with a string? (should error)
        let int_ty = Ty::Primitive(ty::PrimitiveTy::Str);
        let res = unifier.unify_ty_ty(&tv_x_ty, &int_ty);
        assert!(res.is_err());
    }
}
