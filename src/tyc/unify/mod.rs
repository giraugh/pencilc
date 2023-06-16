mod var;

use ena::unify as ena;

use crate::error::TypeError;

use super::{
    ty::{self, Ty},
    Result,
};
pub use var::{TyInferValue, TyInferVar};

#[derive(Debug)]
struct Unifier {
    table: ena::UnificationTable<ena::InPlace<TyInferVar>>, // TODO: should this be a ref?
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            table: ena::UnificationTable::new(),
        }
    }

    /// Create a new type variable for inference
    pub fn new_variable(&mut self) -> TyInferVar {
        self.table.new_key(TyInferValue::Unbound)
    }

    pub fn unify_ty_ty(&mut self, a: &Ty, b: &Ty) -> Result<()> {
        // Normalize types (first norm a then b, then call recursive)
        if let Some(norm_a) = self.normalize(a) {
            return self.unify_ty_ty(&norm_a, b);
        } else if let Some(norm_b) = self.normalize(b) {
            return self.unify_ty_ty(a, &norm_b);
        }

        // Unify
        use ty::InferenceTyKind::*;
        match (a, b) {
            // If one is an integral and the other general, they should both become integral
            // inferences
            (&Ty::Infer(a, Integral), &Ty::Infer(b, General))
            | (&Ty::Infer(b, General), &Ty::Infer(a, Integral)) => {
                // I hope this is right, plan is to make the g type variable an integral type var
                // self.table
                //     .unify_var_value(b, TyInferValue::Bound(Ty::Infer(a, Integral)))
                //     .unwrap();

                Ok(self
                    .table
                    .unify_var_var(a, b)
                    .expect("Unifying two unbound values shouldnt fail"))
            }

            // In the case they are both general, we just do a standard unification
            (&Ty::Infer(a, _), &Ty::Infer(b, _)) => Ok(self
                .table
                .unify_var_var(a, b)
                .expect("Unifying two unbound values shouldnt fail")),

            // This handles unifying with primitive types
            (&Ty::Infer(inf, ref inf_kind), prim @ &Ty::Primitive(ref prim_kind))
            | (prim @ &Ty::Primitive(ref prim_kind), &Ty::Infer(inf, ref inf_kind)) => {
                // First we need to see if the inf kinds are compatible
                // e.g if this inference type is a string we can't bind it to an int
                if !inf_kind.can_unify_with(prim_kind) {
                    return Err(TypeError::CannotUnifyPrimitive(
                        inf_kind.clone(),
                        prim_kind.clone(),
                    ));
                }

                // Unify...
                Ok(self
                    .table
                    .unify_var_value(inf, TyInferValue::Bound(prim.clone()))
                    .expect("fingers crossed"))
            }

            _ => Ok(()),
        }
    }

    /// Normalize a type
    fn normalize(&mut self, ty: &Ty) -> Option<Ty> {
        match ty {
            Ty::Infer(id, _) => {
                let var = TyInferVar::from(*id);
                match self.table.probe_value(var) {
                    TyInferValue::Bound(ref ty) => Some(ty.clone()),
                    TyInferValue::Unbound => None,
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_unify_primitive_valid() {
        // Imagine we are typechecking something like this and want to infer x
        // let x = 10;

        // To start with, create a unifier
        let mut unifier = Unifier::new();

        // Create a type variable to represent the type of x
        let tv_x = unifier.new_variable().to_ty();

        // We also create a type variable to represent the literal 10
        // we know its an integral type but not which exact one
        let tv_e: Ty = unifier.new_variable().to_integral();

        unifier.unify_ty_ty(&tv_x, &tv_e).unwrap();

        // At this point we know that tv_x has to be an integral
        dbg!(unifier.normalize(&tv_x));

        // Now say we see some code like this
        // foo(x)
        // and we happen to know that foo has signature foo(int) -> int
        // now we can unify x with int

        // Pretend we tc'd the expression to get this type
        let int_ty = Ty::Primitive(ty::PrimitiveTy::Int);
        unifier.unify_ty_ty(&tv_x, &int_ty).unwrap();

        // Get normalized tv_x (it should be an int)
        dbg!(unifier.normalize(&tv_x));

        assert!(false);
    }
}
