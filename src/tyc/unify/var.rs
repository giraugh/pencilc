use super::super::ty;
use crate::{
    error::TypeError,
    id::Idx,
    make_id,
    tyc::ty::{PrimitiveTy, Ty},
};

make_id!(TyInferVar, "infv");

use ena::unify as ena;

impl ena::UnifyKey for TyInferVar {
    type Value = TyInferValue;

    fn index(&self) -> u32 {
        self.0 as u32
    }

    fn from_index(u: u32) -> Self {
        Self(u as usize)
    }

    fn tag() -> &'static str {
        "TyInferVar"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyInferValue {
    /// This type variable isn't bound to anything yet
    Unbound(InferValueKind),

    /// This type variable is bound to a concrete type
    Bound(ty::Ty),
}

/// What this type variable can be bound to
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferValueKind {
    /// Can be bound to anything
    General,

    /// Can be bound to an integral type
    Integral,
}

impl InferValueKind {
    pub fn can_unify_with(&self, ty: &Ty) -> bool {
        match (self, ty) {
            (&Self::General, _) => true,
            (&Self::Integral, Ty::Primitive(PrimitiveTy::SInt | PrimitiveTy::UInt)) => true,
            _ => false,
        }
    }
}

impl ena::UnifyValue for TyInferValue {
    type Error = TypeError;

    fn unify_values(a: &Self, b: &Self) -> Result<Self, Self::Error> {
        use InferValueKind::*;
        use TyInferValue::*;
        match (a, b) {
            // If one is integral, then both integral
            (&Unbound(Integral), &Unbound(_)) | (&Unbound(_), &Unbound(Integral)) => {
                Ok(Unbound(InferValueKind::Integral))
            }

            // If both are generically unbound then remain that way
            (&Unbound(_), &Unbound(_)) => Ok(Unbound(InferValueKind::General)),

            // If one is bound, unify
            (bound @ &Bound(ref bound_ty), &Unbound(ref infer_kind))
            | (&Unbound(ref infer_kind), bound @ &Bound(ref bound_ty)) => {
                // Is this a valid bind?
                if !infer_kind.can_unify_with(&bound_ty) {
                    Err(TypeError::CannotUnifyPrimitive(
                        infer_kind.clone(),
                        bound_ty.clone(),
                    ))
                } else {
                    Ok(bound.clone())
                }
            }

            // If both bound, error
            (&Bound(_), &Bound(_)) => {
                panic!("Can't unify two bound variables")
            }
        }
    }
}

impl TyInferVar {
    pub fn to_ty(self) -> ty::Ty {
        ty::Ty::Infer(self)
    }
}
