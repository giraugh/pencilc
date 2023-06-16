use super::super::ty;
use crate::{error::TypeError, id::Idx, make_id};

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
    Unbound,

    /// This type variable is bound to a concrete type
    Bound(ty::Ty),
}

impl ena::UnifyValue for TyInferValue {
    type Error = TypeError;

    fn unify_values(a: &Self, b: &Self) -> Result<Self, Self::Error> {
        use TyInferValue::*;
        match (a, b) {
            (&Unbound, &Unbound) => Ok(Unbound),
            (bound @ &Bound(_), &Unbound) | (&Unbound, bound @ &Bound(_)) => Ok(bound.clone()),
            (&Bound(_), &Bound(_)) => {
                panic!("Can't unify two bound variables")
            }
        }
    }
}

impl TyInferVar {
    pub fn to_ty(self) -> ty::Ty {
        ty::Ty::Infer(self, ty::InferenceTyKind::General)
    }

    pub fn to_integral(self) -> ty::Ty {
        ty::Ty::Infer(self, ty::InferenceTyKind::Integral)
    }
}
