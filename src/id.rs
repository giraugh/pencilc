use std::hash::Hash;

pub trait Idx: Sized + Copy + 'static + Eq + Default + Hash {
    fn new(idx: usize) -> Self;
    fn index(self) -> usize;

    fn next(&mut self) -> Self {
        let id = self.clone();
        self.increment_by(1);
        id
    }

    fn increment_by(&mut self, amt: usize) {
        *self = self.plus(amt);
    }

    fn plus(&self, amt: usize) -> Self {
        Self::new(self.index() + amt)
    }
}

#[macro_export]
macro_rules! make_id {
    ($name: ident, $tag: expr) => {
        #[derive(Clone, Default, PartialEq, Eq, Copy, Hash)]
        pub struct $name(usize);

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", $tag, self.0)
            }
        }

        impl Idx for $name {
            fn new(idx: usize) -> Self {
                Self(idx)
            }

            fn index(self) -> usize {
                self.0
            }
        }

        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                $name(value)
            }
        }
    };
}

make_id!(BlockId, '@');
make_id!(StatementId, '@');
make_id!(ExprId, '@');
make_id!(SymbolId, '$');
make_id!(StringId, '#');
make_id!(NameId, 'n');
