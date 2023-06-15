pub trait Idx: Sized + Copy + 'static + Eq {
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

macro_rules! make_id {
    ($name: ident) => {
        #[derive(Clone, Default, PartialEq, Eq, Copy)]
        pub struct $name(usize);

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "@{}", self.0)
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
    };
}

make_id!(BlockId);
make_id!(StatementId);
make_id!(ExprId);
