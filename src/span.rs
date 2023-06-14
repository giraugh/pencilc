use std::fmt::{Debug, Display};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, derive_more::Add)]
pub struct CharPos(pub usize);

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Span {
    pub start: CharPos,
    pub end: CharPos,
}

impl Span {
    pub fn new(start: CharPos, end: CharPos) -> Self {
        Self { start, end }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start.0, self.end.0)?;
        Ok(())
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start.0, self.end.0)?;
        Ok(())
    }
}
