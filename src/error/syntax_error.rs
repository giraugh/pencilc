use crate::span::Span;

#[allow(unused)]
#[derive(Debug)]
pub struct SyntaxError {
    pub span: Span,
    pub message: String,
}
