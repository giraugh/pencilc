use std::str::Chars;

pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
        }
    }

    pub fn remaining_chars(&self) -> usize {
        self.chars.as_str().len()
    }

    /// Consume and return the next character
    pub fn next_char(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Get the next character without peeking
    /// panics if no such character
    pub fn peek_first(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    /// Are there characters left to consume?
    pub fn is_exhausted(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Consume characters until the next character to be consumed doesn't meet a predicate
    /// or we are fully exhausted
    pub fn eat_while(&mut self, mut pred_fn: impl FnMut(char) -> bool) {
        while pred_fn(self.peek_first()) && !self.is_exhausted() {
            self.next_char();
        }
    }

    /// Consume numeric digits
    pub fn eat_numeric_digits(&mut self) {
        self.eat_while(|c| matches!(c, '0'..='9' | '_'));
    }

    pub fn is_ident_start(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '$')
    }

    pub fn is_ident_continue(c: char) -> bool {
        Self::is_ident_start(c) || matches!(c, '0'..='9')
    }
}
