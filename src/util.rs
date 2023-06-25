use std::iter::Peekable;

pub trait MarkLastIterExt<T: Iterator>: Iterator<Item = T::Item> {
    fn mark_last(self) -> MarkLast<T>;
}

pub struct MarkLast<T: Iterator>(Peekable<T>);

impl<T: Iterator> MarkLastIterExt<T> for T {
    fn mark_last(self) -> MarkLast<T> {
        MarkLast(self.peekable())
    }
}

impl<T: Iterator> Iterator for MarkLast<T> {
    type Item = (bool, T::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.0.next();
        let has_next_item = self.0.peek().is_some();
        item.and_then(|item| Some((!has_next_item, item)))
    }
}
