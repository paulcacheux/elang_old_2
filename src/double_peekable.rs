use std::collections::VecDeque;
use std::iter::Fuse;

#[derive(Clone)]
pub struct DoublePeekable<I>
    where I: Iterator
{
    iter: Fuse<I>,
    buf: VecDeque<I::Item>,
}

impl<I: Iterator> DoublePeekable<I> {
    pub fn new(iter: I) -> DoublePeekable<I> {
        DoublePeekable {
            iter: iter.fuse(),
            buf: VecDeque::new(),
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        if self.buf.is_empty() {
            match self.iter.next() {
                Some(val) => {
                    self.buf.push_front(val);
                    self.buf.front()
                }
                None => None,
            }
        } else {
            self.buf.front()
        }
    }

    pub fn peek_double(&mut self) -> Option<(&I::Item, &I::Item)> {
        while self.buf.len() < 2 {
            match self.iter.next() {
                Some(val) => self.buf.push_back(val),
                None => break,
            }
        }
        let first = self.buf.get(0);
        let second = self.buf.get(1);
        match (first, second) {
            (Some(ref a), Some(ref b)) => Some((a, b)),
            _ => None,
        }
    }
}

impl<I> Iterator for DoublePeekable<I>
    where I: Iterator
{
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        if self.buf.is_empty() {
            self.iter.next()
        } else {
            self.buf.pop_front()
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let offset = self.buf.len();
        match self.iter.size_hint() {
            (a, Some(b)) => (a + offset, Some(b + offset)),
            (a, None) => (a + offset, None),
        }
    }
}
