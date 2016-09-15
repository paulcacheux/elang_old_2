use std::collections::VecDeque;
use std::iter::Fuse;

#[derive(Clone)]
pub struct NPeekable<I>
    where I: Iterator
{
    iter: Fuse<I>,
    buf: VecDeque<I::Item>,
}

impl<I: Iterator> NPeekable<I> {
    pub fn new(iter: I) -> NPeekable<I> {
        NPeekable {
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

    pub fn peek_n(&mut self, n: usize) -> Option<&I::Item> {
        loop {
            if self.buf.len() <= n {
                match self.iter.next() {
                    Some(val) => self.buf.push_back(val),
                    None => break,
                }
            } else {
                break;
            }
        }
        self.buf.get(n)
    }
}

impl<I> Iterator for NPeekable<I>
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
