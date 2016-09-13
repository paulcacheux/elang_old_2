use std::iter::Fuse;

#[derive(Clone)]
pub struct PeekAndPush<I>
    where I: Iterator
{
    iter: Fuse<I>,
    buf: Vec<I::Item>,
}

impl<I: Iterator> PeekAndPush<I> {
    pub fn new(iter: I) -> PeekAndPush<I> {
        PeekAndPush {
            iter: iter.fuse(),
            buf: Vec::new(),
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        if self.buf.is_empty() {
            if let Some(val) = self.iter.next() {
                self.buf.push(val);
                self.buf.last()
            } else {
                None
            }
        } else {
            self.buf.last()
        }
    }

    pub fn push(&mut self, val: I::Item) {
        self.buf.push(val);
    }
}

impl<I> Iterator for PeekAndPush<I>
    where I: Iterator
{
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        if self.buf.is_empty() {
            self.iter.next()
        } else {
            self.buf.pop()
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
