use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;
use std::str;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub begin: usize,
    pub end: usize
}

impl Span {
    pub fn new(b: usize, e: usize) -> Span {
        Span {
            begin: b,
            end: e
        }
    }

    pub fn new_with_len(b: usize, len: usize) -> Span {
        Self::new(b, b + len)
    }

    pub fn merge(a: Span, b: Span) -> Span {
        Span {
            begin: a.begin,
            end: b.end
        }
    }
}

pub struct SourceManager {
    pub source: String
}

impl SourceManager {
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<SourceManager> {
        let mut file = try!(File::open(path));
        let mut raw_input = String::new();
        try!(file.read_to_string(&mut raw_input));
        Ok(SourceManager {
            source: raw_input
        })
    }

    pub fn reader<'a>(&'a self) -> Reader<'a> {
        Reader {
            iter: self.source.char_indices(),
            commenting: false
        }
    }
}

pub struct Reader<'a> {
    iter: str::CharIndices<'a>,
    commenting: bool
}

impl<'a> Iterator for Reader<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((bytepos, c)) = self.iter.next() {
            match (c, self.commenting) {
                ('{', false) => { self.commenting = true; self.next() },
                ('}', true) => { self.commenting = false; self.next() },
                (c, false) => { Some((bytepos, c)) },
                (_, true) => { self.next() }
            }
        } else {
            None
        }
    }
}
