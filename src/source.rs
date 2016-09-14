use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;
use std::str;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
}

impl Span {
    pub fn new(b: usize, e: usize) -> Span {
        Span { begin: b, end: e }
    }

    pub fn new_with_len(b: usize, len: usize) -> Span {
        Self::new(b, b + len)
    }

    pub fn merge(a: Span, b: Span) -> Span {
        Span {
            begin: a.begin,
            end: b.end,
        }
    }
}

pub fn write_to_file<P: AsRef<Path>>(path: P, content: String) -> io::Result<()> {
    let mut file = try!(File::create(path));
    write!(file, "{}", content)
}

#[derive(Debug, Clone)]
pub struct Manager {
    pub source: String,
}

impl Manager {
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<Manager> {
        let mut file = try!(File::open(path));
        let mut raw_input = String::new();
        try!(file.read_to_string(&mut raw_input));
        Ok(Manager { source: raw_input })
    }

    pub fn reader(&self) -> Reader {
        Reader { iter: self.source.char_indices() }
    }
}

#[derive(Debug, Clone)]
pub struct Reader<'a> {
    iter: str::CharIndices<'a>,
}

impl<'a> Iterator for Reader<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.as_str().starts_with("//") {
            loop {
                match self.iter.next() {
                    Some((_, '\n')) => break,
                    None => break,
                    _ => continue,
                }
            }
            self.next()
        } else if self.iter.as_str().starts_with("/*") {
            self.iter.next();
            let target = |s: &str| s.len() != 0 && !s.starts_with("*/");
            while target(self.iter.as_str()) {
                self.iter.next();
            }
            self.iter.next();
            self.iter.next();
            self.next()
        } else {
            self.iter.next()
        }
    }
}
