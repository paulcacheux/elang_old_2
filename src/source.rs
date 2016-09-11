use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;
use std::str;

use diagnostic::DiagnosticEngine;

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
    pub warning_activated: bool,
}

impl Manager {
    pub fn new<P: AsRef<Path>>(path: P, warning_activated: bool) -> io::Result<Manager> {
        let mut file = try!(File::open(path));
        let mut raw_input = String::new();
        try!(file.read_to_string(&mut raw_input));
        Ok(Manager {
            source: raw_input,
            warning_activated: warning_activated,
        })
    }

    pub fn reader(&self) -> Reader {
        Reader {
            iter: self.source.char_indices(),
            commenting: false,
        }
    }

    pub fn diagnostic_engine(&self) -> DiagnosticEngine {
        DiagnosticEngine {
            source: &self.source,
            warning_activated: self.warning_activated,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reader<'a> {
    iter: str::CharIndices<'a>,
    commenting: bool,
}

impl<'a> Iterator for Reader<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((bytepos, c)) = self.iter.next() {
            match (c, self.commenting) {
                ('{', false) => {
                    self.commenting = true;
                    self.next()
                }
                ('}', true) => {
                    self.commenting = false;
                    self.next()
                }
                (c, false) => Some((bytepos, c)),
                (_, true) => self.next(),
            }
        } else {
            None
        }
    }
}
