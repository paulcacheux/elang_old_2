use itertools::Itertools;

use std::process;
use std::fmt;
use std::io::Write;

use source::Span;
use error::*;

macro_rules! println_stderr(
    ($($arg:tt)*) => { {
        writeln!(&mut ::std::io::stderr(), $($arg)*).expect("failed printing to stderr");
    } }
);

#[derive(Debug, Clone)]
pub struct DiagnosticEngine<'a> {
    pub source: &'a str,
    pub warning_activated: bool,
}

impl<'a> DiagnosticEngine<'a> {
    pub fn new(source: &'a str, warning_activated: bool) -> DiagnosticEngine {
        DiagnosticEngine {
            source: source,
            warning_activated: warning_activated,
        }
    }

    pub fn report_error(&self, error: CodeError) -> ! {
        let (span, description) = match error {
            CodeError::LexError(error) => {
                (Span::new_one(error.1), error.0.to_string())
            },
            CodeError::ParseError(error) => {
                let span = {
                    let end_span = Span::new_with_len(self.source.len() - 1, 1);
                    error.span.unwrap_or(end_span)
                };
                let description =
                    format!("Expected one of [{}]", error.expected.into_iter().join(", "));
                (span, description)
            },
            CodeError::SemaError(error) => {
                (error.1, error.0.to_string())
            },
        };
        let error = RenderError::from_span(span, description, self.source);
        println_stderr!("{}", error);
        process::exit(-1);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ErrorLine {
    pub line: usize,
    pub src: String,
    pub arrow: String,
}

impl fmt::Display for ErrorLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line_str = self.line.to_string();
        let padding_arrow: String = line_str.chars().map(|_| ' ').collect();

        try!(write!(f, "{}: {}\n", line_str, self.src));
        write!(f, "{}  {}\n", padding_arrow, self.arrow)
    }
}

#[derive(Debug, Clone)]
struct RenderError {
    pub description: String,
    pub lines: Vec<ErrorLine>,
}

impl RenderError {
    pub fn from_span(span: Span, description: String, source: &str) -> RenderError {
        let arrow: String = source.char_indices()
            .map(|(pos, c)| {
                if c == '\n' {
                    c
                } else if span.begin <= pos && pos < span.end {
                    '^'
                } else {
                    ' '
                }
            })
            .collect();

        let lines: Vec<_> = source.lines()
            .zip(arrow.lines())
            .enumerate()
            .filter_map(|(index, (src, arrow))| {
                if arrow.contains('^') {
                    Some(ErrorLine {
                        line: index + 1,
                        src: String::from(src),
                        arrow: String::from(arrow),
                    })
                } else {
                    None
                }
            })
            .collect();

        RenderError {
            description: description,
            lines: lines,
        }
    }
}

impl fmt::Display for RenderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let light_red = "\x1b[91m";
        let clear = "\x1b[0m";
        let error_text = format!("{}Error{}", light_red, clear);

        try!(write!(f, "{}: {}\n", error_text, self.description));
        for line in &self.lines {
            try!(line.fmt(f));
        }
        Ok(())
    }
}
