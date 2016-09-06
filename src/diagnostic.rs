use std::process;
use std::fmt;

use source::Span;
use parser::ParseError;

#[derive(Debug, Clone)]
pub struct DiagnosticEngine<'a> {
    pub source: &'a str
}

impl<'a> DiagnosticEngine<'a> {
    pub fn report_lex_error(&self, description: String, bytepos: usize) -> ! {
        let span = Span::new_with_len(bytepos, 1);
        let error = RenderError::from_span(span, description, self.source);

        println!("{}", error);
        process::exit(-1);
    }

    pub fn report_parse_error(&self, error: ParseError) -> ! {
        let (description, span) = match error {
            ParseError::Unexpected(desc, span) => {
                (desc, span.unwrap_or(Span::new_with_len(self.source.len() - 1, 1)))
            }
        };

        let error = RenderError::from_span(span, description, self.source);

        println!("{}", error);
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
    pub lines: Vec<ErrorLine>
}

impl RenderError {
    pub fn from_span(span: Span, description: String, source: &str) -> RenderError {
        let arrow: String = source.char_indices().map(|(pos, c)| {
            if c == '\n' {
                c
            } else if span.begin <= pos && pos < span.end {
                '^'
            } else {
                ' '
            }
        }).collect();

        let lines: Vec<_> = source.lines().zip(arrow.lines())
        .enumerate()
        .filter_map(|(index, (src, arrow))| {
            if arrow.contains('^') {
                Some(ErrorLine{
                    line: index + 1,
                    src: String::from(src),
                    arrow: String::from(arrow)
                })
            } else {
                None
            }
        }).collect();

        RenderError {
            description: description,
            lines: lines
        }
    }
}

impl fmt::Display for RenderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Error: {}\n", self.description));
        for line in &self.lines {
            try!(line.fmt(f));
        }
        Ok(())
    }
}
