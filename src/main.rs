extern crate itertools;
extern crate docopt;
extern crate rustc_serialize;

use docopt::Docopt;

mod double_peekable;
mod source;
mod token;
mod lexer;
mod ast;
mod parser;
mod diagnostic;
mod error;

use source::Manager;
use lexer::Lexer;
use parser::Parser;
use ast::sema::Sema;

const USAGE: &'static str = "
Usage: elang [options] INPUT
       elang --help

Options:
    -h, --help         Show this message.
    --emit TYPE        Configure the output type.
                       Valid values: ir, c.
    -O                 Enable the optimizations.
    -o OUTPUT          Configure the output path.
    -W                 Enable warnings.
";

#[derive(RustcDecodable, Debug, Clone)]
#[allow(non_snake_case)]
struct Args {
    arg_INPUT: String,
    flag_emit: Option<EmitType>,
    flag_O: bool,
    flag_o: Option<String>,
    flag_W: bool,
}

#[derive(RustcDecodable, Debug, Clone, Copy, PartialEq, Eq)]
enum EmitType {
    C,
    AST,
}

fn main() {
    let args: Args = Docopt::new(USAGE).and_then(|d| d.decode()).unwrap_or_else(|e| e.exit());

    let source_manager = Manager::new(args.arg_INPUT).unwrap();
    let source_reader = source_manager.reader();
    let diagnostic_engine = diagnostic::DiagnosticEngine::new(&source_manager.source, args.flag_W);

    let lexer = Lexer::new(source_reader);
    let sema = Sema::new();
    let mut parser = Parser::new(lexer, sema);

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(code_error) => diagnostic_engine.report_error(code_error),
    };

    let emit_type = args.flag_emit.unwrap_or(EmitType::C);

    let output_content = match emit_type {
        EmitType::C => ast::cgen::generate_program(program),
        EmitType::AST => ast::pretty_printer::print(&program),
    };

    if let Some(output_path) = args.flag_o {
        source::write_to_file(output_path, output_content).unwrap();
    } else {
        println!("{}", output_content);
    }
}
