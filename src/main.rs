extern crate itertools;
extern crate docopt;
extern crate rustc_serialize;

use docopt::Docopt;

mod peek_and_push;
mod source;
mod token;
mod lexer;
mod ast;
mod parser;
mod diagnostic;
mod ir;
mod ir_gen;
mod ir_opt;
mod cgen;

use source::Manager;
use lexer::Lexer;
use parser::Parser;

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
    Ir,
    C,
}

fn main() {
    let args: Args = Docopt::new(USAGE).and_then(|d| d.decode()).unwrap_or_else(|e| e.exit());

    let source_manager = Manager::new(args.arg_INPUT).unwrap();
    let source_reader = source_manager.reader();
    let diagnostic_engine = diagnostic::DiagnosticEngine::new(&source_manager.source, args.flag_W);

    let lexer = Lexer::new(source_reader, &diagnostic_engine);
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(parser_error) => diagnostic_engine.report_parse_error(parser_error),
    };

    let mut module = ir_gen::generate(program, &diagnostic_engine);
    if args.flag_O {
        ir_opt::optimize(&mut module);
    }

    let emit_type = args.flag_emit.unwrap_or(EmitType::C);

    let output_content = match emit_type {
        EmitType::Ir => module.to_string(),
        EmitType::C => cgen::generate(module),
    };

    if let Some(output_path) = args.flag_o {
        source::write_to_file(output_path, output_content).unwrap();
    } else {
        println!("{}", output_content);
    }

}
