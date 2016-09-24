extern crate itertools;
extern crate docopt;
extern crate rustc_serialize;

use docopt::Docopt;

mod double_peekable;
mod source;
mod token;
mod lexer;
// mod ast;
mod parse_tree;
mod parser;
mod diagnostic;
// mod ir;
// mod cgen;

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
    // Ir,
    // C,
    Pretty,
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

    // let emit_type = args.flag_emit.unwrap_or(EmitType::C);
    let emit_type = args.flag_emit.unwrap_or(EmitType::Pretty);

    let output_content = match emit_type {
        // EmitType::Ir => {
        //     let module = get_module(program, &diagnostic_engine, args.flag_O);
        //     module.to_string()
        // }
        // EmitType::C => {
        //     let module = get_module(program, &diagnostic_engine, args.flag_O);
        //     cgen::generate(module)
        // }
        EmitType::Pretty => parse_tree::pretty_printer::print(&program),
    };

    if let Some(output_path) = args.flag_o {
        source::write_to_file(output_path, output_content).unwrap();
    } else {
        println!("{}", output_content);
    }
}

// fn get_module(program: ast::Program,
//               diag_engine: &diagnostic::DiagnosticEngine,
//               opt: bool)
//               -> ir::Module {
//     let mut module = ir::gen::generate(program, diag_engine);
//     if opt {
//         ir::opt::optimize(&mut module);
//     }
//     module
// }
