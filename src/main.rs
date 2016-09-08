extern crate itertools;

use std::env;

mod source;
mod token;
mod lexer;
mod ast;
mod parser;
mod diagnostic;
mod ir;
mod ir_opt;
mod cgen;

use source::Manager;
use lexer::Lexer;
use parser::Parser;

fn usage(prog_path: &str) {
    println!("Bitsy compiler");
    println!("Usage: {} <input-file>", prog_path);
}

fn main() {
    let mut args = env::args();
    let prog_path = args.next().unwrap_or(String::from("./bitsy"));
    let file_path = match args.next() {
        Some(path) => path,
        None => {
            usage(&prog_path);
            return;
        }
    };
    let output_path = args.next().unwrap_or(String::from("a.c"));

    let source_manager = Manager::new(file_path).unwrap();
    let source_reader = source_manager.reader();
    let diagnostic_engine = source_manager.diagnostic_engine();

    let lexer = Lexer::new(source_reader, &diagnostic_engine);
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(parser_error) => diagnostic_engine.report_parse_error(parser_error),
    };

    let blocks = ir::generate(&program);
    // print_blocks(&blocks);
    let blocks = ir_opt::optimize(blocks);
    // print_blocks(&blocks);

    let c_source = cgen::generate(blocks);
    source::write_to_file(output_path, c_source).unwrap();
}

fn print_blocks(blocks: &Vec<ir::BasicBlock>) {
    println!("------------------------------------");
    for block in blocks {
        println!("{}", block);
    }
}
