use std::env;

mod reader;
mod token;
mod lexer;
mod ast;
mod parser;
mod vm;

use reader::SourceManager;
use lexer::Lexer;
use parser::Parser;
use ast::Visitor;

fn usage(prog_path: &str) {
    println!("Bitsy compiler");
    println!("Usage: {} <input-file>", prog_path);
}

fn main() {
    let mut args = env::args();
    let prog_path = args.next().unwrap_or(String::from("./bitsy"));
    let file_path = match args.next() {
        Some(path) => path,
        None => { usage(&prog_path); return }
    };

    let source_manager = SourceManager::new(file_path).unwrap();
    let source_reader = source_manager.reader();
    let lexer = Lexer::new(source_reader);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    println!("{:#?}", program);
    let mut builder = vm::Builder::new();
    builder.visit_program(&program);
    //println!("{:?}", builder.opcodes);
    vm::run(builder.opcodes);
}
