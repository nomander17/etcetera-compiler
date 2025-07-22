use std::{env, fs, path::Path};

use inkwell::context::Context;

use crate::{compiler::Compiler, lexer::Lexer, parser::Parser};

mod ast;
mod compiler;
mod lexer;
mod parser;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.etc> [-r|--run]", args[0]);
        return;
    }
    let file_path = Path::new(&args[1]);
    let _should_run = args.len() > 2 && (args[2] == "-r" || args[2] == "--run");

    let input = fs::read_to_string(file_path).expect("Error reading file");

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let context = Context::create();
    let mut compiler = Compiler::new(&context);
    let llvm_ir = compiler.compile(program).expect("Compilation failed");

    // Write IR to a file
    let _executable_name = file_path.file_stem().unwrap().to_str().unwrap();
    fs::write("output.ll", llvm_ir).expect("Error writing LLVM IR to file.");
}
