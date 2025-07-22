use std::{env, fs, io::Write, path::Path, process::Command};

use inkwell::context::Context;
use tempfile::NamedTempFile;

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
    let should_run = args.len() > 2 && (args[2] == "-r" || args[2] == "--run");

    let input = fs::read_to_string(file_path).expect("Error reading file");

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let context = Context::create();
    let mut compiler = Compiler::new(&context);

    match compiler.compile(program) {
        Ok(llvm_ir) => {
            let mut ll_file = NamedTempFile::new().expect("Failed to create temp file.");
            ll_file
                .write_all(llvm_ir.as_bytes())
                .expect("Failed to write to temp LLVM IR file.");

            let obj_file = NamedTempFile::new().expect("Failed to create temp file.");
            let obj_path = obj_file.path();

            let llc_status = Command::new("llc")
                .arg("-filetype=obj")
                .arg("-relocation-model=pic")
                .arg(ll_file.path())
                .arg("-o")
                .arg(obj_path)
                .status()
                .expect("Failed to execute llc");

            if !llc_status.success() {
                eprintln!("llc failed!");
                return;
            }

            let executable_name = file_path.file_stem().unwrap().to_str().unwrap();

            let clang_status = Command::new("clang")
                .arg(obj_path)
                .arg("-o")
                .arg(executable_name)
                .status()
                .expect("Failed to execute clang");

            if !clang_status.success() {
                eprintln!("clang failed!");
                return;
            }

            if should_run {
                let run_status = Command::new(format!("./{}", executable_name))
                    .status()
                    .expect("Failed to run exectuable!");

                if !run_status.success() {
                    eprintln!("Executable failed!");
                    return;
                }
            }
        }
        Err(e) => eprintln!("Error: {}", e),
    }
}
