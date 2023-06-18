use std::{fs, path::Path, rc::Rc, sync::RwLock};

use cli::CliArgs;
use codegen::Codegen;
use lex::TokenLexer;
use parse::Parser;
use session::Session;
use tyc::Tyc;

pub(crate) mod ast;
mod cli;
pub(crate) mod codegen;
pub(crate) mod error;
pub(crate) mod id;
pub(crate) mod lex;
pub(crate) mod parse;
pub(crate) mod session;
pub(crate) mod span;
pub(crate) mod tyc;

fn main() {
    use clap::Parser;
    let cli_args = CliArgs::parse();

    // Compile each input
    // for now we dont link them
    // so just use the first input
    let source_path = cli_args
        .inputs
        .first()
        .expect("Expected at least one input file");

    // Determine module name from path
    let module_name = source_path
        .file_stem()
        .expect("To be able to determine module name")
        .to_str()
        .expect("To be able to determine module name");

    // Compile it
    compile(
        &source_path,
        &cli_args.get_output(module_name),
        &module_name,
        &cli_args.emit,
    );
}

fn compile(input_path: &Path, output_path: &Path, module_name: &str, emit: &cli::Output) {
    let source_text = fs::read_to_string(input_path).expect("Can't read source file");

    // Initialise a compilation session
    let session = Rc::new(RwLock::new(Session::new(&source_text)));

    // Create lexer to get token stream
    let lexer = TokenLexer::new(session.clone());
    let tokens = lexer.tokenize().expect("lexing error");

    // Create parser
    let mut parser = Parser::new(session.clone(), tokens);
    let root_module = parser.parse_module(module_name).expect("parsing error");

    // Typecheck
    let mut tc = Tyc::new();
    let root_module = tc
        .typecheck_module(root_module)
        .unwrap_or_else(|e| panic!("{}", e));

    // Codegen
    let session = inkwell::context::Context::create();
    let codegen = Codegen::codegen_module(&session, root_module).expect("codegen error");
    match *emit {
        cli::Output::LlvmIr => codegen.emit_llvm_ir(output_path),
        cli::Output::LlvmBc => codegen.emit_llvm_bc(output_path),
        cli::Output::Obj => { /* Do nothing here, need all modules compiled first */ }
    }
}
