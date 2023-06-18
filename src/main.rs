use std::{env, fs, path::Path, rc::Rc, sync::RwLock};

use codegen::Codegen;
use lex::TokenLexer;
use parse::Parser;
use session::Session;
use tyc::Tyc;

pub(crate) mod ast;
pub(crate) mod codegen;
pub(crate) mod error;
pub(crate) mod id;
pub(crate) mod lex;
pub(crate) mod parse;
pub(crate) mod session;
pub(crate) mod span;
pub(crate) mod tyc;

fn main() {
    // First argument is source input
    let source_path = env::args()
        .nth(1)
        .expect("Expected a path to a source file as the first argument");

    // Read the source file
    let source_path = Path::new(&source_path);
    compile(source_path);
}

fn compile(source_path: &Path) {
    let source_text = fs::read_to_string(source_path).expect("Can't read source file");

    // Initialise a compilation session
    let session = Rc::new(RwLock::new(Session::new(&source_text)));

    // Create lexer to get token stream
    let lexer = TokenLexer::new(session.clone());
    let tokens = lexer.tokenize().expect("lexing error");

    // Create parser
    let mut parser = Parser::new(session.clone(), tokens);
    let module_name = source_path
        .file_stem()
        .expect("To be able to determine module name")
        .to_str()
        .expect("To be able to determine module name");
    let root_module = parser.parse_module(module_name).expect("parsing error");

    // Typecheck
    let mut tc = Tyc::new();
    let root_module = tc
        .typecheck_module(root_module)
        .unwrap_or_else(|e| panic!("{}", e));

    // Codegen
    Codegen::compile(root_module).expect("codegen error");

    eprintln!("done");
}
