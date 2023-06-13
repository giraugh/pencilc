use std::{env, fs, rc::Rc, sync::RwLock};

use lex::TokenLexer;
use parse::Parser;
use session::Session;

pub(crate) mod error;
pub(crate) mod lex;
pub(crate) mod parse;
pub(crate) mod session;
pub(crate) mod span;

fn main() {
    // First argument is source input
    let source_path = env::args()
        .nth(1)
        .expect("Expected a path to a source file as the first argument");

    // Read the source file
    let source_text = fs::read_to_string(source_path).expect("Can't read source file");

    // Initialise a compilation session
    let session = Rc::new(RwLock::new(Session::new(&source_text)));

    // Create lexer to get token stream
    let lexer = TokenLexer::new(session.clone());
    let tokens = lexer.tokenize().expect("lexing error");

    // Create parser
    let mut parser = Parser::new(session, tokens);

    // For now, parse a single function
    let result = parser.parse_module().expect("parsing error");
    dbg!(result);
}
