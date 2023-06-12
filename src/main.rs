use std::{env, fs};

use lex::tokenize;
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
    let mut session = Session::new(&source_text);

    // Turn source into stream of tokens
    let tokens = tokenize(&mut session);
    {
        let tokens_vec = tokens.flatten().collect::<Vec<_>>();
        dbg!(tokens_vec);
    }
}
