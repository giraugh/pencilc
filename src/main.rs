use std::{env, fs};

use lex::tokenize;

mod lex;

fn main() {
    // First argument is source input
    let source_path = env::args()
        .nth(1)
        .expect("Expected a path to a source file as the first argument");

    // Read the source file
    let source_text = fs::read_to_string(source_path).expect("Can't read source file");

    // Turn source into stream of tokens
    let tokens = tokenize(&source_text);
    {
        let tokens_vec = tokens.flatten().collect::<Vec<_>>();
        dbg!(tokens_vec);
    }
}
