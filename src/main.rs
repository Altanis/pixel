#![allow(dead_code)]
#![allow(clippy::upper_case_acronyms)]

use std::fs;

mod processor;
mod utils;

fn main() {
    let program = fs::read_to_string("test.px").expect("Invalid source file.");
    let mut lexer = processor::lexer::Lexer::new(program);
    let tokens = lexer.tokenise_program();

    if let Err(e) = tokens {
        eprintln!("{}", e);
    } else {
        println!("Compiling finished! {} tokens produced... reading all:", lexer.tokens.len());
        for token in lexer.tokens.iter() {
            println!("{:?}", token);
        }
    }
}
