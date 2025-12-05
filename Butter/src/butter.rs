mod lexer;
mod parser;
mod transpiler;
mod compiletask;
mod cli;

use std::{env, fs};
use lexer::lex;
use parser::parse_tokens;
use transpiler::transpile;
use cli::parse_termargs;

fn main() {
    let envargs = env::args().collect();

    let source = fs::read_to_string("main.fp").expect("File read error");

    let tokens = lex(&source);
    let program = parse_tokens(tokens);
    println!("{:?}", program);
    transpile(program);
    parse_termargs(envargs);
}
