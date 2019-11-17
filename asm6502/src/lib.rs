#![allow(dead_code)]
mod lexer;
mod parser;

pub use lexer::{lex, Span, Token};
pub use parser::parse;
