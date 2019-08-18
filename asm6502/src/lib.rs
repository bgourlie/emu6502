#![allow(dead_code)]
mod lexer;
mod parser;

pub use lexer::{parse, Span, Token};
