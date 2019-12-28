#![type_length_limit = "1575113"] // See https://github.com/rust-lang/rust/issues/54540 (due to abundance of closures in lexer)

mod lexer;
mod parser;
mod resolver;
mod types;

pub use lexer::Lexer;
pub use parser::parse;
pub use resolver::Resolver;
pub use types::{PositionedToken, Token};
