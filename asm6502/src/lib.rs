mod lexer;
mod parser;
mod resolver;
mod types;

pub use lexer::lex;
pub use parser::parse;
pub use resolver::Resolver;
pub use types::{Span, Token};

#[cfg(test)]
pub fn tlex(input: &'static str) -> Vec<crate::types::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::lex(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
