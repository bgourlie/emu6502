mod lexer;
mod parser;
mod resolver;

pub use lexer::{lex, Span, Token};
pub use parser::parse;
pub use resolver::Resolver;

#[cfg(test)]
pub fn tlex(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::lex(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
