use super::*;
use crate::{parse, Lexer, Token};

#[test]
fn test_resolve_expr() {
    let tokens: Vec<Token> = Lexer::new("val = 2 * (3 + 4)\n")
        .map(|(token, _, _, _)| token)
        .collect();
    let (_, lines) = parse(&tokens).unwrap();

    let mut resolver = Resolver::new(lines.len());
    for line in lines.into_iter() {
        resolver.resolve_line(line).unwrap();
    }

    unimplemented!()
    //assert_eq!(Line::Equals("val", 14), resolver.lines[0])
}
