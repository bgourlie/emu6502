use super::*;
use crate::{parse, Lexer, Token};

#[test]
fn test_resolve_expr() {
    let tokens: Vec<Token> = Lexer::new("val = 2 * (3 + 4)\n").map(|i| i.token).collect();
    let (_, lines) = parse(&tokens).unwrap();

    let mut resolver = Resolver::default();
    for line in lines.iter() {
        resolver.resolve_line(line).unwrap();
    }

    assert_eq!(14, resolver.variables["val"])
}
