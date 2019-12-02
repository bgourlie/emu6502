use super::*;
use crate::{lex, parse};

#[test]
fn test_resolve_expr() {
    let (_, tokens) = lex("val = 2 * (3 + 4)\n").unwrap();
    let (_, lines) = parse(&tokens).unwrap();

    let mut resolver = Resolver::new(lines.len());
    for line in lines.into_iter() {
        resolver.resolve_line(line).unwrap();
    }

    unimplemented!()
    //assert_eq!(Line::Equals("val", 14), resolver.lines[0])
}
