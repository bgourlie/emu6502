use super::*;
use crate::{parse, tlex};

#[test]
fn test_resolve_expr() {
    let tokens = tlex("val = 2 * (3 + 4)\n");
    let (_, lines) = parse(&tokens).unwrap();

    let mut resolver = Resolver::new(lines.len());
    for line in lines.into_iter() {
        resolver.resolve_line(line).unwrap();
    }

    unimplemented!()
    //assert_eq!(Line::Equals("val", 14), resolver.lines[0])
}
