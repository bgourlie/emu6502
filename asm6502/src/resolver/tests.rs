use super::*;
use crate::{parse, tlex};

#[test]
fn test_resolve_expr() {
    let tokens = tlex("val = 2 * (3 + 4)\n");
    let (_, lines) = parse(&tokens).unwrap();

    let resolver = Resolver::new(0);
    if let Line::Equals(name, expr) = &lines[0] {
        let result = resolver.resolve_expr(Rc::clone(expr)).unwrap();
        assert_eq!(14, result);
    } else {
        panic!("Expected Line::Equals")
    }
}
