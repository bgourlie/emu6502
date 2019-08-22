use super::expr;
use crate::{parse, Span};

#[test]
fn test_expr() {
    let input = Span::new("a + b - c \n");
    let (_, parsed) = parse(input).unwrap();
    let expression = expr(&parsed);
    println!("{:?}", expression)
}
