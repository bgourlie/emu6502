use super::*;

#[test]
fn test_parse_expr() {
    let parsed = parse("(a + b) - c \n");
    let (remaining, expression) = parse_expr(&parsed, None, 0).unwrap();
    println!("{:?}", remaining);
    println!("{:?}", expression);
}

fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
