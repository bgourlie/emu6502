use super::expr;

#[test]
fn test_expr() {
    let parsed = parse("a + b - c \n");
    let (remaining, expression) = expr(&parsed).unwrap();
    println!("{:?}", remaining);
    println!("{:?}", expression);
}

fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
