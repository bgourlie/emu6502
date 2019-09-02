use super::*;

#[test]
fn test() {
    let tokens = parse("()(()()())\n ;asdfasdfasdf\n");
    let tokens2 = expression_tokens(TokenSlice(&tokens)).unwrap();
    println!("{:?}", tokens2);
}

#[test]
fn test2() {
    let tokens = parse("123 ; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let primary = primary(tokens);
    println!("{:?}", primary);
}

fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
