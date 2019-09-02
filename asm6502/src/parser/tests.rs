use super::*;

#[test]
fn test() {
    let tokens = parse("()(()()())\n ;asdfasdfasdf\n");
    let tokens2 = expression_tokens(TokenSlice(&tokens)).unwrap();
    println!("{:?}", tokens2);
}

#[test]
fn test_primary_literal_expr() {
    let tokens = parse("123 ; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let (_, primary) = primary(tokens).unwrap();
    assert_eq!(Expression::Literal(123), primary);
}

#[test]
fn test_primary_symbol_expr() {
    let tokens = parse("something = 1; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let (_, primary_expr) = primary(tokens).unwrap();
    assert_eq!(Expression::Symbol("something"), primary_expr);
}

#[test]
fn test_unary_expr() {
    let tokens = parse("something = 1; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let (_, unary_expr) = unary(tokens).unwrap();
    assert_eq!(Expression::Symbol("something"), unary_expr);
}

#[test]
fn test_unary_expr_with_operator() {
    let tokens = parse("!something ; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let (_, unary_expr) = unary(tokens).unwrap();
    assert_eq!(
        Expression::Unary(
            UnaryOperator::Negation,
            Rc::new(Box::new(Expression::Symbol("something")))
        ),
        unary_expr
    );
}

#[test]
fn test_multiplication_expression() {
    let tokens = parse("10 * 5 ; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let (_, unary_expr) = multiplication(tokens).unwrap();
    assert_eq!(
        Expression::Binary(
            Rc::new(Box::new(Expression::Literal(10))),
            BinaryOperator::Multiply,
            Rc::new(Box::new(Expression::Literal(5)))
        ),
        unary_expr
    );
}

#[test]
fn test_multiplication_expression_2() {
    let tokens = parse("10 * !something ; hello\n ;asdfasdfasdf\n");
    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
    let (_, unary_expr) = multiplication(tokens).unwrap();
    assert_eq!(
        Expression::Binary(
            Rc::new(Box::new(Expression::Literal(10))),
            BinaryOperator::Multiply,
            Rc::new(Box::new(Expression::Unary(
                UnaryOperator::Negation,
                Rc::new(Box::new(Expression::Symbol("something")))
            )))
        ),
        unary_expr
    );
}

//#[test]
//fn test_expression() {
//    let tokens = parse("!(10 * !something) ; hello\n ;asdfasdfasdf\n");
//    let (_, tokens) = expression_tokens(TokenSlice(&tokens)).unwrap();
//    let expr = expression(tokens);
//    println!("{:?}", expr);
//    let (_, unary_expr) = expr.unwrap();
//    assert_eq!(
//        Expression::Unary(UnaryOperator::Negation, Rc::new(Box::new(
//            Expression::Binary(
//                Rc::new(Box::new(Expression::Literal(10))),
//                BinaryOperator::Multiply,
//                Rc::new(Box::new(Expression::Unary(
//                    UnaryOperator::Negation,
//                    Rc::new(Box::new(Expression::Symbol("something")))
//                )))
//            ),
//        ))),
//        unary_expr
//    );
//}

fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
