use super::*;
use crate::parser::parse;

#[test]
fn test_literal_expr() {
    let tokens = parse("123 ; hello\n ;asdfasdfasdf\n");
    let (_, primary) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(Expression::Literal(123), primary);
}

#[test]
fn test_symbol_expr() {
    let tokens = parse("something = 1; hello\n ;asdfasdfasdf\n");
    let (_, primary_expr) = precedence0(TokenSlice(&tokens)).unwrap();
    assert_eq!(Expression::Symbol("something"), primary_expr);
}

#[test]
fn test_expression_1() {
    let tokens = parse("something = 1; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Symbol("something")),
            BinaryOperator::Equals,
            Box::new(Expression::Literal(1))
        ),
        expr
    );
}

#[test]
fn test_expression_2() {
    let tokens = parse("!something ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Unary(
            UnaryOperator::Negation,
            Box::new(Expression::Symbol("something"))
        ),
        expr
    );
}

#[test]
fn test_expression_3() {
    let tokens = parse("10 * 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Literal(10)),
            BinaryOperator::Multiply,
            Box::new(Expression::Literal(5))
        ),
        expr
    );
}

#[test]
fn test_expression_4() {
    let tokens = parse("10 + 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Literal(10)),
            BinaryOperator::Addition,
            Box::new(Expression::Literal(5))
        ),
        expr
    );
}

#[test]
fn test_expression_5() {
    let tokens = parse("10 > 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Literal(10)),
            BinaryOperator::GreaterThan,
            Box::new(Expression::Literal(5))
        ),
        expr
    );
}

#[test]
fn test_expression_6() {
    let tokens = parse("10 = 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Literal(10)),
            BinaryOperator::Equals,
            Box::new(Expression::Literal(5))
        ),
        expr
    );
}

#[test]
fn test_expression_7() {
    let tokens = parse("10 * !something ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Literal(10)),
            BinaryOperator::Multiply,
            Box::new(Expression::Unary(
                UnaryOperator::Negation,
                Box::new(Expression::Symbol("something"))
            ))
        ),
        expr
    );
}

#[test]
fn test_expression_8() {
    let tokens = parse("!(10 * !something) ; hello\n ;asdfasdfasdf\n");
    let expr = expression(TokenSlice(&tokens));
    println!("{:?}", expr);
    let (_, unary_expr) = expr.unwrap();
    assert_eq!(
        Expression::Unary(
            UnaryOperator::Negation,
            Box::new(Expression::Grouping(Box::new(Expression::Binary(
                Box::new(Expression::Literal(10)),
                BinaryOperator::Multiply,
                Box::new(Expression::Unary(
                    UnaryOperator::Negation,
                    Box::new(Expression::Symbol("something"))
                ))
            ),)))
        ),
        unary_expr
    );
}

#[test]
fn test_expression_9() {
    let tokens = parse("2 * 3 + 1 = 7 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(TokenSlice(&tokens)).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(2)),
                    BinaryOperator::Multiply,
                    Box::new(Expression::Literal(3))
                )),
                BinaryOperator::Addition,
                Box::new(Expression::Literal(1))
            )),
            BinaryOperator::Equals,
            Box::new(Expression::Literal(7))
        ),
        expr
    );
}
