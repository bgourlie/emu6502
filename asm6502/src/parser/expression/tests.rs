use super::expression;
use crate::parser::{
    tlex,
    types::{BinaryOperator, Expression, Symbol, UnaryOperator},
};

#[test]
fn test_literal_expr() {
    let tokens = tlex("123 ; hello\n ;asdfasdfasdf\n");
    let (_, primary) = expression(&tokens).unwrap();
    assert_eq!(Expression::Literal(123), primary);
}

#[test]
fn test_expression_1() {
    let tokens = tlex("something = 1; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Symbol(Symbol::Named("something"))),
            BinaryOperator::Equals,
            Box::new(Expression::Literal(1))
        ),
        expr
    );
}

#[test]
fn test_expression_2() {
    let tokens = tlex("!something ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
    assert_eq!(
        Expression::Unary(
            UnaryOperator::Negation,
            Box::new(Expression::Symbol(Symbol::Named("something")))
        ),
        expr
    );
}

#[test]
fn test_expression_3() {
    let tokens = tlex("10 * 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
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
    let tokens = tlex("10 + 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
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
    let tokens = tlex("10 > 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
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
    let tokens = tlex("10 = 5 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
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
    let tokens = tlex("10 * !something ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Literal(10)),
            BinaryOperator::Multiply,
            Box::new(Expression::Unary(
                UnaryOperator::Negation,
                Box::new(Expression::Symbol(Symbol::Named("something")))
            ))
        ),
        expr
    );
}

#[test]
fn test_expression_8() {
    let tokens = tlex("!(10 * !something) ; hello\n ;asdfasdfasdf\n");
    let (_, unary_expr) = expression(&tokens).unwrap();
    assert_eq!(
        Expression::Unary(
            UnaryOperator::Negation,
            Box::new(Expression::Grouping(Box::new(Expression::Binary(
                Box::new(Expression::Literal(10)),
                BinaryOperator::Multiply,
                Box::new(Expression::Unary(
                    UnaryOperator::Negation,
                    Box::new(Expression::Symbol(Symbol::Named("something")))
                ))
            ),)))
        ),
        unary_expr
    );
}

#[test]
fn test_expression_9() {
    let tokens = tlex("2 * 3 + 1 = 7 ; hello\n ;asdfasdfasdf\n");
    let (_, expr) = expression(&tokens).unwrap();
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

#[test]
fn test_expression_10() {
    let tokens = tlex("(data_segment & $ff) != 0\n");
    let (_, expr) = expression(&tokens).unwrap();
    assert_eq!(
        Expression::Binary(
            Box::new(Expression::Grouping(Box::new(Expression::Binary(
                Box::new(Expression::Symbol(Symbol::Named("data_segment"))),
                BinaryOperator::And,
                Box::new(Expression::Literal(255))
            )))),
            BinaryOperator::NotEquals,
            Box::new(Expression::Literal(0))
        ),
        expr
    );
}
