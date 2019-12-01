use super::expression;
use crate::{
    parser::types::{BinaryOperator, Expression, Symbol, UnaryOperator},
    tlex,
};
use std::rc::Rc;

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
            Rc::new(Expression::Symbol(Symbol::Named("something"))),
            BinaryOperator::Equals,
            Rc::new(Expression::Literal(1))
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
            UnaryOperator::LogicalNot,
            Rc::new(Expression::Symbol(Symbol::Named("something")))
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
            Rc::new(Expression::Literal(10)),
            BinaryOperator::Multiply,
            Rc::new(Expression::Literal(5))
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
            Rc::new(Expression::Literal(10)),
            BinaryOperator::Addition,
            Rc::new(Expression::Literal(5))
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
            Rc::new(Expression::Literal(10)),
            BinaryOperator::GreaterThan,
            Rc::new(Expression::Literal(5))
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
            Rc::new(Expression::Literal(10)),
            BinaryOperator::Equals,
            Rc::new(Expression::Literal(5))
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
            Rc::new(Expression::Literal(10)),
            BinaryOperator::Multiply,
            Rc::new(Expression::Unary(
                UnaryOperator::LogicalNot,
                Rc::new(Expression::Symbol(Symbol::Named("something")))
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
            UnaryOperator::LogicalNot,
            Rc::new(Expression::Grouping(Rc::new(Expression::Binary(
                Rc::new(Expression::Literal(10)),
                BinaryOperator::Multiply,
                Rc::new(Expression::Unary(
                    UnaryOperator::LogicalNot,
                    Rc::new(Expression::Symbol(Symbol::Named("something")))
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
            Rc::new(Expression::Binary(
                Rc::new(Expression::Binary(
                    Rc::new(Expression::Literal(2)),
                    BinaryOperator::Multiply,
                    Rc::new(Expression::Literal(3))
                )),
                BinaryOperator::Addition,
                Rc::new(Expression::Literal(1))
            )),
            BinaryOperator::Equals,
            Rc::new(Expression::Literal(7))
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
            Rc::new(Expression::Grouping(Rc::new(Expression::Binary(
                Rc::new(Expression::Symbol(Symbol::Named("data_segment"))),
                BinaryOperator::And,
                Rc::new(Expression::Literal(255))
            )))),
            BinaryOperator::NotEquals,
            Rc::new(Expression::Literal(0))
        ),
        expr
    );
}
