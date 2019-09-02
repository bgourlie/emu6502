#[cfg(test)]
mod tests;
mod types;

use crate::Token;
use nom::{
    bytes::complete::{take, take_while1},
    combinator::map_res,
    IResult,
};
use std::rc::Rc;
use types::TokenSlice;

type BoxedExpression<'a> = Rc<Box<Expression<'a>>>;

#[derive(Debug)]
enum Operator {
    Multiply,
    Addition,
    Subtraction,
    Equals,
    NotEquals,
    GreaterThanOrEquals,
    LessThanOrEquals,
    Complement,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

#[derive(Debug)]
enum Expression<'a> {
    Literal(i32),
    Symbol(&'a str),
    Binary(BoxedExpression<'a>, Operator, BoxedExpression<'a>),
    Grouping(BoxedExpression<'a>),
}

fn primary(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    map_res(take(1 as usize), |t: TokenSlice| match t[0] {
        Token::Identifier(name) => Ok(Expression::Symbol(name)),
        Token::CharacterLiteral(chr) => Ok(Expression::Literal(chr as i32)),
        Token::DecLiteral(val)
        | Token::HexLiteral(val)
        | Token::BinLiteral(val)
        | Token::OctLiteral(val) => Ok(Expression::Literal(val)),
        _ => Err(()),
    })(input)
}

fn expression_tokens(input: TokenSlice) -> IResult<TokenSlice, TokenSlice> {
    take_while1(|t| {
        if let Token::SubExprStart
        | Token::SubExprEnd
        | Token::XorOperator
        | Token::StarOperator
        | Token::EqualsOperator
        | Token::BinLiteral(_)
        | Token::OctLiteral(_)
        | Token::CharacterLiteral(_)
        | Token::DecLiteral(_)
        | Token::ComplementOperator
        | Token::GreaterThanOperator
        | Token::GreaterThanOrEqualToOperator
        | Token::LessThanOperator
        | Token::LessThanOrEqualToOperator
        | Token::MinusOperator
        | Token::PlusOperator
        | Token::LeftShiftOperator
        | Token::RightShiftOperator
        | Token::NotEqualsOperator
        | Token::OrOperator
        | Token::AndOperator = t
        {
            true
        } else {
            false
        }
    })(input)
}
