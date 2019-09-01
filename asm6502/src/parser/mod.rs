#[cfg(test)]
mod tests;
mod types;

use crate::Token;
use nom::{
    bytes::complete::{tag, take_while1},
    character::complete::one_of,
    combinator::opt,
    sequence::{pair, terminated},
    IResult, InputLength,
};
use std::rc::Rc;
use types::{GenericToken, TokenSlice};

type BoxedExpression<'a> = Rc<Box<Expression<'a>>>;

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

enum Expression<'a> {
    Literal(i32),
    Symbol(&'a str),
    Binary(BoxedExpression<'a>, Operator, BoxedExpression<'a>),
    Grouping(BoxedExpression<'a>),
}

fn primary(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    unimplemented!()
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
