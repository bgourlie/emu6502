#[cfg(test)]
mod tests;

use super::types::TokenSlice;
use crate::parser::{
    identifier,
    token_parsers::{
        bang_operator, bin_literal, character_literal, close_paren, dec_literal, equals,
        greater_than_operator, greater_than_or_equals_operator, hex_literal, less_than_operator,
        less_than_or_equals_operator, minus_operator, not_equals, oct_literal, open_paren,
        plus_operator, star_operator,
    },
};
use nom::{branch::alt, combinator::map, IResult};

type BoxedExpression<'a> = Box<Expression<'a>>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Negation,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Multiply,
    Addition,
    Subtraction,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEquals,
    LessThan,
    LessThanOrEquals,
    Complement,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Literal(i32),
    Symbol(&'a str),
    Unary(UnaryOperator, BoxedExpression<'a>),
    Binary(BoxedExpression<'a>, BinaryOperator, BoxedExpression<'a>),
    Grouping(BoxedExpression<'a>),
}

/// Top-level expression parser
pub fn expression(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (input, left) = precedence4(input)?;
    if let Ok((input, operator)) = equality_operator(input) {
        let (input, right) = expression(input)?;
        Ok((
            input,
            Expression::Binary(Box::new(left), operator, Box::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses comparison expressions
fn precedence4(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (input, left) = precedence3(input)?;
    if let Ok((input, operator)) = comparison_operator(input) {
        let (input, right) = precedence4(input)?;
        Ok((
            input,
            Expression::Binary(Box::new(left), operator, Box::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses addition and subtraction expressions
fn precedence3(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (input, left) = precedence2(input)?;
    if let Ok((input, operator)) = addition_operator(input) {
        let (input, right) = precedence3(input)?;
        Ok((
            input,
            Expression::Binary(Box::new(left), operator, Box::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses multiplication expressions (and division, if we end up supporting it)
fn precedence2(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (input, left) = precedence1(input)?;
    if let Ok((input, operator)) = multiplication_operator(input) {
        let (input, right) = precedence2(input)?;
        Ok((
            input,
            Expression::Binary(Box::new(left), operator, Box::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses unary expressions
fn precedence1(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    if let Ok((input, operator)) = unary_operator(input) {
        let (input, expr) = precedence1(input)?;
        Ok((input, Expression::Unary(operator, Box::new(expr))))
    } else {
        precedence0(input)
    }
}

/// Parses expressions with the lowest precedence (literals, symbols, and subexpressions)
fn precedence0(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    if let Ok((input, expr)) = symbol_or_literal(input) {
        Ok((input, expr))
    } else {
        let (input, _) = open_paren(input)?;
        let (input, expr) = expression(input)?;
        let (input, _) = close_paren(input)?;
        Ok((input, Expression::Grouping(Box::new(expr))))
    }
}

fn symbol_or_literal(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    alt((
        map(identifier, |ident| Expression::Symbol(ident)),
        map(character_literal, |chr| Expression::Literal(chr as i32)),
        map(
            alt((dec_literal, hex_literal, bin_literal, oct_literal)),
            |val| Expression::Literal(val),
        ),
    ))(input)
}

fn unary_operator(input: TokenSlice) -> IResult<TokenSlice, UnaryOperator> {
    map(bang_operator, |_| UnaryOperator::Negation)(input)
}

fn multiplication_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    map(star_operator, |_| BinaryOperator::Multiply)(input)
}

fn addition_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        map(plus_operator, |_| BinaryOperator::Addition),
        map(minus_operator, |_| BinaryOperator::Subtraction),
    ))(input)
}

fn comparison_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        map(greater_than_operator, |_| BinaryOperator::GreaterThan),
        map(less_than_operator, |_| BinaryOperator::LessThan),
        map(greater_than_or_equals_operator, |_| {
            BinaryOperator::GreaterThanOrEquals
        }),
        map(less_than_or_equals_operator, |_| {
            BinaryOperator::LessThanOrEquals
        }),
    ))(input)
}

fn equality_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        map(equals, |_| BinaryOperator::Equals),
        map(not_equals, |_| BinaryOperator::NotEquals),
    ))(input)
}
