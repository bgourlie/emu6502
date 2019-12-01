#[cfg(test)]
mod tests;

use super::types::TokenSlice;
use crate::parser::{
    token::{self, identifier},
    types::{BinaryOperator, Expression, Symbol, UnaryOperator},
};
use nom::{
    branch::alt,
    combinator::{map, map_res},
    sequence::pair,
    IResult,
};
use std::rc::Rc;

/// Top-level expression parser
pub fn expression<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let (input, left) = precedence5(input.into())?;
    if let Ok((input, operator)) = equality_operator(input) {
        let (input, right) = expression(input)?;
        Ok((
            input,
            Expression::Binary(Rc::new(left), operator, Rc::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses comparison expressions
fn precedence5<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let (input, left) = precedence4(input.into())?;
    if let Ok((input, operator)) = comparison_operator(input) {
        let (input, right) = precedence5(input)?;
        Ok((
            input,
            Expression::Binary(Rc::new(left), operator, Rc::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses bitwise expressions
fn precedence4<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let (input, left) = precedence3(input.into())?;
    if let Ok((input, operator)) = bitwise_operator(input) {
        let (input, right) = precedence4(input)?;
        Ok((
            input,
            Expression::Binary(Rc::new(left), operator, Rc::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses addition and subtraction expressions
fn precedence3<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let (input, left) = precedence2(input.into())?;
    if let Ok((input, operator)) = addition_operator(input) {
        let (input, right) = precedence3(input)?;
        Ok((
            input,
            Expression::Binary(Rc::new(left), operator, Rc::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses multiplication expressions (and division, if we end up supporting it)
fn precedence2<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let (input, left) = precedence1(input)?;
    if let Ok((input, operator)) = multiplication_operator(input) {
        let (input, right) = precedence2(input)?;
        Ok((
            input,
            Expression::Binary(Rc::new(left), operator, Rc::new(right)),
        ))
    } else {
        Ok((input, left))
    }
}

/// Parses unary expressions
fn precedence1<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let input = input.into();
    if let Ok((input, operator)) = unary_operator(input) {
        let (input, expr) = precedence1(input)?;
        Ok((input, Expression::Unary(operator, Rc::new(expr))))
    } else {
        precedence0(input)
    }
}

/// Parses expressions with the lowest precedence (literals, symbols, and subexpressions)
fn precedence0<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    let input = input.into();
    if let Ok((input, expr)) = hi_or_lo(input) {
        Ok((input, expr))
    } else if let Ok((input, expr)) = symbol_or_literal(input) {
        Ok((input, expr))
    } else {
        let (input, _) = token::open_paren(input)?;
        let (input, expr) = expression(input)?;
        let (input, _) = token::close_paren(input)?;
        Ok((input, Expression::Grouping(Rc::new(expr))))
    }
}

fn symbol_or_literal<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Expression<'a>> {
    alt((
        map(token::identifier, |ident| {
            Expression::Symbol(Symbol::Named(ident))
        }),
        map(token::macro_pos_arg, |pos| {
            Expression::Symbol(Symbol::MacroArg(pos))
        }),
        map(token::character_literal, |chr| {
            Expression::Literal(chr as u16)
        }),
        map(
            alt((
                token::dec_literal,
                token::hex_literal,
                token::bin_literal,
                token::oct_literal,
            )),
            Expression::Literal,
        ),
        map(token::star_operator, |_| Expression::CurrentAddress),
    ))(input.into())
}

fn hi_or_lo<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Expression<'a>> {
    map_res(pair(identifier, expression), |(ident, expr)| {
        if ident.eq_ignore_ascii_case("hi") {
            Ok(Expression::Hi(Rc::new(expr)))
        } else if ident.eq_ignore_ascii_case("lo") {
            Ok(Expression::Lo(Rc::new(expr)))
        } else {
            Err(())
        }
    })(input.into())
}

fn unary_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, UnaryOperator> {
    alt((
        map(token::minus_operator, |_| UnaryOperator::Negation),
        map(token::bang_operator, |_| UnaryOperator::LogicalNot),
        map(token::complement_operator, |_| UnaryOperator::Complement),
    ))(input.into())
}

fn multiplication_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, BinaryOperator> {
    map(token::star_operator, |_| BinaryOperator::Multiply)(input.into())
}

fn bitwise_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, BinaryOperator> {
    alt((
        map(token::and_operator, |_| BinaryOperator::And),
        map(token::or_operator, |_| BinaryOperator::Or),
        map(token::xor_operator, |_| BinaryOperator::Xor),
    ))(input.into())
}

fn addition_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, BinaryOperator> {
    alt((
        map(token::plus_operator, |_| BinaryOperator::Addition),
        map(token::minus_operator, |_| BinaryOperator::Subtraction),
    ))(input.into())
}

fn comparison_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, BinaryOperator> {
    alt((
        map(token::greater_than_operator, |_| {
            BinaryOperator::GreaterThan
        }),
        map(token::less_than_operator, |_| BinaryOperator::LessThan),
        map(token::greater_than_or_equals_operator, |_| {
            BinaryOperator::GreaterThanOrEquals
        }),
        map(token::less_than_or_equals_operator, |_| {
            BinaryOperator::LessThanOrEquals
        }),
    ))(input.into())
}

fn equality_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, BinaryOperator> {
    alt((
        map(token::equals_operator, |_| BinaryOperator::Equals),
        map(token::not_equals_operator, |_| BinaryOperator::NotEquals),
    ))(input.into())
}
