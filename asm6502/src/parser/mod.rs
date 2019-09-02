#[cfg(test)]
mod tests;
mod types;

use crate::Token;
use nom::{
    bytes::complete::{take, take_while1},
    combinator::{map, map_res},
    error::ParseError,
    IResult,
};
use std::rc::Rc;
use types::TokenSlice;

type BoxedExpression<'a> = Rc<Box<Expression<'a>>>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum UnaryOperator {
    Negation,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BinaryOperator {
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

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    Literal(i32),
    Symbol(&'a str),
    Unary(UnaryOperator, BoxedExpression<'a>),
    Binary(BoxedExpression<'a>, BinaryOperator, BoxedExpression<'a>),
    Grouping(BoxedExpression<'a>),
}

/// An an expression with multiplication precedence (product or division).
fn multiplication(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (input, left) = unary(input)?;
    let (input, operator) = multiplication_operator(input)?;
    let (input, right) = unary(input)?;
    Ok((
        input,
        Expression::Binary(Rc::new(Box::new(left)), operator, Rc::new(Box::new(right))),
    ))
}

fn multiplication_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    map_res(take(1 as usize), |t: TokenSlice| {
        if let Token::StarOperator = t[0] {
            Ok(BinaryOperator::Multiply)
        } else {
            Err(())
        }
    })(input)
}

/// An expression preceded with a unary operator, or just a primary expression (symbol or literal)
fn unary(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    if let Ok((input, operator)) = unary_operator(input) {
        map(primary, move |expr| {
            Expression::Unary(operator, Rc::new(Box::new(expr)))
        })(input)
    } else {
        primary(input)
    }
}

fn unary_operator(input: TokenSlice) -> IResult<TokenSlice, UnaryOperator> {
    map_res(take(1 as usize), |t: TokenSlice| {
        if let Token::BangOperator = t[0] {
            Ok(UnaryOperator::Negation)
        } else {
            Err(())
        }
    })(input)
}

fn subexpr_start(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1 as usize), |t: TokenSlice| {
        if let Token::SubExprStart = t[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn subexpr_end(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1 as usize), |t: TokenSlice| {
        if let Token::SubExprEnd = t[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn primary(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    if let Ok((input, expr)) =
        map_res::<_, _, _, (), _, _, _>(take(1 as usize), |t: TokenSlice| match t[0] {
            Token::Identifier(name) => Ok(Expression::Symbol(name)),
            Token::CharacterLiteral(chr) => Ok(Expression::Literal(chr as i32)),
            Token::DecLiteral(val)
            | Token::HexLiteral(val)
            | Token::BinLiteral(val)
            | Token::OctLiteral(val) => Ok(Expression::Literal(val)),
            _ => Err(()),
        })(input)
    {
        Ok((input, expr))
    } else {
        let (input, _) = subexpr_start(input)?;
        let (input, expr) = primary(input)?;
        let (input, _) = subexpr_end(input)?;
        Ok((input, Expression::Grouping(Rc::new(Box::new(expr)))))
    }
}

fn expression_tokens(input: TokenSlice) -> IResult<TokenSlice, TokenSlice> {
    take_while1(|t| {
        if let Token::SubExprStart
        | Token::SubExprEnd
        | Token::XorOperator
        | Token::StarOperator
        | Token::EqualsOperator
        | Token::BangOperator
        | Token::BinLiteral(_)
        | Token::OctLiteral(_)
        | Token::CharacterLiteral(_)
        | Token::DecLiteral(_)
        | Token::Identifier(_)
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
