#[cfg(test)]
mod tests;

use super::types::TokenSlice;
use crate::Token;
use nom::{
    bytes::complete::{take, take_while1},
    combinator::map_res,
    IResult, InputLength, Needed,
};

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

pub fn expression(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (remaining, input) = expression_tokens(input)?;
    let (input, expr) = precedence5(input)?;
    if input.input_len() == 0 {
        Ok((remaining, expr))
    } else {
        // TODO: This is not the correct error type. Eventually do proper error reporting.
        Err(nom::Err::Incomplete(Needed::Size(input.input_len())))
    }
}

/// Parses equality expressions
fn precedence5(input: TokenSlice) -> IResult<TokenSlice, Expression> {
    let (input, left) = precedence4(input)?;
    if let Ok((input, operator)) = equality_operator(input) {
        let (input, right) = precedence5(input)?;
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
        let (input, expr) = precedence5(input)?;
        let (input, _) = subexpr_end(input)?;
        Ok((input, Expression::Grouping(Box::new(expr))))
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

fn unary_operator(input: TokenSlice) -> IResult<TokenSlice, UnaryOperator> {
    map_res(take(1 as usize), |t: TokenSlice| {
        if let Token::BangOperator = t[0] {
            Ok(UnaryOperator::Negation)
        } else {
            Err(())
        }
    })(input)
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

fn addition_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    map_res(take(1 as usize), |t: TokenSlice| match t[0] {
        Token::PlusOperator => Ok(BinaryOperator::Addition),
        Token::MinusOperator => Ok(BinaryOperator::Subtraction),
        _ => Err(()),
    })(input)
}

fn comparison_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    map_res(take(1 as usize), |t: TokenSlice| match t[0] {
        Token::GreaterThanOperator => Ok(BinaryOperator::GreaterThan),
        Token::LessThanOperator => Ok(BinaryOperator::LessThan),
        Token::GreaterThanOrEqualToOperator => Ok(BinaryOperator::GreaterThanOrEquals),
        Token::LessThanOrEqualToOperator => Ok(BinaryOperator::LessThanOrEquals),
        _ => Err(()),
    })(input)
}

fn equality_operator(input: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    map_res(take(1 as usize), |t: TokenSlice| match t[0] {
        Token::EqualsOperator => Ok(BinaryOperator::Equals),
        Token::NotEqualsOperator => Ok(BinaryOperator::NotEquals),
        _ => Err(()),
    })(input)
}
