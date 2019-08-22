#[cfg(test)]
mod tests;

use crate::Token;
use nom::{
    branch::alt,
    combinator::{map, map_res},
    sequence::{delimited, tuple},
    Err, IResult, Needed,
};
use std::rc::Rc;

enum Line<'a> {
    MacroInvocation(&'a str),
    Directive(Directive<'a>),
    Opcode,
}

enum Directive<'a> {
    MacroDecl(&'a str),
}

#[derive(Debug)]
enum Expr<'a> {
    Symbol(&'a str),
    Literal(i32),
    BinaryExpr(Rc<Box<Expr<'a>>>, Operator, Rc<Box<Expr<'a>>>),
    SubExpr(Rc<Box<Expr<'a>>>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Operator {
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanOrEquals,
    LessThanOrEquals,
    And,
    Or,
    Xor,
    Plus,
    Minus,
    Product,
    Complement,
    ShiftRight,
    ShiftLeft,
}

fn expr<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Expr<'a>> {
    alt((
        literal,
        map(identifier, |id| Expr::Symbol(id)),
        map(tuple((expr, operator, expr)), |(left, op, right)| {
            Expr::BinaryExpr(Rc::new(Box::new(left)), op, Rc::new(Box::new(right)))
        }),
        map(delimited(start_subexpr, expr, end_subexpr), |e| {
            Expr::SubExpr(Rc::new(Box::new(e)))
        }),
    ))(input)
}

fn start_subexpr<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], ()> {
    map_res(next_token, |token| {
        if let Token::SubExprStart = token {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn end_subexpr<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], ()> {
    map_res(next_token, |token| {
        if let Token::SubExprEnd = token {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn operator<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Operator> {
    map_res(next_token, |t| match t {
        Token::OrOperator => Ok(Operator::Or),
        Token::AndOperator => Ok(Operator::And),
        Token::PlusOperator => Ok(Operator::Plus),
        Token::MinusOperator => Ok(Operator::Minus),
        Token::StarOperator => Ok(Operator::Product),
        Token::EqualsOperator => Ok(Operator::Equals),
        Token::NotEqualsOperator => Ok(Operator::NotEquals),
        Token::GreaterThanOperator => Ok(Operator::GreaterThan),
        Token::GreaterThanOrEqualToOperator => Ok(Operator::GreaterThanOrEquals),
        Token::LessThanOperator => Ok(Operator::LessThan),
        Token::LessThanOrEqualToOperator => Ok(Operator::LessThanOrEquals),
        Token::LeftShiftOperator => Ok(Operator::ShiftLeft),
        Token::RightShiftOperator => Ok(Operator::ShiftRight),
        Token::ComplementOperator => Ok(Operator::Complement),
        Token::XorOperator => Ok(Operator::Xor),
        _ => Err(()),
    })(input)
}

fn literal<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Expr<'a>> {
    map_res(next_token, |token| match token {
        Token::BinLiteral(val)
        | Token::OctLiteral(val)
        | Token::HexLiteral(val)
        | Token::DecLiteral(val) => Ok(Expr::Literal(val)),
        Token::CharacterLiteral(val) => Ok(Expr::Literal(val as i32)),
        _ => Err(()),
    })(input)
}

fn identifier<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a str> {
    map_res(next_token, |token| {
        if let Token::Identifier(name) = token {
            Ok(name)
        } else {
            Err(())
        }
    })(input)
}

//fn macro_decl<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Directive<'a>> {
//    let (input, macro_name) = identifier(input)?;
//    map_next_token(|t| {
//        if let Token::MacroStart(_) = t {
//            Some(Directive::MacroDecl(macro_name))
//        } else {
//            None
//        }
//    })(input)
//}

fn next_token<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Token<'a>> {
    input
        .get(0)
        .and_then(|token| Some((&input[1..], *token)))
        .ok_or(Err::Incomplete(Needed::Size(1)))
}
