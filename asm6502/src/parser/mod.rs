mod expression;
mod types;

use crate::{parser::expression::Expression, Token};
use fnv::FnvHashSet;
use nom::{
    bytes::complete::take,
    combinator::{map_res, opt},
    IResult,
};
use types::TokenSlice;

struct Parser<'a> {
    macro_names: FnvHashSet<&'a str>,
}

fn identifier(input: TokenSlice) -> IResult<TokenSlice, Option<&str>> {
    opt(map_res(take(1 as usize), |token: TokenSlice| {
        if let Token::Identifier(identifier) = token[0] {
            Ok(identifier)
        } else {
            Err(())
        }
    }))(input)
}

struct Line<'a> {
    label: Option<&'a str>,
    action: Option<&'static str>,
    arguments: &'a [Token<'a>],
    comment: Option<&'a str>,
}

#[derive(Debug)]
enum Operand<'a> {
    Absolute,
    AbsoluteX(Box<Expression<'a>>),
    AbsoluteY(Box<Expression<'a>>),
    Accumulator,
    IndexedIndirect(Box<Expression<'a>>),
    IndirectIndexed(Box<Expression<'a>>),
    Implied,
    Immediate(Box<Expression<'a>>),
    Indirect(Box<Expression<'a>>),
    Relative(Box<Expression<'a>>),
}

#[cfg(test)]
fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
