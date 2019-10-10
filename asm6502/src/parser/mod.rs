mod expression;
mod types;

use crate::{parser::expression::Expression, Token};
use nom::{
    bytes::complete::take,
    combinator::{map, map_res},
    sequence::terminated,
    IResult,
};
use shared6502::Op;
use types::TokenSlice;

fn identifier(input: TokenSlice) -> IResult<TokenSlice, &str> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Identifier(identifier) = token[0] {
            Ok(identifier)
        } else {
            Err(())
        }
    })(input)
}

fn macro_start(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(
        terminated(
            identifier,
            map_res(take(1_usize), |token: TokenSlice| {
                if Token::MacroStart == token[0] {
                    Ok(())
                } else {
                    Err(())
                }
            }),
        ),
        |ident| Line::MacroStart(ident),
    )(input)
}

fn macro_end(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map_res(take(1_usize), |token: TokenSlice| {
        if Token::MacroEnd == token[0] {
            Ok(Line::MacroEnd)
        } else {
            Err(())
        }
    })(input)
}

fn comment(input: TokenSlice) -> IResult<TokenSlice, &str> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Comment(com) = token[0] {
            Ok(com)
        } else {
            Err(())
        }
    })(input)
}

fn op(input: TokenSlice) -> IResult<TokenSlice, Op> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Mnemonic(op) = token[0] {
            Ok(op)
        } else {
            Err(())
        }
    })(input)
}

enum Line<'a> {
    MacroStart(&'a str),
    MacroEnd,
    Instruction(Op, Operand<'a>),
    Directive,
}

#[derive(Debug)]
enum Operand<'a> {
    Absolute(Box<Expression<'a>>),
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
