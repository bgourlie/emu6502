mod expression;
mod token_parsers;
mod types;

use crate::{
    parser::{
        expression::{expression, Expression},
        token_parsers::{
            comment, identifier, immediate_prefix, newline, offset_x_suffix, offset_y_suffix,
        },
    },
    Token,
};
use nom::{
    bytes::complete::take,
    combinator::{map, map_res, opt},
    sequence::{preceded, terminated},
    IResult,
};
use shared6502::Op;
use types::TokenSlice;

#[derive(Debug)]
enum Line<'a> {
    MacroStart(&'a str),
    MacroEnd,
    Instruction(Op, Operand<'a>),
    Directive,
}

#[derive(Debug)]
enum Operand<'a> {
    AbsoluteOrRelative(Box<Expression<'a>>),
    AbsoluteX(Box<Expression<'a>>),
    AbsoluteY(Box<Expression<'a>>),
    Accumulator,
    IndexedIndirect(Box<Expression<'a>>),
    IndirectIndexed(Box<Expression<'a>>),
    Implied,
    Immediate(Box<Expression<'a>>),
    Indirect(Box<Expression<'a>>),
}

pub fn optional_comment_and_newline(input: TokenSlice) -> IResult<TokenSlice, ()> {
    preceded(opt(comment), newline)(input)
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

fn operand_implied(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(optional_comment_and_newline, |_| Operand::Implied)(input)
}

fn operand_immediate(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(
            preceded(immediate_prefix, expression),
            optional_comment_and_newline,
        ),
        |expr| Operand::Immediate(Box::new(expr)),
    )(input)
}

fn operand_absolute_or_relative(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(expression, optional_comment_and_newline),
        |expr| Operand::AbsoluteOrRelative(Box::new(expr)),
    )(input)
}

fn operand_absolute_x(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(
            terminated(expression, offset_x_suffix),
            optional_comment_and_newline,
        ),
        |expr| Operand::AbsoluteX(Box::new(expr)),
    )(input)
}

fn operand_absolute_y(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(
            terminated(expression, offset_y_suffix),
            optional_comment_and_newline,
        ),
        |expr| Operand::AbsoluteY(Box::new(expr)),
    )(input)
}

#[cfg(test)]
fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
