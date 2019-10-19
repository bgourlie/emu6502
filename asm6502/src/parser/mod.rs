mod expression;
mod token_parsers;
mod types;

use crate::parser::{
    expression::{expression, Expression},
    token_parsers::{
        comment, identifier, immediate_prefix, macro_start, newline, offset_x_suffix,
        offset_y_suffix,
    },
};
use nom::{
    combinator::{map, opt},
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

pub fn maybe_comment_then_newline<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    preceded(opt(comment), newline)(input.into())
}

fn macro_decl<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(terminated(identifier, macro_start), |ident| {
        Line::MacroStart(ident)
    })(input.into())
}

fn operand_implied<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(maybe_comment_then_newline, |_| Operand::Implied)(input.into())
}

fn operand_immediate<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        terminated(
            preceded(immediate_prefix, expression),
            maybe_comment_then_newline,
        ),
        |expr| Operand::Immediate(Box::new(expr)),
    )(input.into())
}

fn operand_absolute_or_relative<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(terminated(expression, maybe_comment_then_newline), |expr| {
        Operand::AbsoluteOrRelative(Box::new(expr))
    })(input.into())
}

fn operand_absolute_x<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        terminated(
            terminated(expression, offset_x_suffix),
            maybe_comment_then_newline,
        ),
        |expr| Operand::AbsoluteX(Box::new(expr)),
    )(input.into())
}

fn operand_absolute_y<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        terminated(
            terminated(expression, offset_y_suffix),
            maybe_comment_then_newline,
        ),
        |expr| Operand::AbsoluteY(Box::new(expr)),
    )(input.into())
}

#[cfg(test)]
fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
