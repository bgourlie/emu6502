#[cfg(test)]
mod tests;

use crate::parser::{
    expression::expression,
    maybe_comment_then_newline,
    token_parsers::{
        close_paren, immediate_prefix, offset_x_suffix, offset_y_suffix, op, open_paren,
    },
    types::TokenSlice,
    Operand,
};
use nom::{
    branch::alt,
    combinator::map,
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};
use shared6502::Op;

pub fn instruction<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, (Op, Operand<'a>)> {
    pair(op, operand)(input.into())
}

fn operand<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Operand<'a>> {
    alt((
        map(maybe_comment_then_newline, |_| Operand::Implied),
        alt((
            operand_immediate,
            operand_indirect_indexed,
            operand_indexed_indirect,
            operand_indirect,
            operand_absolute_x,
            operand_absolute_y,
            operand_absolute_or_relative,
        )),
    ))(input.into())
}

fn operand_immediate<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(preceded(immediate_prefix, expression), |expr| {
        Operand::Immediate(Box::new(expr))
    })(input.into())
}

fn operand_absolute_or_relative<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(expression, |expr| {
        Operand::AbsoluteOrRelative(Box::new(expr))
    })(input.into())
}

fn operand_absolute_x<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(terminated(expression, offset_x_suffix), |expr| {
        Operand::AbsoluteX(Box::new(expr))
    })(input.into())
}

fn operand_absolute_y<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(terminated(expression, offset_y_suffix), |expr| {
        Operand::AbsoluteY(Box::new(expr))
    })(input.into())
}

fn operand_indexed_indirect<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        delimited(
            open_paren,
            terminated(expression, offset_x_suffix),
            close_paren,
        ),
        |expr| Operand::IndexedIndirect(Box::new(expr)),
    )(input.into())
}

fn operand_indirect<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(delimited(open_paren, expression, close_paren), |expr| {
        Operand::Indirect(Box::new(expr))
    })(input.into())
}

fn operand_indirect_indexed<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        terminated(
            delimited(open_paren, expression, close_paren),
            offset_y_suffix,
        ),
        |expr| Operand::IndirectIndexed(Box::new(expr)),
    )(input.into())
}
