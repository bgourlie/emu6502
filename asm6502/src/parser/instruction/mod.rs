#[cfg(test)]
mod tests;

use crate::parser::{
    expression::expression,
    maybe_comment_then_newline, token,
    types::{Operand, TokenSlice},
};
use nom::{
    branch::alt,
    combinator::{map, peek},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};
use shared6502::Op;
use std::rc::Rc;

pub fn instruction<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, (Op, Operand<'a>)> {
    pair(token::op, operand)(input.into())
}

fn operand<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Operand<'a>> {
    alt((
        map(peek(maybe_comment_then_newline), |_| Operand::Implied),
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
    map(preceded(token::immediate_prefix, expression), |expr| {
        Operand::Immediate(Rc::new(expr))
    })(input.into())
}

fn operand_absolute_or_relative<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(expression, |expr| {
        Operand::AbsoluteOrRelative(Rc::new(expr))
    })(input.into())
}

fn operand_absolute_x<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(terminated(expression, token::offset_x_suffix), |expr| {
        Operand::AbsoluteX(Rc::new(expr))
    })(input.into())
}

fn operand_absolute_y<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(terminated(expression, token::offset_y_suffix), |expr| {
        Operand::AbsoluteY(Rc::new(expr))
    })(input.into())
}

fn operand_indexed_indirect<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        delimited(
            token::open_paren,
            terminated(expression, token::offset_x_suffix),
            token::close_paren,
        ),
        |expr| Operand::IndexedIndirect(Rc::new(expr)),
    )(input.into())
}

fn operand_indirect<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        delimited(token::open_paren, expression, token::close_paren),
        |expr| Operand::Indirect(Rc::new(expr)),
    )(input.into())
}

fn operand_indirect_indexed<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Operand<'a>> {
    map(
        terminated(
            delimited(token::open_paren, expression, token::close_paren),
            token::offset_y_suffix,
        ),
        |expr| Operand::IndirectIndexed(Rc::new(expr)),
    )(input.into())
}
