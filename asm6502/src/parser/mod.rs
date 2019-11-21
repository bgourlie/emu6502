#[cfg(test)]
mod tests;

mod expression;
mod instruction;
mod token;
mod types;

use crate::parser::{token, types::Line};
use instruction::instruction;
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, separated_nonempty_list},
    sequence::{pair, preceded, separated_pair, terminated},
    IResult,
};
use types::TokenSlice;

pub fn maybe_comment_then_newline<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    preceded(opt(token::comment), token::newline)(input.into())
}

fn macro_decl<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(terminated(token::identifier, token::macro_start), |ident| {
        Line::MacroStart(ident)
    })(input.into())
}

fn macro_end<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::macro_end, |_| Line::MacroEnd)(input.into())
}

fn equals_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        separated_pair(
            token::identifier,
            token::equals_operator,
            expression::expression,
        ),
        |(ident, expr)| Line::Equals(ident, expr),
    )(input.into())
}

fn equ_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        separated_pair(
            token::identifier,
            token::equ_directive,
            expression::expression,
        ),
        |(ident, expr)| Line::Equ(ident, expr),
    )(input.into())
}

fn if_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(preceded(r#if, expression::expression), |expr| {
        Line::If(expr)
    })(input.into())
}

fn end_if_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::end_if, |_| Line::EndIf)(input.into())
}

fn error_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::error_directive, |msg| Line::Error(msg))(input.into())
}

fn noopt_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::noopt_directive, |_| Line::NoOpt)(input.into())
}

fn macro_invocation<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        pair(
            token::identifier,
            opt(separated_nonempty_list(
                token::comma,
                expression::expression,
            )),
        ),
        |(ident, maybe_list)| Line::MacroInvocation(ident, maybe_list),
    )(input.into())
}

pub fn parse<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Vec<Line<'a>>> {
    many0(alt((
        map(maybe_comment_then_newline, |_| Line::Empty),
        terminated(
            alt((
                macro_decl,
                macro_end,
                equals_operator,
                if_directive,
                end_if_directive,
                error_directive,
                noopt_directive,
                equ_directive,
                macro_invocation,
                map(instruction, |(op, operand)| Line::Instruction(op, operand)),
            )),
            maybe_comment_then_newline,
        ),
    )))(input.into())
}

#[cfg(test)]
fn tlex(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::lex(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
