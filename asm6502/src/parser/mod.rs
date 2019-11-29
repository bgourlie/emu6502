#[cfg(test)]
mod tests;

mod expression;
mod instruction;
mod token;
pub mod types;

use crate::parser::{
    token::identifier,
    types::{Expression, Line},
};
use instruction::instruction;
use nom::{
    branch::alt,
    combinator::{map, opt, peek},
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

fn if_statement<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(preceded(token::r#if, expression::expression), |expr| {
        Line::If(expr)
    })(input.into())
}

fn else_statement<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::r#else, |_| Line::Else)(input.into())
}

fn end_if_statement<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::end_if, |_| Line::EndIf)(input.into())
}

fn error_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::error_directive, |msg| Line::Error(msg))(input.into())
}

fn noopt_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::noopt_directive, |_| Line::NoOpt)(input.into())
}

fn ds_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        pair(
            opt(token::identifier),
            preceded(token::ds_directive, expression::expression),
        ),
        |(ident, expr)| Line::Ds(ident, expr),
    )(input.into())
}

fn db_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        pair(
            opt(token::identifier),
            preceded(token::db_directive, expression_list),
        ),
        |(ident, args)| Line::Db(ident, args),
    )(input.into())
}

fn dw_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        pair(
            opt(token::identifier),
            preceded(token::dw_directive, expression_list),
        ),
        |(ident, args)| Line::Dw(ident, args),
    )(input.into())
}

fn expression_list<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, Vec<Expression<'a>>> {
    separated_nonempty_list(token::comma, expression::expression)(input.into())
}

fn macro_invocation<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    alt((
        map(pair(token::identifier, expression_list), |(ident, args)| {
            Line::MacroInvocation(ident, args)
        }),
        map(
            terminated(token::identifier, peek(maybe_comment_then_newline)),
            |ident| Line::MacroInvocationOrLabel(ident),
        ),
    ))(input.into())
}

fn include_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token::include_directive, |file| Line::Include(file))(input.into())
}

fn end_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(preceded(token::end_directive, token::identifier), |label| {
        Line::End(label)
    })(input.into())
}

pub fn parse<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Vec<Line<'a>>> {
    many0(alt((
        map(maybe_comment_then_newline, |_| Line::Empty),
        terminated(
            alt((
                macro_decl,
                macro_end,
                equals_operator,
                if_statement,
                else_statement,
                end_if_statement,
                error_directive,
                noopt_directive,
                equ_directive,
                macro_invocation,
                ds_directive,
                db_directive,
                dw_directive,
                include_directive,
                end_directive,
                map(
                    pair(opt(identifier), instruction),
                    |(label, (op, operand))| Line::Instruction(label, op, operand),
                ),
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
