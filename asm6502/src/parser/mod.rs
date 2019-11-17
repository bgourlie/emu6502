mod expression;
mod instruction;
mod token_parsers;
mod types;

use crate::parser::{
    token_parsers::{comment, end_if, equals_operator, identifier, macro_start, newline, r#if},
    types::{Directive, Line},
};
use instruction::instruction;
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::many0,
    sequence::{preceded, terminated, tuple},
    IResult,
};
use types::TokenSlice;

pub fn maybe_comment_then_newline<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    preceded(opt(comment), newline)(input.into())
}

fn macro_decl<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(terminated(identifier, macro_start), |ident| {
        Line::Directive(Directive::MacroStart(ident))
    })(input.into())
}

fn macro_end<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(token_parsers::macro_end, |_| {
        Line::Directive(Directive::MacroEnd)
    })(input.into())
}

fn equ_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(
        tuple((identifier, equals_operator, expression::expression)),
        |(ident, _, expr)| Line::Directive(Directive::Equ(ident, expr)),
    )(input.into())
}

fn if_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(preceded(r#if, expression::expression), |expr| {
        Line::Directive(Directive::If(expr))
    })(input.into())
}

fn end_if_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Line<'a>> {
    map(end_if, |_| Line::Directive(Directive::EndIf))(input.into())
}

pub fn parse<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Vec<Line<'a>>> {
    many0(alt((
        map(maybe_comment_then_newline, |_| Line::Empty),
        terminated(
            alt((
                macro_decl,
                macro_end,
                equ_directive,
                if_directive,
                end_if_directive,
                map(instruction, |(op, operand)| Line::Instruction(op, operand)),
            )),
            maybe_comment_then_newline,
        ),
    )))(input.into())
}

#[cfg(test)]
fn tparse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::lex(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}

#[test]
fn test_macro_decl() {
    let tokens = tparse("foo macro\n");
    let (_, line) = macro_decl(&tokens).unwrap();
    assert_eq!(Line::Directive(Directive::MacroStart("foo")), line);
}

#[test]
fn test_macro_end() {
    let tokens = tparse("endm ; This is a comment\n");
    let (_, line) = macro_end(&tokens).unwrap();
    assert_eq!(Line::Directive(Directive::MacroEnd), line);
}
