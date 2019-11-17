mod expression;
mod instruction;
mod token_parsers;
mod types;

use crate::parser::token_parsers::{comment, identifier, macro_start, newline};
use nom::{
    combinator::{map, opt},
    sequence::{preceded, terminated},
    IResult,
};
use shared6502::Op;
use types::TokenSlice;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Negation,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Multiply,
    Addition,
    Subtraction,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterThanOrEquals,
    LessThan,
    LessThanOrEquals,
    Complement,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Literal(i32),
    Symbol(&'a str),
    Unary(UnaryOperator, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, BinaryOperator, Box<Expression<'a>>),
    Grouping(Box<Expression<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Operand<'a> {
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

#[derive(Debug, PartialEq)]
enum Line<'a> {
    MacroStart(&'a str),
    MacroEnd,
    Instruction(Op, Operand<'a>),
    Directive,
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

#[test]
fn test_macro_decl() {
    let tokens = parse("foo macro\n");
    let (_, line) = macro_decl(&tokens).unwrap();
    assert_eq!(Line::MacroStart("foo"), line);
}

#[cfg(test)]
fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
