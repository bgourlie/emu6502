#[cfg(test)]
mod tests;

mod expression;
mod instruction;
mod token;

use crate::{
    parser::token::identifier,
    types::{Expression, Line, TokenSlice},
};
use instruction::instruction;
use nom::{
    branch::alt,
    combinator::{map, opt, peek},
    multi::{many0, separated_nonempty_list},
    sequence::{pair, preceded, separated_pair, terminated},
    IResult,
};
use std::rc::Rc;

pub fn maybe_comment_then_newline(input: TokenSlice) -> IResult<TokenSlice, ()> {
    preceded(opt(token::comment), token::newline)(input)
}

fn macro_decl(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(terminated(token::identifier, token::macro_start), |ident| {
        Line::MacroStart(ident)
    })(input)
}

fn macro_end(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(token::macro_end, |_| Line::MacroEnd)(input)
}

fn equals_operator(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(
        separated_pair(
            token::identifier,
            token::equals_operator,
            expression::expression,
        ),
        |(ident, expr)| Line::Equals(ident, Rc::new(expr)),
    )(input)
}

fn equ_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(
        separated_pair(
            token::identifier,
            token::equ_directive,
            expression::expression,
        ),
        |(ident, expr)| Line::Equ(ident, Rc::new(expr)),
    )(input)
}

fn if_statement(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(preceded(token::r#if, expression::expression), |expr| {
        Line::If(expr)
    })(input)
}

fn else_statement(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(token::r#else, |_| Line::Else)(input)
}

fn end_if_statement(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(token::end_if, |_| Line::EndIf)(input)
}

fn error_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(token::error_directive, |msg| Line::Error(msg))(input)
}

fn noopt_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(token::noopt_directive, |_| Line::NoOpt)(input)
}

fn ds_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(
        pair(
            opt(token::identifier),
            preceded(token::ds_directive, expression::expression),
        ),
        |(ident, expr)| Line::Ds(ident, Rc::new(expr)),
    )(input)
}

fn db_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(
        pair(
            opt(token::identifier),
            preceded(token::db_directive, expression_list),
        ),
        |(ident, args)| Line::Db(ident, args),
    )(input)
}

fn dw_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(
        pair(
            opt(token::identifier),
            preceded(token::dw_directive, expression_list),
        ),
        |(ident, args)| Line::Dw(ident, args),
    )(input)
}

fn expression_list(input: TokenSlice) -> IResult<TokenSlice, Vec<Rc<Expression>>> {
    separated_nonempty_list(token::comma, map(expression::expression, Rc::new))(input)
}

fn macro_invocation(input: TokenSlice) -> IResult<TokenSlice, Line> {
    alt((
        map(pair(token::identifier, expression_list), |(ident, args)| {
            Line::MacroInvocation(ident, args)
        }),
        map(
            terminated(token::identifier, peek(maybe_comment_then_newline)),
            |ident| Line::MacroInvocationOrLabel(ident),
        ),
    ))(input)
}

fn include_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(token::include_directive, |file| Line::Include(file))(input)
}

fn end_directive(input: TokenSlice) -> IResult<TokenSlice, Line> {
    map(preceded(token::end_directive, token::identifier), |label| {
        Line::End(label)
    })(input)
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
