mod expression;
mod types;

use crate::{
    parser::expression::{expression, Expression},
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

fn newline(input: TokenSlice) -> IResult<TokenSlice, ()> {
    preceded(
        opt(comment),
        map_res(take(1_usize), |token: TokenSlice| {
            if let Token::Newline = token[0] {
                Ok(())
            } else {
                Err(())
            }
        }),
    )(input)
}

fn immediate_prefix(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::ImmediatePrefix = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

//fn open_paren(input: TokenSlice) -> IResult<TokenSlice, ()> {
//    map_res(take(1_usize), |token: TokenSlice| {
//        if let Token::P = token[0] {
//            Ok(())
//        } else {
//            Err(())
//        }
//    })(input)
//}

fn offset_x_suffix(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OffsetByXOperand = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn offset_y_suffix(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OffsetByYOperand = token[0] {
            Ok(())
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

fn operand_implied(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(newline, |_| Operand::Implied)(input)
}

fn operand_immediate(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(preceded(immediate_prefix, expression), newline),
        |expr| Operand::Immediate(Box::new(expr)),
    )(input)
}

fn operand_absolute_or_relative(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(terminated(expression, newline), |expr| {
        Operand::AbsoluteOrRelative(Box::new(expr))
    })(input)
}

fn operand_absolute_x(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(terminated(expression, offset_x_suffix), newline),
        |expr| Operand::AbsoluteX(Box::new(expr)),
    )(input)
}

fn operand_absolute_y(input: TokenSlice) -> IResult<TokenSlice, Operand> {
    map(
        terminated(terminated(expression, offset_y_suffix), newline),
        |expr| Operand::AbsoluteY(Box::new(expr)),
    )(input)
}

#[cfg(test)]
fn parse(input: &'static str) -> Vec<crate::Token> {
    let input = crate::Span::new(input);
    let (_, parsed) = crate::parse(input).unwrap();
    parsed.into_iter().map(|(_, token)| token).collect()
}
