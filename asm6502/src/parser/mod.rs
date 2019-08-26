#[cfg(test)]
mod tests;

use crate::Token;
use nom::{
    branch::alt,
    combinator::{map, map_res, opt},
    sequence::{delimited, pair, preceded, tuple},
    Err, IResult, Needed,
};
use std::rc::Rc;

enum Line<'a> {
    MacroInvocation(&'a str),
    Directive(Directive<'a>),
    Opcode,
}

enum Directive<'a> {
    MacroDecl(&'a str),
}

#[derive(Debug)]
enum Expr<'a> {
    Symbol(&'a str),
    Literal(i32),
    BinaryExpr(Rc<Box<Expr<'a>>>, Operator, Rc<Box<Expr<'a>>>),
    SubExpr(Rc<Box<Expr<'a>>>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Operator {
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanOrEquals,
    LessThanOrEquals,
    And,
    Or,
    Xor,
    Plus,
    Minus,
    Product,
    Complement,
    ShiftRight,
    ShiftLeft,
}

fn parse_expr<'a>(
    input: &'a [Token<'a>],
    expr: Option<Expr<'a>>,
    subexpr_nesting: u8,
) -> IResult<&'a [Token<'a>], Expr<'a>> {
    if let Some(expr) = expr {
        if let Ok((input, _)) = preceded(opt(comment), newline)(input) {
            if subexpr_nesting == 0 {
                Ok((input, expr))
            } else {
                Err(Err::Incomplete(Needed::Size(subexpr_nesting as _)))
            }
        } else {
            if let Ok((input, operator)) = operator(input) {
                let (input, rhs) = parse_expr(input, None, subexpr_nesting)?;
                Ok((
                    input,
                    Expr::BinaryExpr(Rc::new(Box::new(expr)), operator, Rc::new(Box::new(rhs))),
                ))
            } else {
                new_subexpr(input, subexpr_nesting)
            }
        }
    } else {
        if let Ok((input, expr)) = literal_or_symbol(input) {
            parse_expr(input, Some(expr), subexpr_nesting)
        } else {
            new_subexpr(input, subexpr_nesting)
        }
    }
}

fn new_subexpr<'a>(
    input: &'a [Token<'a>],
    current_nesting: u8,
) -> IResult<&'a [Token<'a>], Expr<'a>> {
    if let Ok((input, _)) = start_subexpr(input) {
        let (input, subexpr) = parse_expr(input, None, current_nesting + 1)?;
        Ok((input, Expr::SubExpr(Rc::new(Box::new(subexpr)))))
    } else {
        let (input, _) = end_subexpr(input)?;
        parse_expr(input, None, current_nesting - 1)
    }
}

fn literal_or_symbol<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Expr<'a>> {
    alt((literal, map(identifier, |id| Expr::Symbol(id))))(input)
}

fn start_subexpr<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], ()> {
    map_res(next_token, |token| {
        if let Token::SubExprStart = token {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn end_subexpr<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], ()> {
    map_res(next_token, |token| {
        if let Token::SubExprEnd = token {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn comment<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], ()> {
    map_res(next_token, |token| {
        if let Token::Comment(_) = token {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

fn newline<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], ()> {
    map_res(next_token, |token| {
        if let Token::Newline = token {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}
fn operator<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Operator> {
    map_res(next_token, |t| match t {
        Token::OrOperator => Ok(Operator::Or),
        Token::AndOperator => Ok(Operator::And),
        Token::PlusOperator => Ok(Operator::Plus),
        Token::MinusOperator => Ok(Operator::Minus),
        Token::StarOperator => Ok(Operator::Product),
        Token::EqualsOperator => Ok(Operator::Equals),
        Token::NotEqualsOperator => Ok(Operator::NotEquals),
        Token::GreaterThanOperator => Ok(Operator::GreaterThan),
        Token::GreaterThanOrEqualToOperator => Ok(Operator::GreaterThanOrEquals),
        Token::LessThanOperator => Ok(Operator::LessThan),
        Token::LessThanOrEqualToOperator => Ok(Operator::LessThanOrEquals),
        Token::LeftShiftOperator => Ok(Operator::ShiftLeft),
        Token::RightShiftOperator => Ok(Operator::ShiftRight),
        Token::ComplementOperator => Ok(Operator::Complement),
        Token::XorOperator => Ok(Operator::Xor),
        _ => Err(()),
    })(input)
}

fn literal<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Expr<'a>> {
    map_res(next_token, |token| match token {
        Token::BinLiteral(val)
        | Token::OctLiteral(val)
        | Token::HexLiteral(val)
        | Token::DecLiteral(val) => Ok(Expr::Literal(val)),
        Token::CharacterLiteral(val) => Ok(Expr::Literal(val as i32)),
        _ => Err(()),
    })(input)
}

fn identifier<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a str> {
    map_res(next_token, |token| {
        if let Token::Identifier(name) = token {
            Ok(name)
        } else {
            Err(())
        }
    })(input)
}

//fn macro_decl<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Directive<'a>> {
//    let (input, macro_name) = identifier(input)?;
//    map_next_token(|t| {
//        if let Token::MacroStart(_) = t {
//            Some(Directive::MacroDecl(macro_name))
//        } else {
//            None
//        }
//    })(input)
//}

fn next_token<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Token<'a>> {
    input
        .get(0)
        .and_then(|token| Some((&input[1..], *token)))
        .ok_or(Err::Incomplete(Needed::Size(1)))
}
