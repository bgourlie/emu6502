use crate::Token;
use nom::{combinator::map_res, Err, IResult, Needed};
use std::rc::Rc;

enum Line<'a> {
    MacroInvocation(&'a str),
    Directive(Directive<'a>),
    Opcode,
}

enum Directive<'a> {
    MacroDecl(&'a str),
}

enum Expr<'a> {
    Symbol(&'a str),
    Literal(i32),
    SubExpr(Rc<Box<Expr<'a>>>, Operator, Rc<Box<Expr<'a>>>),
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

//fn expr<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Expr<'a>> {}

fn operator<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Operator> {
    map_res(next_token(), |t| {
        match t {
            Token::OrOperator(_) => Ok(Operator::Or),
            Token::AndOperator(_) => Ok(Operator::And),
            Token::PlusOperator(_) => Ok(Operator::Plus),
            Token::MinusOperator(_) => Ok(Operator::Minus),
            Token::StarOperator(_) => Ok(Operator::Product),
            Token::EqualsOperator(_) => Ok(Operator::Equals),
            Token::NotEqualsOperator(_) => Ok(Operator::NotEquals),
            Token::GreaterThanOperator(_) => Ok(Operator::GreaterThan),
            Token::GreaterThanOrEqualToOperator(_) => Ok(Operator::GreaterThanOrEquals),
            Token::LessThanOperator(_) => Ok(Operator::LessThan),
            Token::LessThanOrEqualToOperator(_) => Ok(Operator::LessThanOrEquals),
            Token::LeftShiftOperator(_) => Ok(Operator::ShiftLeft),
            Token::RightShiftOperator(_) => Ok(Operator::ShiftRight),
            Token::ComplementOperator(_) => Ok(Operator::Complement),
            Token::XorOperator(_) => Ok(Operator::Xor),
            _  => Err(())
        }
    })(input)
}

fn literal<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Expr<'a>> {
    map_res(next_token(), |token| match token {
        Token::BinLiteral(_, val)
        | Token::OctLiteral(_, val)
        | Token::HexLiteral(_, val)
        | Token::DecLiteral(_, val) => Ok(Expr::Literal(val)),
        Token::CharacterLiteral(_, val) => Ok(Expr::Literal(val as i32)),
        _ => Err(()),
    })(input)
}

fn identifier<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a str> {
    map_res(next_token(), |token| {
        if let Token::Identifier(_, name) = token {
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

fn next_token<'a>() -> impl Fn(&'a [Token<'a>]) -> IResult<&'a [Token<'a>], Token<'a>> {
    |input: &'a [Token<'a>]| {
        input
            .get(0)
            .and_then(|token| Some((&input[1..], *token)))
            .ok_or(Err::Incomplete(Needed::Size(1)))
    }
}
