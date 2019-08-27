#[cfg(test)]
mod tests;
mod types;

use crate::Token;
use nom::{
    bytes::complete::{tag, take_while1},
    combinator::opt,
    sequence::{pair, terminated},
    IResult,
};
use types::{GenericToken, TokenSlice};

fn expression_tokens(input: TokenSlice) -> IResult<TokenSlice, TokenSlice> {
    take_while1(|t| {
        if let Token::SubExprStart
        | Token::SubExprEnd
        | Token::XorOperator
        | Token::StarOperator
        | Token::EqualsOperator
        | Token::BinLiteral(_)
        | Token::OctLiteral(_)
        | Token::CharacterLiteral(_)
        | Token::DecLiteral(_)
        | Token::ComplementOperator
        | Token::GreaterThanOperator
        | Token::GreaterThanOrEqualToOperator
        | Token::LessThanOperator
        | Token::LessThanOrEqualToOperator
        | Token::MinusOperator
        | Token::PlusOperator
        | Token::LeftShiftOperator
        | Token::RightShiftOperator
        | Token::NotEqualsOperator
        | Token::OrOperator
        | Token::AndOperator = t
        {
            true
        } else {
            false
        }
    })(input)
}
