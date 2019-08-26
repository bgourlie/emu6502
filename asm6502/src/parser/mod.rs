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
    terminated(
        take_while1(|t| {
            if let Token::SubExprStart | Token::SubExprEnd | Token::XorOperator = t {
                true
            } else {
                false
            }
        }),
        pair(opt(tag(GenericToken::Comment)), tag(GenericToken::Newline)),
    )(input)
}
