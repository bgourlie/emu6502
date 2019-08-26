#[cfg(test)]
mod tests;

use crate::Token;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    combinator::{map, map_res, opt},
    error::{ErrorKind, ParseError},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Compare, CompareResult, Err, IResult, InputLength, InputTake, InputTakeAtPosition, Needed,
};
use std::cmp::PartialEq;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum GenericToken {
    Comment,
    Newline,
}

impl InputLength for GenericToken {
    fn input_len(&self) -> usize {
        1
    }
}

#[derive(Copy, Clone, Debug)]
struct TokenSlice<'a>(&'a [Token<'a>]);

impl<'a> Compare<GenericToken> for TokenSlice<'a> {
    fn compare(&self, other: GenericToken) -> CompareResult {
        self.compare_no_case(other)
    }

    fn compare_no_case(&self, other: GenericToken) -> CompareResult {
        let TokenSlice(inner) = self;
        inner
            .get(0)
            .map(|t| match t {
                Token::Newline => other == GenericToken::Newline,
                Token::Comment(_) => other == GenericToken::Comment,
                _ => unimplemented!(),
            })
            .map(|found| {
                if found {
                    CompareResult::Ok
                } else {
                    CompareResult::Error
                }
            })
            .unwrap_or_else(|| CompareResult::Error)
    }
}

impl<'a> InputLength for TokenSlice<'a> {
    fn input_len(&self) -> usize {
        let TokenSlice(inner) = self;
        inner.input_len()
    }
}

impl<'a> InputTake for TokenSlice<'a> {
    fn take(&self, count: usize) -> Self {
        let TokenSlice(inner) = self;
        TokenSlice(&inner[0..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let TokenSlice(inner) = *self;
        let (prefix, suffix) = inner.split_at(count);
        (TokenSlice(suffix), TokenSlice(prefix))
    }
}

impl<'a> InputTakeAtPosition for TokenSlice<'a> {
    type Item = Token<'a>;

    fn split_at_position<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> Result<(Self, Self), Err<E>>
    where
        P: Fn(Self::Item) -> bool,
    {
        let TokenSlice(inner) = self;
        match (0..inner.len()).find(|b| predicate(inner[*b])) {
            Some(i) => Ok((TokenSlice(&inner[i..]), TokenSlice(&inner[..i]))),
            None => Err(Err::Incomplete(Needed::Size(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> Result<(Self, Self), Err<E>>
    where
        P: Fn(Self::Item) -> bool,
    {
        let TokenSlice(inner) = self;
        match (0..inner.len()).find(|b| predicate(inner[*b])) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            Some(i) => Ok((TokenSlice(&inner[i..]), TokenSlice(&inner[..i]))),
            None => Err(Err::Incomplete(Needed::Size(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> Result<(Self, Self), Err<E>>
    where
        P: Fn(Self::Item) -> bool,
    {
        let TokenSlice(inner) = self;
        match (0..inner.len()).find(|b| predicate(inner[*b])) {
            Some(i) => Ok((TokenSlice(&inner[i..]), TokenSlice(&inner[..i]))),
            None => Ok(self.take_split(self.input_len())),
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> Result<(Self, Self), Err<E>>
    where
        P: Fn(Self::Item) -> bool,
    {
        let TokenSlice(inner) = self;
        match (0..inner.len()).find(|b| predicate(inner[*b])) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            Some(i) => Ok((TokenSlice(&inner[i..]), TokenSlice(&inner[..i]))),
            None => {
                if inner.is_empty() {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

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
