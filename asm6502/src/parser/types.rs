use crate::Token;
use nom::{
    error::{ErrorKind, ParseError},
    Compare, CompareResult, Err, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed,
    Slice,
};

use std::{
    cmp::PartialEq,
    iter::{Enumerate, Map},
    ops::Index,
    slice::Iter,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GenericToken {
    Comment,
    Newline,
    StarOperator,
    PlusOperator,
    MinusOperator,
    AndOperator,
    OrOperator,
    XorOperator,
    ComplementOperator,
    EqualsOperator,
    NotEqualsOperator,
    GreaterThanOrEqualsOperator,
    LessThanOrEqualsOperator,
    SubExprStart,
    SubExprEnd,
    MacroStart,
}

impl InputLength for GenericToken {
    fn input_len(&self) -> usize {
        1
    }
}

impl GenericToken {
    pub fn matches_token(self, token: Token) -> bool {
        match token {
            Token::Newline => self == GenericToken::Newline,
            Token::Comment(_) => self == GenericToken::Comment,
            Token::StarOperator => self == GenericToken::StarOperator,
            Token::PlusOperator => self == GenericToken::PlusOperator,
            Token::MinusOperator => self == GenericToken::MinusOperator,
            Token::EqualsOperator => self == GenericToken::EqualsOperator,
            Token::NotEqualsOperator => self == GenericToken::NotEqualsOperator,
            Token::LessThanOrEqualToOperator => self == GenericToken::LessThanOrEqualsOperator,
            Token::GreaterThanOrEqualToOperator => {
                self == GenericToken::GreaterThanOrEqualsOperator
            }
            Token::AndOperator => self == GenericToken::AndOperator,
            Token::OrOperator => self == GenericToken::OrOperator,
            Token::ComplementOperator => self == GenericToken::ComplementOperator,
            Token::SubExprStart => self == GenericToken::SubExprStart,
            Token::SubExprEnd => self == GenericToken::SubExprEnd,
            Token::MacroStart => self == GenericToken::MacroStart,
            _ => unimplemented!(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TokenSlice<'a>(pub &'a [Token<'a>]);

impl<'a> InputIter for TokenSlice<'a> {
    type Item = Token<'a>;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Map<Iter<'a, Self::Item>, fn(&Token<'a>) -> Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        let TokenSlice(inner) = self;
        inner.iter().map(|t| *t)
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        let TokenSlice(inner) = self;
        inner.iter().position(|b| predicate(*b))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.input_len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a> Slice<std::ops::RangeFrom<usize>> for TokenSlice<'a> {
    fn slice(&self, range: std::ops::RangeFrom<usize>) -> Self {
        let TokenSlice(inner) = self;
        TokenSlice(inner.slice(range))
    }
}

impl<'a> Index<usize> for TokenSlice<'a> {
    type Output = Token<'a>;

    fn index(&self, index: usize) -> &Self::Output {
        let TokenSlice(inner) = self;
        &inner[index]
    }
}

impl<'a> Compare<GenericToken> for TokenSlice<'a> {
    fn compare(&self, other: GenericToken) -> CompareResult {
        self.compare_no_case(other)
    }

    fn compare_no_case(&self, other: GenericToken) -> CompareResult {
        let TokenSlice(inner) = self;
        inner
            .get(0)
            .map(|t| other.matches_token(*t))
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
