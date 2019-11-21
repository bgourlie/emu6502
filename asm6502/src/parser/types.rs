use crate::Token;
use nom::{
    error::{ErrorKind, ParseError},
    Err, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed, Slice,
};

use shared6502::Op;
use std::{
    iter::{Copied, Enumerate},
    ops::Index,
    slice::Iter,
};

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
    CurrentAddress,
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
pub enum Line<'a> {
    Empty,
    Instruction(Op, Operand<'a>),
    If(Expression<'a>),
    EndIf,
    Equ(&'a str, Expression<'a>),
    Error(&'a str),
    MacroStart(&'a str),
    MacroEnd,
    MacroInvocation(&'a str, Option<Vec<Expression<'a>>>),
    NoOpt,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TokenSlice<'a>(pub &'a [Token<'a>]);

impl<'a> From<&'a Vec<Token<'a>>> for TokenSlice<'a> {
    fn from(tokens: &'a Vec<Token<'a>>) -> Self {
        TokenSlice(tokens)
    }
}

impl<'a> From<&'a [Token<'a>]> for TokenSlice<'a> {
    fn from(tokens: &'a [Token<'a>]) -> Self {
        TokenSlice(tokens)
    }
}

impl<'a> InputIter for TokenSlice<'a> {
    type Item = Token<'a>;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Copied<Iter<'a, Token<'a>>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        let TokenSlice(inner) = self;
        inner.iter().copied()
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
