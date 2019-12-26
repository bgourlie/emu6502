use nom::{
    error::{ErrorKind, ParseError},
    Err, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed, Slice,
};
use std::rc::Rc;

use shared6502::Op;
use std::{
    iter::{Copied, Enumerate},
    ops::Index,
    slice::Iter,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Token<'a> {
    Newline,
    Comment(&'a str),
    Identifier(&'a str),
    CharacterLiteral(char),
    StringLiteral(&'a str),
    HexLiteral(u16),
    DecLiteral(u16),
    BinLiteral(u16),
    OctLiteral(u16),
    NoOptDirective,
    EquDirective,
    OpenParen,
    CloseParen,
    BangOperator,
    EqualsOperator,
    NotEqualsOperator,
    PlusOperator,
    MinusOperator,
    StarOperator,
    RightShiftOperator,
    LeftShiftOperator,
    GreaterThanOperator,
    GreaterThanOrEqualToOperator,
    LessThanOperator,
    LessThanOrEqualToOperator,
    ComplementOperator,
    AndOperator,
    OrOperator,
    XorOperator,
    MacroStart,
    MacroPositionalArg(u8),
    MacroExpansionCount,
    MacroEnd,
    IfStart,
    Else,
    IfEnd,
    Mnemonic(Op),
    ImmediatePrefix,
    OffsetByXOperand,
    OffsetByYOperand,
    Comma,
    ErrorDirective(&'a str),
    EndDirective,
    DbDirective,
    DsDirective,
    DwDirective,
    IncludeDirective(&'a str),
    Invalid(&'a str),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PositionedToken<'a> {
    pub(crate) token: Token<'a>,
    pub(crate) line: u32,
    pub(crate) span: (u16, u16),
}

impl<'a> PositionedToken<'a> {
    pub fn token(&self) -> Token<'a> {
        self.token
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn span(&self) -> (u16, u16) {
        self.span
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    LogicalNot,
    Negation,
    Complement,
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
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    CurrentAddress,
    Literal(u16),
    Symbol(Symbol<'a>),
    Unary(UnaryOperator, Rc<Expression<'a>>),
    Binary(Rc<Expression<'a>>, BinaryOperator, Rc<Expression<'a>>),
    Grouping(Rc<Expression<'a>>),
    Hi(Rc<Expression<'a>>),
    Lo(Rc<Expression<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Symbol<'a> {
    Named(&'a str),
    MacroArg(u8),
}

#[derive(Debug, PartialEq)]
pub enum Operand<'a> {
    AbsoluteOrRelative(Rc<Expression<'a>>),
    AbsoluteX(Rc<Expression<'a>>),
    AbsoluteY(Rc<Expression<'a>>),
    Accumulator,
    IndexedIndirect(Rc<Expression<'a>>),
    IndirectIndexed(Rc<Expression<'a>>),
    Implied,
    Immediate(Rc<Expression<'a>>),
    Indirect(Rc<Expression<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Line<'a> {
    Empty,
    Instruction(Option<&'a str>, Op, Operand<'a>),
    If(Rc<Expression<'a>>),
    Else,
    EndIf,
    Equ(&'a str, Rc<Expression<'a>>),
    Error(&'a str),
    MacroStart(&'a str),
    MacroEnd,
    MacroInvocation(&'a str, Vec<Rc<Expression<'a>>>),
    MacroInvocationOrLabel(&'a str),
    NoOpt,
    Ds(Option<&'a str>, Rc<Expression<'a>>),
    Db(Option<&'a str>, Vec<Rc<Expression<'a>>>),
    Dw(Option<&'a str>, Vec<Rc<Expression<'a>>>),
    Include(&'a str),
    End(&'a str),
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
    type IterElem = Copied<Iter<'a, Self::Item>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter().copied()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.0.iter().position(|b| predicate(*b))
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
        TokenSlice(self.0.slice(range))
    }
}

impl<'a> Index<usize> for TokenSlice<'a> {
    type Output = Token<'a>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'a> InputLength for TokenSlice<'a> {
    fn input_len(&self) -> usize {
        self.0.input_len()
    }
}

impl<'a> InputTake for TokenSlice<'a> {
    fn take(&self, count: usize) -> Self {
        TokenSlice(&self.0[0..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
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
        match (0..self.0.len()).find(|b| predicate(self.0[*b])) {
            Some(i) => Ok((TokenSlice(&self.0[i..]), TokenSlice(&self.0[..i]))),
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
        match (0..self.0.len()).find(|b| predicate(self.0[*b])) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            Some(i) => Ok((TokenSlice(&self.0[i..]), TokenSlice(&self.0[..i]))),
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
        match (0..self.0.len()).find(|b| predicate(self.0[*b])) {
            Some(i) => Ok((TokenSlice(&self.0[i..]), TokenSlice(&self.0[..i]))),
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
        match (0..self.0.len()).find(|b| predicate(self.0[*b])) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            Some(i) => Ok((TokenSlice(&self.0[i..]), TokenSlice(&self.0[..i]))),
            None => {
                if self.0.is_empty() {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}
