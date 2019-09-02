mod expression;
mod types;

use expression::expression;
use fnv::FnvHashSet;
use nom::IResult;
use types::TokenSlice;

use crate::Token;

struct Parser<'a> {
    macro_names: FnvHashSet<&'a str>,
}

struct Line<'a> {
    label: Option<&'a str>,
    action: Option<&'static str>,
    arguments: &'a [Token<'a>],
    comment: Option<&'a str>,
}

fn line(input: TokenSlice) -> IResult<TokenSlice, Line> {
    unimplemented!()
}
