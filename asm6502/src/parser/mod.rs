use crate::Token;
use nom::{Err, IResult, Needed};

enum Line<'a> {
    MacroInvocation(&'a str),
    Directive(Directive<'a>),
    Opcode,
}

enum Directive<'a> {
    MacroDecl(&'a str),
}

fn identifier<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a str> {
    map_next_token(input, |token| {
        if let Token::Identifier(_, name) = token {
            Some(*name)
        } else {
            None
        }
    })
}

fn macro_decl<'a>(input: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Directive<'a>> {
    let (input, macro_name) = identifier(input)?;
    map_next_token(input, |t| {
        if let Token::MacroStart(_) = t {
            Some(Directive::MacroDecl(macro_name))
        } else {
            None
        }
    })
}

fn map_next_token<'a, T, F>(input: &'a [Token<'a>], f: F) -> IResult<&'a [Token<'a>], T>
where
    F: FnOnce(&'a Token<'a>) -> Option<T>,
{
    input
        .get(0)
        .and_then(|token| f(token).map(|t| (&input[1..], t)))
        .ok_or(Err::Incomplete(Needed::Size(1)))
}
