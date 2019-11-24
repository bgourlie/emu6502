/// A collection of parsers that match a single token.
use crate::parser::types::TokenSlice;
use crate::Token;
use nom::{bytes::complete::take, combinator::map_res, IResult};
use shared6502::Op;

pub fn comment<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, &'a str> {
    match_token(input, |token| {
        if let Token::Comment(com) = token {
            Some(com)
        } else {
            None
        }
    })
}

pub fn identifier<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, &'a str> {
    match_token(input, |token| {
        if let Token::Identifier(ident) = token {
            Some(ident)
        } else {
            None
        }
    })
}

pub fn ds_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::DsDirective = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn db_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::DbDirective = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn r#else<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::Else = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn dw_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::DwDirective = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn macro_pos_arg<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, u8> {
    match_token(input, |token| {
        if let Token::MacroPositionalArg(arg_num) = token {
            Some(arg_num)
        } else {
            None
        }
    })
}

pub fn comma<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::Comma = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn r#if<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::IfStart = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn end_if<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::IfEnd = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn character_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, char> {
    match_token(input, |token| {
        if let Token::CharacterLiteral(chr) = token {
            Some(chr)
        } else {
            None
        }
    })
}

pub fn dec_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, u16> {
    match_token(input, |token| {
        if let Token::DecLiteral(val) = token {
            Some(val)
        } else {
            None
        }
    })
}

pub fn hex_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, u16> {
    match_token(input, |token| {
        if let Token::HexLiteral(val) = token {
            Some(val)
        } else {
            None
        }
    })
}

pub fn bin_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, u16> {
    match_token(input, |token| {
        if let Token::BinLiteral(val) = token {
            Some(val)
        } else {
            None
        }
    })
}

pub fn oct_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, u16> {
    match_token(input, |token| {
        if let Token::OctLiteral(val) = token {
            Some(val)
        } else {
            None
        }
    })
}

pub fn newline<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::Newline = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn bang_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::BangOperator = token {
            Some(())
        } else {
            None
        }
    })
}
pub fn complement_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::ComplementOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn and_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::AndOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn or_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::OrOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn xor_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::XorOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn star_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::StarOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn plus_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::PlusOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn equals_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::EqualsOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn not_equals_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::NotEqualsOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn minus_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::MinusOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn greater_than_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::GreaterThanOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn greater_than_or_equals_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::GreaterThanOrEqualToOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn less_than_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::LessThanOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn less_than_or_equals_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::LessThanOrEqualToOperator = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn immediate_prefix<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::ImmediatePrefix = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn open_paren<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::OpenParen = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn close_paren<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::CloseParen = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn offset_x_suffix<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::OffsetByXOperand = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn offset_y_suffix<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::OffsetByYOperand = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn op<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Op> {
    match_token(input, |token| {
        if let Token::Mnemonic(op) = token {
            Some(op)
        } else {
            None
        }
    })
}

pub fn equ_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::EquDirective = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn macro_start<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::MacroStart = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn macro_end<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::MacroEnd = token {
            Some(())
        } else {
            None
        }
    })
}

pub fn error_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, &'a str> {
    match_token(input, |token| {
        if let Token::ErrorDirective(msg) = token {
            Some(msg)
        } else {
            None
        }
    })
}

pub fn noopt_directive<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    match_token(input, |token| {
        if let Token::NoOptDirective = token {
            Some(())
        } else {
            None
        }
    })
}

fn match_token<'a, I: Into<TokenSlice<'a>>, F, T: Copy>(
    input: I,
    mapper: F,
) -> IResult<TokenSlice<'a>, T>
where
    F: Fn(Token<'a>) -> Option<T>,
{
    map_res(take(1_usize), |token: TokenSlice| {
        mapper(token[0]).ok_or_else(|| Result::<T, ()>::Err(()))
    })(input.into())
}
