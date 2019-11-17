/// A collection of parsers that match a single token.
use crate::parser::types::TokenSlice;
use crate::Token;
use nom::{bytes::complete::take, combinator::map_res, IResult};
use shared6502::Op;

pub fn comment<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, &'a str> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Comment(com) = token[0] {
            Ok(com)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn identifier<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, &'a str> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Identifier(ident) = token[0] {
            Ok(ident)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn character_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, char> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::CharacterLiteral(chr) = token[0] {
            Ok(chr)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn dec_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::DecLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn hex_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::HexLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn bin_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::BinLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn oct_literal<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OctLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn newline<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Newline = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn bang_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::BangOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn star_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::StarOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn plus_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::PlusOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn equals_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::EqualsOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn minus_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::MinusOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn greater_than_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::GreaterThanOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn greater_than_or_equals_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::GreaterThanOrEqualToOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn less_than_operator<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::LessThanOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn less_than_or_equals_operator<'a, T: Into<TokenSlice<'a>>>(
    input: T,
) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::LessThanOrEqualToOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn immediate_prefix<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::ImmediatePrefix = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn open_paren<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OpenParen = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn close_paren<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::CloseParen = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn offset_x_suffix<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OffsetByXOperand = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn offset_y_suffix<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OffsetByYOperand = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn op<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, Op> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Mnemonic(op) = token[0] {
            Ok(op)
        } else {
            Err(())
        }
    })(input.into())
}

pub fn equals<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::EqualsOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn not_equals<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::NotEqualsOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn macro_start<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::MacroStart = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}

pub fn macro_end<'a, T: Into<TokenSlice<'a>>>(input: T) -> IResult<TokenSlice<'a>, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if Token::MacroEnd == token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input.into())
}
