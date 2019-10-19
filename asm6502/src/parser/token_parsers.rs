/// A collection of parsers that match a single token.
use crate::parser::types::TokenSlice;
use crate::Token;
use nom::{bytes::complete::take, combinator::map_res, IResult};
use shared6502::Op;

pub fn comment(input: TokenSlice) -> IResult<TokenSlice, &str> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Comment(com) = token[0] {
            Ok(com)
        } else {
            Err(())
        }
    })(input)
}

pub fn identifier(input: TokenSlice) -> IResult<TokenSlice, &str> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Identifier(ident) = token[0] {
            Ok(ident)
        } else {
            Err(())
        }
    })(input)
}

pub fn character_literal(input: TokenSlice) -> IResult<TokenSlice, char> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::CharacterLiteral(chr) = token[0] {
            Ok(chr)
        } else {
            Err(())
        }
    })(input)
}

pub fn dec_literal(input: TokenSlice) -> IResult<TokenSlice, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::DecLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input)
}

pub fn hex_literal(input: TokenSlice) -> IResult<TokenSlice, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::HexLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input)
}

pub fn bin_literal(input: TokenSlice) -> IResult<TokenSlice, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::BinLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input)
}

pub fn oct_literal(input: TokenSlice) -> IResult<TokenSlice, i32> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OctLiteral(val) = token[0] {
            Ok(val)
        } else {
            Err(())
        }
    })(input)
}

pub fn newline(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Newline = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn bang_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::BangOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn star_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::StarOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn plus_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::PlusOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn minus_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::MinusOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn greater_than_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::GreaterThanOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn greater_than_or_equals_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::GreaterThanOrEqualToOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn less_than_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::LessThanOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn less_than_or_equals_operator(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::LessThanOrEqualToOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn immediate_prefix(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::ImmediatePrefix = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn open_paren(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OpenParen = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn close_paren(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::CloseParen = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn offset_x_suffix(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OffsetByXOperand = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn offset_y_suffix(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::OffsetByYOperand = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn op(input: TokenSlice) -> IResult<TokenSlice, Op> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::Mnemonic(op) = token[0] {
            Ok(op)
        } else {
            Err(())
        }
    })(input)
}

pub fn equals(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::EqualsOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}

pub fn not_equals(input: TokenSlice) -> IResult<TokenSlice, ()> {
    map_res(take(1_usize), |token: TokenSlice| {
        if let Token::NotEqualsOperator = token[0] {
            Ok(())
        } else {
            Err(())
        }
    })(input)
}
