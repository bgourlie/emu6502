#[cfg(test)]
mod tests;

use crate::types::Token;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
    character::complete::{char, digit1, hex_digit1, oct_digit1, one_of, space0, space1},
    combinator::{map, map_res, opt, peek},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use shared6502::Op;

#[derive(Debug, Default)]
pub struct Lexer<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn lex<I: Into<&'a str>>(input: I) -> Result<Self, Self> {
        let mut lexer = Lexer::default();
        let mut input = input.into();
        loop {
            match lex2(input) {
                Ok((remaining, token)) => {
                    lexer.tokens.push(token);
                    input = remaining;
                }
                Err(nom::Err::Incomplete(_n)) => {
                    break;
                }
                Err(e) => {
                    println!("{:?}", e);
                    break;
                }
            }
        }

        Ok(lexer)
    }
}

pub fn lex2<'a, T: Into<&'a str>>(input: T) -> IResult<&'a str, Token<'a>> {
    alt((
        alt((
            comment_token,
            error_directive_token,
            equ_directive_token,
            noopt_directive_token,
            macro_start_token,
            macro_end_token,
            macro_positional_arg_token,
            macro_invocation_count_arg_token,
            include_directive_token,
            if_start_token,
            else_token,
            if_end_token,
            mnemonic_token,
            immediate_prefix_token,
            define_directive_token("db", |_| Token::DbDirective),
            define_directive_token("dw", |_| Token::DwDirective),
            define_directive_token("ds", |_| Token::DsDirective),
            offset_operand_token("x", |_| Token::OffsetByXOperand),
            offset_operand_token("y", |_| Token::OffsetByYOperand),
            end_directive_token,
            identifier_token,
        )),
        alt((
            operator_token("=", |_| Token::EqualsOperator),
            dec_literal_token,
            hex_literal_token,
            oct_literal_token,
            bin_literal_token,
            operator_token("!=", |_| Token::NotEqualsOperator),
            operator_token("!", |_| Token::BangOperator),
            operator_token("~", |_| Token::ComplementOperator),
            operator_token("|", |_| Token::OrOperator),
            operator_token("^", |_| Token::XorOperator),
            operator_token("&", |_| Token::AndOperator),
            operator_token("+", |_| Token::PlusOperator),
            operator_token("-", |_| Token::MinusOperator),
            operator_token("*", |_| Token::StarOperator),
            operator_token("<<", |_| Token::LeftShiftOperator),
            operator_token(">>", |_| Token::RightShiftOperator),
            operator_token(">=", |_| Token::GreaterThanOrEqualToOperator),
            operator_token("<=", |_| Token::LessThanOrEqualToOperator),
            operator_token(">", |_| Token::GreaterThanOperator),
            operator_token("<", |_| Token::LessThanOperator),
            open_paren_token,
        )),
        alt((
            close_paren_token,
            string_literal_token,
            character_literal_token,
            comma_token,
            newline_token,
        )),
    ))(input.into())
}

pub fn lex<'a, T: Into<&'a str>>(input: T) -> IResult<&'a str, Vec<Token<'a>>> {
    many0(alt((
        alt((
            comment_token,
            error_directive_token,
            equ_directive_token,
            noopt_directive_token,
            macro_start_token,
            macro_end_token,
            macro_positional_arg_token,
            macro_invocation_count_arg_token,
            include_directive_token,
            if_start_token,
            else_token,
            if_end_token,
            mnemonic_token,
            immediate_prefix_token,
            define_directive_token("db", |_| Token::DbDirective),
            define_directive_token("dw", |_| Token::DwDirective),
            define_directive_token("ds", |_| Token::DsDirective),
            offset_operand_token("x", |_| Token::OffsetByXOperand),
            offset_operand_token("y", |_| Token::OffsetByYOperand),
            end_directive_token,
            identifier_token,
        )),
        alt((
            operator_token("=", |_| Token::EqualsOperator),
            dec_literal_token,
            hex_literal_token,
            oct_literal_token,
            bin_literal_token,
            operator_token("!=", |_| Token::NotEqualsOperator),
            operator_token("!", |_| Token::BangOperator),
            operator_token("~", |_| Token::ComplementOperator),
            operator_token("|", |_| Token::OrOperator),
            operator_token("^", |_| Token::XorOperator),
            operator_token("&", |_| Token::AndOperator),
            operator_token("+", |_| Token::PlusOperator),
            operator_token("-", |_| Token::MinusOperator),
            operator_token("*", |_| Token::StarOperator),
            operator_token("<<", |_| Token::LeftShiftOperator),
            operator_token(">>", |_| Token::RightShiftOperator),
            operator_token(">=", |_| Token::GreaterThanOrEqualToOperator),
            operator_token("<=", |_| Token::LessThanOrEqualToOperator),
            operator_token(">", |_| Token::GreaterThanOperator),
            operator_token("<", |_| Token::LessThanOperator),
            open_paren_token,
        )),
        alt((
            close_paren_token,
            string_literal_token,
            character_literal_token,
            comma_token,
            newline_token,
        )),
    )))(input.into())
}

fn newline(input: &str) -> IResult<&str, char> {
    preceded(
        space0,
        preceded(opt(char('\r')), nom::character::complete::newline),
    )(input)
}

fn newline_token(input: &str) -> IResult<&str, Token> {
    map(newline, |_| Token::Newline)(input)
}

fn end_directive_token(input: &str) -> IResult<&str, Token> {
    map(preceded(space0, tag_no_case("end")), |_| {
        Token::EndDirective
    })(input)
}

fn error_directive_token(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            delimited(space0, tag("ERROR ERROR ERROR"), space1),
            take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        Token::ErrorDirective,
    )(input)
}

fn comma_token(input: &str) -> IResult<&str, Token> {
    map(tag(","), |_| Token::Comma)(input)
}

fn offset_operand_token<'a, F>(
    register: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, Token<'a>>
where
    F: Fn(&'a str) -> Token<'a>,
{
    map(
        preceded(
            space0,
            preceded(char(','), preceded(space0, tag_no_case(register))),
        ),
        mapper,
    )
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn character_literal_token(input: &str) -> IResult<&str, Token> {
    map_res(
        delimited(char('\''), take(1_usize), char('\'')),
        |chr: &str| {
            let chr: char = chr.chars().nth(0_usize).unwrap();
            if chr.is_ascii() && !chr.is_ascii_control() {
                Ok(Token::CharacterLiteral(chr))
            } else {
                Err(())
            }
        },
    )(input)
}

fn immediate_prefix_token(input: &str) -> IResult<&str, Token> {
    map(char('#'), |_| Token::ImmediatePrefix)(input)
}

fn operator_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, Token<'a>>
where
    F: Fn(&'a str) -> Token<'a>,
{
    map(delimited(space0, tag(chars), space0), mapper)
}

fn mnemonic_implied<'a>(
    mnemonic: &'static str,
    op: Op,
) -> impl Fn(&'a str) -> IResult<&'a str, Op> {
    map(
        terminated(tag_no_case(mnemonic), comment_or_newline),
        move |_| op,
    )
}

fn mnemonic_operand<'a>(
    mnemonic: &'static str,
    op: Op,
) -> impl Fn(&'a str) -> IResult<&'a str, Op> {
    map(terminated(tag_no_case(mnemonic), space1), move |_| op)
}

fn mnemonic_token(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            space0,
            alt((
                alt((
                    mnemonic_operand("adc", Op::Adc),
                    mnemonic_operand("and", Op::And),
                    mnemonic_operand("asl", Op::Asl),
                    mnemonic_operand("bcc", Op::Bcc),
                    mnemonic_operand("bcs", Op::Bcs),
                    mnemonic_operand("beq", Op::Beq),
                    mnemonic_implied("bit", Op::Bit),
                    mnemonic_operand("bmi", Op::Bmi),
                    mnemonic_operand("bne", Op::Bne),
                    mnemonic_operand("bpl", Op::Bpl),
                    mnemonic_implied("brk", Op::Brk),
                    mnemonic_operand("bvc", Op::Bvc),
                    mnemonic_operand("bvs", Op::Bvs),
                    mnemonic_implied("clc", Op::Clc),
                    mnemonic_implied("cld", Op::Cld),
                    mnemonic_implied("cli", Op::Cli),
                    mnemonic_implied("clv", Op::Clv),
                    mnemonic_operand("cmp", Op::Cmp),
                    mnemonic_operand("cpx", Op::Cpx),
                    mnemonic_operand("cpy", Op::Cpy),
                    mnemonic_operand("dec", Op::Dec),
                )),
                alt((
                    mnemonic_implied("dex", Op::Dex),
                    mnemonic_implied("dey", Op::Dey),
                    mnemonic_operand("eor", Op::Eor),
                    mnemonic_operand("inc", Op::Inc),
                    mnemonic_implied("inx", Op::Inx),
                    mnemonic_implied("iny", Op::Iny),
                    mnemonic_operand("jmp", Op::Jmp),
                    mnemonic_operand("jsr", Op::Jsr),
                    mnemonic_operand("lda", Op::Lda),
                    mnemonic_operand("ldx", Op::Ldx),
                    mnemonic_operand("ldy", Op::Ldy),
                    mnemonic_operand("lsr", Op::Lsr),
                    mnemonic_implied("nop", Op::Nop),
                    mnemonic_operand("ora", Op::Ora),
                    mnemonic_implied("pha", Op::Pha),
                    mnemonic_implied("php", Op::Php),
                    mnemonic_implied("pla", Op::Pla),
                    mnemonic_implied("plp", Op::Plp),
                    mnemonic_operand("rol", Op::Rol),
                    mnemonic_operand("ror", Op::Ror),
                    mnemonic_implied("rti", Op::Rti),
                )),
                alt((
                    mnemonic_implied("rts", Op::Rts),
                    mnemonic_operand("sbc", Op::Sbc),
                    mnemonic_implied("sec", Op::Sec),
                    mnemonic_implied("sed", Op::Sed),
                    mnemonic_implied("sei", Op::Sei),
                    mnemonic_operand("sta", Op::Sta),
                    mnemonic_operand("stx", Op::Stx),
                    mnemonic_operand("sty", Op::Sty),
                    mnemonic_implied("tax", Op::Tax),
                    mnemonic_implied("tay", Op::Tay),
                    mnemonic_implied("tsx", Op::Tsx),
                    mnemonic_implied("txa", Op::Txa),
                    mnemonic_implied("txs", Op::Txs),
                    mnemonic_implied("tya", Op::Tya),
                )),
            )),
        ),
        Token::Mnemonic,
    )(input)
}

fn if_start_token(input: &str) -> IResult<&str, Token> {
    map(delimited(space0, tag_no_case("if"), space1), |_| {
        Token::IfStart
    })(input)
}

fn comment_or_newline(input: &str) -> IResult<&str, ()> {
    peek(alt((map(newline, |_| ()), map(comment_token, |_| ()))))(input)
}

fn if_end_token(input: &str) -> IResult<&str, Token> {
    map(delimited(space0, tag_no_case("endif"), space0), |_| {
        Token::IfEnd
    })(input)
}

fn else_token(input: &str) -> IResult<&str, Token> {
    map(delimited(space0, tag_no_case("else"), space0), |_| {
        Token::Else
    })(input)
}

fn macro_start_token(input: &str) -> IResult<&str, Token> {
    map(preceded(space1, tag_no_case("macro")), |_| {
        Token::MacroStart
    })(input)
}

fn macro_end_token(input: &str) -> IResult<&str, Token> {
    map(delimited(space0, tag_no_case("endm"), space0), |_| {
        Token::MacroEnd
    })(input)
}

fn macro_invocation_count_arg_token(input: &str) -> IResult<&str, Token> {
    map(preceded(space0, tag("\\?")), |_| Token::MacroExpansionCount)(input)
}

fn macro_positional_arg_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(
            preceded(space0, preceded(char('\\'), one_of("123456789"))),
            parse_u8_dec,
        ),
        Token::MacroPositionalArg,
    )(input)
}

fn noopt_directive_token(input: &str) -> IResult<&str, Token> {
    map(preceded(space0, tag_no_case("noopt")), |_| {
        Token::NoOptDirective
    })(input)
}

fn open_paren_token(input: &str) -> IResult<&str, Token> {
    map(char('('), |_| Token::OpenParen)(input)
}

fn close_paren_token(input: &str) -> IResult<&str, Token> {
    map(char(')'), |_| Token::CloseParen)(input)
}

fn equ_directive_token(input: &str) -> IResult<&str, Token> {
    map(delimited(space0, tag_no_case("equ"), space1), |_| {
        Token::EquDirective
    })(input)
}

fn define_directive_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, Token<'a>>
where
    F: Fn(&'a str) -> Token<'a>,
{
    map(delimited(space1, tag_no_case(chars), space1), mapper)
}

fn hex_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(pair(space0, char('$')), hex_digit1), parse_i32_hex),
        Token::HexLiteral,
    )(input)
}

fn dec_literal_token(input: &str) -> IResult<&str, Token> {
    map(map_res(preceded(space0, digit1), parse_i32_dec), |val| {
        Token::DecLiteral(val)
    })(input)
}

fn oct_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(pair(space0, char('@')), oct_digit1), parse_i32_oct),
        Token::OctLiteral,
    )(input)
}

fn bin_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(
            preceded(
                pair(space0, char('%')),
                take_while1(|chr| chr == '0' || chr == '1'),
            ),
            parse_i32_bin,
        ),
        Token::BinLiteral,
    )(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        delimited(
            pair(space0, char('"')),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            char('"'),
        ),
        Token::StringLiteral,
    )(input)
}

fn identifier_token(input: &str) -> IResult<&str, Token> {
    map_res(
        preceded(
            space0,
            preceded(
                valid_identifier_start,
                take_while1(|chr: char| {
                    chr.is_ascii_alphanumeric() || chr == '_' || chr == '\\' || chr == '?'
                }),
            ),
        ),
        |identifier| {
            // '/' and '?' are only valid when used together ("/?"), and we validate that here
            let slash_count = identifier.matches('\\').count();
            let question_count = identifier.matches('?').count();
            let macro_expansion_count = identifier.matches("\\?").count();

            if slash_count == question_count && question_count == macro_expansion_count {
                Ok(Token::Identifier(identifier))
            } else {
                Err(())
            }
        },
    )(input)
}

fn valid_identifier_start(input: &str) -> IResult<&str, &str> {
    map_res(peek(take(1_u32)), |first_char: &str| {
        if first_char.chars().nth(0).unwrap().is_ascii_alphabetic() {
            Ok(first_char)
        } else {
            Err(())
        }
    })(input)
}

fn include_directive_token(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            space0,
            delimited(
                tuple((tag_no_case("include"), space1, char('"'))),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
                char('"'),
            ),
        ),
        |path: &str| Token::IncludeDirective(path),
    )(input)
}

fn comment_token(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            space0,
            preceded(
                char(';'),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
            ),
        ),
        Token::Comment,
    )(input)
}

fn parse_i32_hex(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 16)
}

fn parse_i32_oct(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 8)
}

fn parse_i32_dec(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 10)
}

fn parse_i32_bin(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 2)
}

fn parse_u8_dec(input: char) -> Result<u8, std::num::ParseIntError> {
    let mut tmp = [0_u8; 1];
    u8::from_str_radix(char::encode_utf8(input, &mut tmp), 10)
}
