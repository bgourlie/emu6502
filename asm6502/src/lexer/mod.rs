#[cfg(test)]
mod tests;

use crate::types::{LexErr, LexErrorKind, LexInput, Token};
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
        let mut input = LexInput(input.into());
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

pub fn lex2<'a, T: Into<LexInput<'a>>>(input: T) -> IResult<LexInput<'a>, Token<'a>, LexErr<'a>> {
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

pub fn err_mapper<F>(
    mapper: F,
) -> impl FnOnce(nom::Err<(LexInput, nom::error::ErrorKind)>) -> nom::Err<LexErr>
where
    F: FnOnce() -> LexErrorKind,
{
    |err| match err {
        nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
        nom::Err::Failure((i, _)) => nom::Err::Failure(LexErr(i, mapper())),
        nom::Err::Error((i, _)) => nom::Err::Error(LexErr(i, mapper())),
    }
}

pub fn lex<'a, T: Into<LexInput<'a>>>(
    input: T,
) -> IResult<LexInput<'a>, Vec<Token<'a>>, LexErr<'a>> {
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

fn comment_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        preceded(
            space0,
            preceded(
                char(';'),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
            ),
        ),
        |i: LexInput| Token::Comment(i.0),
    )(input)
    .map_err(err_mapper(|| LexErrorKind::Comment))
}

fn newline(input: LexInput) -> IResult<LexInput, char, LexErr> {
    preceded(
        space0,
        preceded(opt(char('\r')), nom::character::complete::newline),
    )(input)
    .map_err(err_mapper(|| LexErrorKind::Newline))
}

fn newline_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(newline, |_| Token::Newline)(input)
}

fn end_directive_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(preceded(space0, tag_no_case("end")), |_| {
        Token::EndDirective
    })(input)
    .map_err(err_mapper(|| LexErrorKind::EndDirective))
}

fn error_directive_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        preceded(
            delimited(space0, tag("ERROR ERROR ERROR"), space1),
            take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        |i: LexInput| Token::ErrorDirective(i.0),
    )(input)
    .map_err(err_mapper(|| LexErrorKind::ErrorDirective))
}

fn comma_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(tag(","), |_| Token::Comma)(input).map_err(err_mapper(|| LexErrorKind::Comma))
}

fn offset_operand_token<'a, F>(
    register: &'static str,
    mapper: F,
) -> impl Fn(LexInput<'a>) -> IResult<LexInput<'a>, Token<'a>, LexErr>
where
    F: Fn(LexInput<'a>) -> Token<'a> + Copy,
{
    move |input| {
        map(
            preceded(
                space0,
                preceded(char(','), preceded(space0, tag_no_case(register))),
            ),
            mapper,
        )(input)
        .map_err(err_mapper(|| LexErrorKind::OffsetOperand))
    }
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn character_literal_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map_res(
        delimited(char('\''), take(1_usize), char('\'')),
        |chr: LexInput| {
            let chr: char = chr.0.chars().nth(0_usize).unwrap();
            if chr.is_ascii() && !chr.is_ascii_control() {
                Ok(Token::CharacterLiteral(chr))
            } else {
                Err(())
            }
        },
    )(input)
    .map_err(err_mapper(|| LexErrorKind::CharacterLiteral))
}

fn immediate_prefix_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(char('#'), |_| Token::ImmediatePrefix)(input)
        .map_err(err_mapper(|| LexErrorKind::ImmediatePrefix))
}

fn operator_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(LexInput<'a>) -> IResult<LexInput<'a>, Token<'a>, LexErr>
where
    F: Fn(LexInput<'a>) -> Token<'a> + Copy,
{
    move |input| {
        map(delimited(space0, tag(chars), space0), mapper)(input)
            .map_err(err_mapper(|| LexErrorKind::Operator))
    }
}

// TODO: Make lazy
fn mnemonic_implied<'a, F>(
    mnemonic: &'static str,
    mapper: F,
) -> impl Fn(LexInput<'a>) -> IResult<LexInput<'a>, Op, LexErr>
where
    F: Copy + Fn(LexInput<'a>) -> Op,
{
    move |input| {
        map(
            terminated(tag_no_case(mnemonic), peek_comment_or_newline),
            mapper,
        )(input)
    }
}

fn mnemonic_operand<'a, F>(
    mnemonic: &'static str,
    mapper: F,
) -> impl Fn(LexInput<'a>) -> IResult<LexInput<'a>, Op, LexErr>
where
    F: Copy + Fn(LexInput<'a>) -> Op,
{
    move |input| {
        map(terminated(tag_no_case(mnemonic), space1), mapper)(input)
            .map_err(err_mapper(|| LexErrorKind::Mnemonic(mnemonic)))
    }
}

fn mnemonic_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        preceded(
            space0,
            alt((
                alt((
                    mnemonic_operand("adc", |_| Op::Adc),
                    mnemonic_operand("and", |_| Op::And),
                    mnemonic_operand("asl", |_| Op::Asl),
                    mnemonic_operand("bcc", |_| Op::Bcc),
                    mnemonic_operand("bcs", |_| Op::Bcs),
                    mnemonic_operand("beq", |_| Op::Beq),
                    mnemonic_implied("bit", |_| Op::Bit),
                    mnemonic_operand("bmi", |_| Op::Bmi),
                    mnemonic_operand("bne", |_| Op::Bne),
                    mnemonic_operand("bpl", |_| Op::Bpl),
                    mnemonic_implied("brk", |_| Op::Brk),
                    mnemonic_operand("bvc", |_| Op::Bvc),
                    mnemonic_operand("bvs", |_| Op::Bvs),
                    mnemonic_implied("clc", |_| Op::Clc),
                    mnemonic_implied("cld", |_| Op::Cld),
                    mnemonic_implied("cli", |_| Op::Cli),
                    mnemonic_implied("clv", |_| Op::Clv),
                    mnemonic_operand("cmp", |_| Op::Cmp),
                    mnemonic_operand("cpx", |_| Op::Cpx),
                    mnemonic_operand("cpy", |_| Op::Cpy),
                    mnemonic_operand("dec", |_| Op::Dec),
                )),
                alt((
                    mnemonic_implied("dex", |_| Op::Dex),
                    mnemonic_implied("dey", |_| Op::Dey),
                    mnemonic_operand("eor", |_| Op::Eor),
                    mnemonic_operand("inc", |_| Op::Inc),
                    mnemonic_implied("inx", |_| Op::Inx),
                    mnemonic_implied("iny", |_| Op::Iny),
                    mnemonic_operand("jmp", |_| Op::Jmp),
                    mnemonic_operand("jsr", |_| Op::Jsr),
                    mnemonic_operand("lda", |_| Op::Lda),
                    mnemonic_operand("ldx", |_| Op::Ldx),
                    mnemonic_operand("ldy", |_| Op::Ldy),
                    mnemonic_operand("lsr", |_| Op::Lsr),
                    mnemonic_implied("nop", |_| Op::Nop),
                    mnemonic_operand("ora", |_| Op::Ora),
                    mnemonic_implied("pha", |_| Op::Pha),
                    mnemonic_implied("php", |_| Op::Php),
                    mnemonic_implied("pla", |_| Op::Pla),
                    mnemonic_implied("plp", |_| Op::Plp),
                    mnemonic_operand("rol", |_| Op::Rol),
                    mnemonic_operand("ror", |_| Op::Ror),
                    mnemonic_implied("rti", |_| Op::Rti),
                )),
                alt((
                    mnemonic_implied("rts", |_| Op::Rts),
                    mnemonic_operand("sbc", |_| Op::Sbc),
                    mnemonic_implied("sec", |_| Op::Sec),
                    mnemonic_implied("sed", |_| Op::Sed),
                    mnemonic_implied("sei", |_| Op::Sei),
                    mnemonic_operand("sta", |_| Op::Sta),
                    mnemonic_operand("stx", |_| Op::Stx),
                    mnemonic_operand("sty", |_| Op::Sty),
                    mnemonic_implied("tax", |_| Op::Tax),
                    mnemonic_implied("tay", |_| Op::Tay),
                    mnemonic_implied("tsx", |_| Op::Tsx),
                    mnemonic_implied("txa", |_| Op::Txa),
                    mnemonic_implied("txs", |_| Op::Txs),
                    mnemonic_implied("tya", |_| Op::Tya),
                )),
            )),
        ),
        Token::Mnemonic,
    )(input)
}

fn if_start_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(delimited(space0, tag_no_case("if"), space1), |_| {
        Token::IfStart
    })(input)
    .map_err(err_mapper(|| LexErrorKind::If))
}

fn peek_comment_or_newline(input: LexInput) -> IResult<LexInput, (), LexErr> {
    peek(alt((map(newline, |_| ()), map(comment_token, |_| ()))))(input)
}

fn if_end_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(delimited(space0, tag_no_case("endif"), space0), |_| {
        Token::IfEnd
    })(input)
    .map_err(err_mapper(|| LexErrorKind::EndIf))
}

fn else_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(delimited(space0, tag_no_case("else"), space0), |_| {
        Token::Else
    })(input)
    .map_err(err_mapper(|| LexErrorKind::Else))
}

fn macro_start_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(preceded(space1, tag_no_case("macro")), |_| {
        Token::MacroStart
    })(input)
    .map_err(err_mapper(|| LexErrorKind::MacroStart))
}

fn macro_end_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(delimited(space0, tag_no_case("endm"), space0), |_| {
        Token::MacroEnd
    })(input)
    .map_err(err_mapper(|| LexErrorKind::MacroEnd))
}

fn macro_invocation_count_arg_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(preceded(space0, tag("\\?")), |_| Token::MacroExpansionCount)(input)
        .map_err(err_mapper(|| LexErrorKind::MacroInvocationCount))
}

fn macro_positional_arg_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        map_res(
            preceded(space0, preceded(char('\\'), one_of("123456789"))),
            parse_u8_dec,
        ),
        Token::MacroPositionalArg,
    )(input)
    .map_err(err_mapper(|| LexErrorKind::MacroPositionalArg))
}

fn noopt_directive_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(preceded(space0, tag_no_case("noopt")), |_| {
        Token::NoOptDirective
    })(input)
    .map_err(err_mapper(|| LexErrorKind::NoOptDirective))
}

fn open_paren_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(char('('), |_| Token::OpenParen)(input).map_err(err_mapper(|| LexErrorKind::OpenParen))
}

fn close_paren_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(char(')'), |_| Token::CloseParen)(input).map_err(err_mapper(|| LexErrorKind::CloseParen))
}

fn equ_directive_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(delimited(space0, tag_no_case("equ"), space1), |_| {
        Token::EquDirective
    })(input)
    .map_err(err_mapper(|| LexErrorKind::EquDirective))
}

fn define_directive_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(LexInput<'a>) -> IResult<LexInput<'a>, Token<'a>, LexErr<'a>>
where
    F: Fn(LexInput<'a>) -> Token<'a> + Copy,
{
    move |input| {
        map(delimited(space1, tag_no_case(chars), space1), mapper)(input)
            .map_err(err_mapper(|| LexErrorKind::DefineDirective))
    }
}

fn hex_literal_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        map_res(preceded(pair(space0, char('$')), hex_digit1), parse_i32_hex),
        Token::HexLiteral,
    )(input)
    .map_err(err_mapper(|| LexErrorKind::HexLiteral))
}

fn dec_literal_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(map_res(preceded(space0, digit1), parse_i32_dec), |val| {
        Token::DecLiteral(val)
    })(input)
    .map_err(err_mapper(|| LexErrorKind::DecLiteral))
}

fn oct_literal_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        map_res(preceded(pair(space0, char('@')), oct_digit1), parse_i32_oct),
        Token::OctLiteral,
    )(input)
    .map_err(err_mapper(|| LexErrorKind::OctLiteral))
}

fn bin_literal_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
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
    .map_err(err_mapper(|| LexErrorKind::BinLiteral))
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        delimited(
            pair(space0, char('"')),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            char('"'),
        ),
        |i: LexInput| Token::StringLiteral(i.0),
    )(input)
    .map_err(err_mapper(|| LexErrorKind::StringLiteral))
}

fn identifier_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
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
            let slash_count = identifier.0.matches('\\').count();
            let question_count = identifier.0.matches('?').count();
            let macro_expansion_count = identifier.0.matches("\\?").count();

            if slash_count == question_count && question_count == macro_expansion_count {
                Ok(Token::Identifier(identifier.0))
            } else {
                Err(())
            }
        },
    )(input)
    .map_err(err_mapper(|| LexErrorKind::Identifier))
}

fn valid_identifier_start(input: LexInput) -> IResult<LexInput, &str> {
    map_res(peek(take(1_u32)), |first_char: LexInput| {
        if first_char.0.chars().nth(0).unwrap().is_ascii_alphabetic() {
            Ok(first_char.0)
        } else {
            Err(())
        }
    })(input)
}

fn include_directive_token(input: LexInput) -> IResult<LexInput, Token, LexErr> {
    map(
        preceded(
            space0,
            delimited(
                tuple((tag_no_case("include"), space1, char('"'))),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
                char('"'),
            ),
        ),
        |path: LexInput| Token::IncludeDirective(path.0),
    )(input)
    .map_err(err_mapper(|| LexErrorKind::IncludeDirective))
}

fn parse_i32_hex(input: LexInput) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.0, 16)
}

fn parse_i32_oct(input: LexInput) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.0, 8)
}

fn parse_i32_dec(input: LexInput) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.0, 10)
}

fn parse_i32_bin(input: LexInput) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.0, 2)
}

fn parse_u8_dec(input: char) -> Result<u8, std::num::ParseIntError> {
    let mut tmp = [0_u8; 1];
    u8::from_str_radix(char::encode_utf8(input, &mut tmp), 10)
}
