#[cfg(test)]
mod tests;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
    character::complete::{char, digit1, hex_digit1, oct_digit1, one_of, space0, space1},
    combinator::{map, map_res, opt, peek},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use nom_locate::{position, LocatedSpan};
use shared6502::Op;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Copy, Clone, Debug, PartialEq)]
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
}

pub fn lex(input: Span) -> IResult<Span, Vec<(Span, Token)>> {
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
            if_end_token,
            mnemonic_token,
            immediate_prefix_token,
            define_directive_token("db", |(pos, _)| (pos, Token::DbDirective)),
            define_directive_token("dw", |(pos, _)| (pos, Token::DwDirective)),
            define_directive_token("ds", |(pos, _)| (pos, Token::DsDirective)),
            offset_operand_token("x", |(pos, _)| (pos, Token::OffsetByXOperand)),
            offset_operand_token("y", |(pos, _)| (pos, Token::OffsetByYOperand)),
            end_directive_token,
            identifier_token,
            operator_token("=", |(pos, _)| (pos, Token::EqualsOperator)),
        )),
        alt((
            dec_literal_token,
            hex_literal_token,
            oct_literal_token,
            bin_literal_token,
            operator_token("!=", |(pos, _)| (pos, Token::NotEqualsOperator)),
            operator_token("!", |(pos, _)| (pos, Token::BangOperator)),
            operator_token("~", |(pos, _)| (pos, Token::ComplementOperator)),
            operator_token("|", |(pos, _)| (pos, Token::OrOperator)),
            operator_token("^", |(pos, _)| (pos, Token::XorOperator)),
            operator_token("&", |(pos, _)| (pos, Token::AndOperator)),
            operator_token("+", |(pos, _)| (pos, Token::PlusOperator)),
            operator_token("-", |(pos, _)| (pos, Token::MinusOperator)),
            operator_token("*", |(pos, _)| (pos, Token::StarOperator)),
            operator_token("<<", |(pos, _)| (pos, Token::LeftShiftOperator)),
            operator_token(">>", |(pos, _)| (pos, Token::RightShiftOperator)),
            operator_token(">=", |(pos, _)| (pos, Token::GreaterThanOrEqualToOperator)),
            operator_token("<=", |(pos, _)| (pos, Token::LessThanOrEqualToOperator)),
            operator_token(">", |(pos, _)| (pos, Token::GreaterThanOperator)),
            operator_token("<", |(pos, _)| (pos, Token::LessThanOperator)),
            open_paren_token,
            close_paren_token,
        )),
        alt((
            string_literal_token,
            character_literal_token,
            comma_token,
            newline_token,
        )),
    )))(input)
}

fn newline(input: Span) -> IResult<Span, char> {
    preceded(
        space0,
        preceded(opt(char('\r')), nom::character::complete::newline),
    )(input)
}

fn newline_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(pair(position, newline), |(span, _)| (span, Token::Newline))(input)
}

fn end_directive_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, preceded(space0, tag_no_case("end"))),
        |(pos, _)| (pos, Token::EndDirective),
    )(input)
}

fn error_directive_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            preceded(
                delimited(space0, tag("ERROR ERROR ERROR"), space1),
                take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
            ),
        ),
        |(pos, msg): (Span, Span)| (pos, Token::ErrorDirective(msg.fragment)),
    )(input)
}

fn comma_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(pair(position, tag(",")), |(pos, _)| (pos, Token::Comma))(input)
}

fn offset_operand_token<'a, F>(
    register: &'static str,
    mapper: F,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, (Span<'a>, Token<'a>)>
where
    F: Fn((Span<'a>, Span<'a>)) -> (Span<'a>, Token<'a>),
{
    map(
        pair(
            position,
            preceded(
                space0,
                preceded(char(','), preceded(space0, tag_no_case(register))),
            ),
        ),
        mapper,
    )
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn character_literal_token(input: Span) -> IResult<Span, (Span, Token)> {
    map_res(
        pair(position, delimited(char('\''), take(1_usize), char('\''))),
        |(pos, chr): (Span, Span)| {
            let chr: char = chr.fragment.chars().nth(0_usize).unwrap();
            if chr.is_ascii() && !chr.is_ascii_control() {
                Ok((pos, Token::CharacterLiteral(chr)))
            } else {
                Err(())
            }
        },
    )(input)
}

fn immediate_prefix_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(pair(position, char('#')), |(pos, _)| {
        (pos, Token::ImmediatePrefix)
    })(input)
}

fn operator_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, (Span<'a>, Token<'a>)>
where
    F: Fn((Span<'a>, Span<'a>)) -> (Span<'a>, Token<'a>),
{
    map(
        pair(position, delimited(space0, tag(chars), space0)),
        mapper,
    )
}

fn mnemonic_implied<'a>(
    mnemonic: &'static str,
    op: Op,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Op> {
    map(
        terminated(tag_no_case(mnemonic), comment_or_newline),
        move |_| op,
    )
}

fn mnemonic_operand<'a>(
    mnemonic: &'static str,
    op: Op,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Op> {
    map(terminated(tag_no_case(mnemonic), space1), move |_| op)
}

fn mnemonic_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
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
        ),
        |(pos, op)| (pos, Token::Mnemonic(op)),
    )(input)
}

fn if_start_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, delimited(space0, tag_no_case("if"), space1)),
        |(pos, _)| (pos, Token::IfStart),
    )(input)
}

fn comment_or_newline(input: Span) -> IResult<Span, ()> {
    peek(alt((map(newline, |_| ()), map(comment_token, |_| ()))))(input)
}

fn if_end_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, delimited(space0, tag_no_case("endif"), space0)),
        |(pos, _)| (pos, Token::IfEnd),
    )(input)
}

fn macro_start_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, preceded(space1, tag_no_case("macro"))),
        |(pos, _)| (pos, Token::MacroStart),
    )(input)
}

fn macro_end_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, delimited(space0, tag_no_case("endm"), space0)),
        |(pos, _)| (pos, Token::MacroEnd),
    )(input)
}

fn macro_invocation_count_arg_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(pair(position, preceded(space0, tag("\\?"))), |(pos, _)| {
        (pos, Token::MacroExpansionCount)
    })(input)
}

fn macro_positional_arg_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            map_res(
                preceded(space0, preceded(char('\\'), one_of("123456789"))),
                parse_u8_dec,
            ),
        ),
        |(pos, arg)| (pos, Token::MacroPositionalArg(arg)),
    )(input)
}

fn noopt_directive_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, preceded(space0, tag_no_case("noopt"))),
        |(pos, _)| (pos, Token::NoOptDirective),
    )(input)
}

fn open_paren_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(pair(position, char('(')), |(pos, _)| {
        (pos, Token::OpenParen)
    })(input)
}

fn close_paren_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(pair(position, char(')')), |(pos, _)| {
        (pos, Token::CloseParen)
    })(input)
}

fn equ_directive_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, delimited(space0, tag_no_case("equ"), space1)),
        |(pos, _)| (pos, Token::EquDirective),
    )(input)
}

fn define_directive_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, (Span<'a>, Token<'a>)>
where
    F: Fn((Span<'a>, Span<'a>)) -> (Span<'a>, Token<'a>),
{
    map(
        pair(position, delimited(space1, tag_no_case(chars), space1)),
        mapper,
    )
}

fn hex_literal_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            map_res(preceded(pair(space0, char('$')), hex_digit1), parse_i32_hex),
        ),
        |(pos, val)| (pos, Token::HexLiteral(val)),
    )(input)
}

fn dec_literal_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(position, map_res(preceded(space0, digit1), parse_i32_dec)),
        |(pos, val)| (pos, Token::DecLiteral(val)),
    )(input)
}

fn oct_literal_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            map_res(preceded(pair(space0, char('@')), oct_digit1), parse_i32_oct),
        ),
        |(pos, val)| (pos, Token::OctLiteral(val)),
    )(input)
}

fn bin_literal_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            map_res(
                preceded(
                    pair(space0, char('%')),
                    take_while1(|chr| chr == '0' || chr == '1'),
                ),
                parse_i32_bin,
            ),
        ),
        |(pos, val)| (pos, Token::BinLiteral(val)),
    )(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            delimited(
                pair(space0, char('"')),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
                char('"'),
            ),
        ),
        |(pos, string): (Span, Span)| (pos, Token::StringLiteral(string.fragment)),
    )(input)
}

fn identifier_token(input: Span) -> IResult<Span, (Span, Token)> {
    map_res(
        pair(
            position,
            preceded(
                space0,
                preceded(
                    valid_identifier_start,
                    take_while1(|chr: char| {
                        chr.is_ascii_alphanumeric() || chr == '_' || chr == '\\' || chr == '?'
                    }),
                ),
            ),
        ),
        |(pos, identifier): (_, Span)| {
            // '/' and '?' are only valid when used together ("/?"), and we validate that here
            let slash_count = identifier.fragment.matches('\\').count();
            let question_count = identifier.fragment.matches('?').count();
            let macro_expansion_count = identifier.fragment.matches("\\?").count();

            if slash_count == question_count && question_count == macro_expansion_count {
                Ok((pos, Token::Identifier(identifier.fragment)))
            } else {
                Err(())
            }
        },
    )(input)
}

fn valid_identifier_start(input: Span) -> IResult<Span, Span> {
    map_res(peek(take(1_u32)), |first_char: Span| {
        if first_char
            .fragment
            .chars()
            .nth(0)
            .unwrap()
            .is_ascii_alphabetic()
        {
            Ok(first_char)
        } else {
            Err(())
        }
    })(input)
}

fn include_directive_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            preceded(
                space0,
                delimited(
                    tuple((tag_no_case("include"), space1, char('"'))),
                    take_while(|chr: char| {
                        chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'
                    }),
                    char('"'),
                ),
            ),
        ),
        |(pos, path): (Span, Span)| (pos, Token::IncludeDirective(path.fragment)),
    )(input)
}

fn comment_token(input: Span) -> IResult<Span, (Span, Token)> {
    map(
        pair(
            position,
            preceded(
                space0,
                preceded(
                    char(';'),
                    take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
                ),
            ),
        ),
        |(pos, comment): (Span, Span)| (pos, Token::Comment(comment.fragment)),
    )(input)
}

fn parse_i32_hex(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.fragment, 16)
}

fn parse_i32_oct(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.fragment, 8)
}

fn parse_i32_dec(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.fragment, 10)
}

fn parse_i32_bin(input: Span) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input.fragment, 2)
}

fn parse_u8_dec(input: char) -> Result<u8, std::num::ParseIntError> {
    let mut tmp = [0_u8; 1];
    u8::from_str_radix(char::encode_utf8(input, &mut tmp), 10)
}
