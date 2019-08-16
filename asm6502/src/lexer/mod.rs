#[cfg(test)]
mod tests;

use {
    nom::{
        branch::alt,
        bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
        character::complete::{char, digit1, hex_digit1, oct_digit1, one_of, space0, space1},
        combinator::{map, map_res, opt, peek, recognize},
        multi::many0,
        sequence::{delimited, pair, preceded, terminated},
        IResult,
    },
    nom_locate::{position, LocatedSpan},
    shared6502::Op,
};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Newline,
    Comment(Span<'a>, &'a str),
    Identifier(Span<'a>, &'a str),
    CharacterLiteral(Span<'a>, char),
    StringLiteral(Span<'a>, &'a str),
    HexLiteral(Span<'a>, i32),
    DecLiteral(Span<'a>, i32),
    BinLiteral(Span<'a>, i32),
    OctLiteral(Span<'a>, i32),
    NoOptDirective(Span<'a>),
    EquDirective(Span<'a>),
    SubExprStart(Span<'a>),
    SubExprEnd(Span<'a>),
    EqualsOperator(Span<'a>),
    NotEqualsOperator(Span<'a>),
    PlusOperator(Span<'a>),
    MinusOperator(Span<'a>),
    StarOperator(Span<'a>),
    RightShiftOperator(Span<'a>),
    LeftShiftOperator(Span<'a>),
    GreaterThanOperator(Span<'a>),
    GreaterThanOrEqualToOperator(Span<'a>),
    LessThanOperator(Span<'a>),
    LessThanOrEqualToOperator(Span<'a>),
    ComplementOperator(Span<'a>),
    AndOperator(Span<'a>),
    OrOperator(Span<'a>),
    XorOperator(Span<'a>),
    MacroStart(Span<'a>),
    MacroPositionalArg(Span<'a>, u8),
    MacroInvokeCountArg(Span<'a>),
    MacroEnd(Span<'a>),
    IfStart(Span<'a>),
    IfEnd(Span<'a>),
    Mnemonic(Span<'a>, Op),
    ImmediatePrefix(Span<'a>),
    OffsetByXOperand(Span<'a>),
    OffsetByYOperand(Span<'a>),
    Comma(Span<'a>),
    ErrorDirective(Span<'a>, &'a str),
    EndDirective(Span<'a>),
}

pub fn parse(input: Span) -> IResult<Span, Vec<Token>> {
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
            if_start_token,
            if_end_token,
            mnemonic_token,
            immediate_prefix_token,
            offset_operand_token(",x", |(pos, _)| Token::OffsetByXOperand(pos)),
            offset_operand_token(",y", |(pos, _)| Token::OffsetByYOperand(pos)),
            end_directive_token,
            identifier_token,
            operator_token("=", |(pos, _)| Token::EqualsOperator(pos)),
            operator_token("!=", |(pos, _)| Token::NotEqualsOperator(pos)),
            operator_token("~", |(pos, _)| Token::ComplementOperator(pos)),
            operator_token("|", |(pos, _)| Token::OrOperator(pos)),
            operator_token("^", |(pos, _)| Token::XorOperator(pos)),
        )),
        alt((
            operator_token("&", |(pos, _)| Token::AndOperator(pos)),
            operator_token("+", |(pos, _)| Token::PlusOperator(pos)),
            operator_token("-", |(pos, _)| Token::MinusOperator(pos)),
            operator_token("*", |(pos, _)| Token::StarOperator(pos)),
            operator_token("<<", |(pos, _)| Token::LeftShiftOperator(pos)),
            operator_token(">>", |(pos, _)| Token::RightShiftOperator(pos)),
            operator_token(">=", |(pos, _)| Token::GreaterThanOrEqualToOperator(pos)),
            operator_token("<=", |(pos, _)| Token::LessThanOrEqualToOperator(pos)),
            operator_token(">", |(pos, _)| Token::GreaterThanOperator(pos)),
            operator_token("<", |(pos, _)| Token::LessThanOperator(pos)),
            sub_expr_start_token,
            sub_expr_end_token,
            dec_literal_token,
            hex_literal_token,
            oct_literal_token,
            bin_literal_token,
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

fn newline_token(input: Span) -> IResult<Span, Token> {
    map(newline, |_| Token::Newline)(input)
}

fn end_directive_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, preceded(space0, tag_no_case("end"))),
        |(pos, _)| Token::EndDirective(pos),
    )(input)
}

fn error_directive_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            preceded(
                delimited(space0, tag("ERROR ERROR ERROR"), space1),
                take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
            ),
        ),
        |(pos, msg)| Token::ErrorDirective(pos, msg.fragment),
    )(input)
}

fn comma_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, tag(",")), |(pos, _)| Token::Comma(pos))(input)
}

fn offset_operand_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token<'a>>
where
    F: Fn((Span<'a>, Span<'a>)) -> Token<'a>,
{
    map(pair(position, tag_no_case(chars)), mapper)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn character_literal_token(input: Span) -> IResult<Span, Token> {
    map_res(
        pair(position, delimited(char('\''), take(1_usize), char('\''))),
        |(pos, chr): (Span, Span)| {
            let chr: char = chr.fragment.chars().nth(0_usize).unwrap();
            if chr.is_ascii() && !chr.is_ascii_control() {
                Ok(Token::CharacterLiteral(pos, chr))
            } else {
                Err(())
            }
        },
    )(input)
}

fn immediate_prefix_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, char('#')), |(pos, _)| {
        Token::ImmediatePrefix(pos)
    })(input)
}

fn operator_token<'a, F>(
    chars: &'static str,
    mapper: F,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token<'a>>
where
    F: Fn((Span<'a>, Span<'a>)) -> Token<'a>,
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

fn mnemonic_token(input: Span) -> IResult<Span, Token> {
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
        |(pos, op)| Token::Mnemonic(pos, op),
    )(input)
}

fn if_start_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag_no_case("if"), space1)),
        |(pos, _)| Token::IfStart(pos),
    )(input)
}

fn comment_or_newline(input: Span) -> IResult<Span, ()> {
    peek(alt((map(newline, |_| ()), map(comment_token, |_| ()))))(input)
}

fn if_end_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag_no_case("endif"), space0)),
        |(pos, _)| Token::IfEnd(pos),
    )(input)
}

fn macro_start_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, preceded(space1, tag_no_case("macro"))),
        |(pos, _)| Token::MacroStart(pos),
    )(input)
}

fn macro_end_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag_no_case("endm"), space0)),
        |(pos, _)| Token::MacroEnd(pos),
    )(input)
}

fn macro_invocation_count_arg_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, tag("\\?")), |(pos, _)| {
        Token::MacroInvokeCountArg(pos)
    })(input)
}

fn macro_positional_arg_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            map_res(preceded(char('\\'), one_of("123456789")), parse_u8_dec),
        ),
        |(pos, arg)| Token::MacroPositionalArg(pos, arg),
    )(input)
}

fn noopt_directive_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, preceded(space0, tag_no_case("noopt"))),
        |(pos, _)| Token::NoOptDirective(pos),
    )(input)
}

fn sub_expr_start_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, char('(')), |(pos, _)| {
        Token::SubExprStart(pos)
    })(input)
}

fn sub_expr_end_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, char(')')), |(pos, _)| Token::SubExprEnd(pos))(input)
}

fn equ_directive_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag_no_case("equ"), space1)),
        |(pos, _)| Token::EquDirective(pos),
    )(input)
}

fn hex_literal_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            map_res(preceded(char('$'), hex_digit1), parse_i32_hex),
        ),
        |(pos, val)| Token::HexLiteral(pos, val),
    )(input)
}

fn dec_literal_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            map_res(recognize(preceded(opt(char('-')), digit1)), parse_i32_dec),
        ),
        |(pos, val)| Token::DecLiteral(pos, val),
    )(input)
}

fn oct_literal_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            map_res(preceded(char('@'), oct_digit1), parse_i32_oct),
        ),
        |(pos, val)| Token::OctLiteral(pos, val),
    )(input)
}

fn bin_literal_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            map_res(
                preceded(char('%'), take_while1(|chr| chr == '0' || chr == '1')),
                parse_i32_bin,
            ),
        ),
        |(pos, val)| Token::BinLiteral(pos, val),
    )(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            delimited(
                char('"'),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
                char('"'),
            ),
        ),
        |(pos, string)| Token::StringLiteral(pos, string.fragment),
    )(input)
}

fn identifier_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            delimited(
                space0,
                preceded(
                    valid_identifier_start,
                    take_while1(|chr: char| chr.is_ascii_alphanumeric() || chr == '_'),
                ),
                space0,
            ),
        ),
        |(pos, identifier)| Token::Identifier(pos, identifier.fragment),
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

fn comment_token(input: Span) -> IResult<Span, Token> {
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
        |(pos, comment)| Token::Comment(pos, comment.fragment),
    )(input)
}

fn parse_i32_hex(input: Span) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input.fragment, 16)
}

fn parse_i32_oct(input: Span) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input.fragment, 8)
}

fn parse_i32_dec(input: Span) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input.fragment, 10)
}

fn parse_i32_bin(input: Span) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input.fragment, 2)
}

fn parse_u8_dec(input: char) -> Result<u8, std::num::ParseIntError> {
    let mut tmp = [0_u8; 1];
    u8::from_str_radix(char::encode_utf8(input, &mut tmp), 10)
}
