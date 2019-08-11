use {
    nom::{
        branch::alt,
        bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
        character::complete::{char, digit1, hex_digit1, newline, oct_digit1, one_of, space1},
        combinator::{map, map_res, opt, peek, recognize},
        error::ErrorKind::MapRes,
        multi::many_till,
        sequence::{delimited, preceded, terminated},
        Err, IResult,
    },
    shared6502::Op,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Token<'a> {
    Comment(&'a str),
    Identifier(&'a str),
    CharacterLiteral(char),
    StringLiteral(&'a str),
    HexLiteral(i32),
    DecLiteral(i32),
    BinLiteral(i32),
    OctLiteral(i32),
    NoOptDirective,
    EquDirective,
    SubExprStart,
    SubExprEnd,
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
    MacroStart,
    MacroPositionalArg(u8),
    MacroInvokeCountArg,
    MacroEnd,
    IfStart,
    IfEnd,
    Mnemonic(Op),
    ImmediatePrefix,
    OffsetByXOperand,
    OffsetByYOperand,
    Comma,
}

fn line(input: &str) -> IResult<&str, Option<Vec<Token>>> {
    opt(map(
        many_till(
            alt((
                alt((
                    comment,
                    equ_directive,
                    noopt_directive,
                    macro_start,
                    macro_end,
                    macro_positional_arg,
                    macro_invocation_count_arg,
                    if_start,
                    if_end,
                    mnemonic,
                    immediate_prefix,
                    offset_by_x_operand,
                    offset_by_y_operand,
                    identifier,
                    complement_operator,
                    or_operator,
                    and_operator,
                    plus_operator,
                    star_operator,
                    left_shift_operator,
                )),
                alt((
                    right_shift_operator,
                    greater_than_or_equal_operator,
                    less_than_or_equal_operator,
                    greater_than_operator,
                    less_than_operator,
                    sub_expr_start,
                    sub_expr_end,
                    dec_literal,
                    hex_literal,
                    oct_literal,
                    bin_literal,
                    string_literal,
                    character_literal,
                    comma,
                )),
            )),
            newline,
        ),
        |(tokens, _)| tokens,
    ))(input)
}

fn comma(input: &str) -> IResult<&str, Token> {
    map(tag(","), |_| Token::Comma)(input)
}

fn offset_by_x_operand(input: &str) -> IResult<&str, Token> {
    map(tag_no_case(",x"), |_| Token::OffsetByXOperand)(input)
}

fn offset_by_y_operand(input: &str) -> IResult<&str, Token> {
    map(tag_no_case(",y"), |_| Token::OffsetByYOperand)(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn character_literal(input: &str) -> IResult<&str, Token> {
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

#[test]
fn test_character_literal() {
    assert_eq!(
        character_literal("'a'"),
        Ok(("", Token::CharacterLiteral('a')))
    );
    assert_eq!(character_literal("'\n'"), Err(Err::Error(("'\n'", MapRes))));
}

fn immediate_prefix(input: &str) -> IResult<&str, Token> {
    map(preceded(space1, char('#')), |_| Token::ImmediatePrefix)(input)
}

fn mnemonic(input: &str) -> IResult<&str, Token> {
    map(
        alt((
            alt((
                map(tag_no_case("adc"), |_| Op::Adc),
                map(tag_no_case("and"), |_| Op::And),
                map(tag_no_case("asl"), |_| Op::Asl),
                map(tag_no_case("bcc"), |_| Op::Bcc),
                map(tag_no_case("bcs"), |_| Op::Bcs),
                map(tag_no_case("beq"), |_| Op::Beq),
                map(tag_no_case("bit"), |_| Op::Bit),
                map(tag_no_case("bmi"), |_| Op::Bmi),
                map(tag_no_case("bne"), |_| Op::Bne),
                map(tag_no_case("bpl"), |_| Op::Bpl),
                map(tag_no_case("brk"), |_| Op::Brk),
                map(tag_no_case("bvc"), |_| Op::Bvc),
                map(tag_no_case("bvs"), |_| Op::Bvs),
                map(tag_no_case("clc"), |_| Op::Clc),
                map(tag_no_case("cld"), |_| Op::Cld),
                map(tag_no_case("cli"), |_| Op::Cli),
                map(tag_no_case("clv"), |_| Op::Clv),
                map(tag_no_case("cmp"), |_| Op::Cmp),
                map(tag_no_case("cpx"), |_| Op::Cpx),
                map(tag_no_case("cpy"), |_| Op::Cpy),
                map(tag_no_case("dec"), |_| Op::Dec),
            )),
            alt((
                map(tag_no_case("dex"), |_| Op::Dex),
                map(tag_no_case("dey"), |_| Op::Dey),
                map(tag_no_case("eor"), |_| Op::Eor),
                map(tag_no_case("inc"), |_| Op::Inc),
                map(tag_no_case("inx"), |_| Op::Inx),
                map(tag_no_case("iny"), |_| Op::Iny),
                map(tag_no_case("jmp"), |_| Op::Jmp),
                map(tag_no_case("jsr"), |_| Op::Jsr),
                map(tag_no_case("lda"), |_| Op::Lda),
                map(tag_no_case("ldx"), |_| Op::Ldx),
                map(tag_no_case("ldy"), |_| Op::Ldy),
                map(tag_no_case("lsr"), |_| Op::Lsr),
                map(tag_no_case("nop"), |_| Op::Nop),
                map(tag_no_case("ora"), |_| Op::Ora),
                map(tag_no_case("pha"), |_| Op::Pha),
                map(tag_no_case("php"), |_| Op::Php),
                map(tag_no_case("pla"), |_| Op::Pla),
                map(tag_no_case("plp"), |_| Op::Plp),
                map(tag_no_case("rol"), |_| Op::Rol),
                map(tag_no_case("ror"), |_| Op::Ror),
                map(tag_no_case("rti"), |_| Op::Rti),
            )),
            alt((
                map(tag_no_case("rts"), |_| Op::Rts),
                map(tag_no_case("sbc"), |_| Op::Sbc),
                map(tag_no_case("sec"), |_| Op::Sec),
                map(tag_no_case("sed"), |_| Op::Sed),
                map(tag_no_case("sei"), |_| Op::Sei),
                map(tag_no_case("sta"), |_| Op::Sta),
                map(tag_no_case("stx"), |_| Op::Stx),
                map(tag_no_case("sty"), |_| Op::Sty),
                map(tag_no_case("tax"), |_| Op::Tax),
                map(tag_no_case("tay"), |_| Op::Tay),
                map(tag_no_case("tsx"), |_| Op::Tsx),
                map(tag_no_case("txa"), |_| Op::Txa),
                map(tag_no_case("txs"), |_| Op::Txs),
                map(tag_no_case("tya"), |_| Op::Tya),
            )),
        )),
        |op| Token::Mnemonic(op),
    )(input)
}

fn if_start(input: &str) -> IResult<&str, Token> {
    map(terminated(tag_no_case("if"), space1), |_| Token::IfStart)(input)
}

#[test]
fn test_if_start() {
    assert_eq!(if_start("if "), Ok(("", Token::IfStart)));
}

fn if_end(input: &str) -> IResult<&str, Token> {
    map(tag_no_case("endif"), |_| Token::IfEnd)(input)
}

fn macro_start(input: &str) -> IResult<&str, Token> {
    map(preceded(space1, tag_no_case("macro")), |_| {
        Token::MacroStart
    })(input)
}

fn macro_end(input: &str) -> IResult<&str, Token> {
    map(tag_no_case("endm"), |_| Token::MacroEnd)(input)
}

fn macro_invocation_count_arg(input: &str) -> IResult<&str, Token> {
    map(tag("\\?"), |_| Token::MacroInvokeCountArg)(input)
}

fn macro_positional_arg(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(char('\\'), one_of("123456789")), parse_u8_dec),
        |arg| Token::MacroPositionalArg(arg),
    )(input)
}

#[test]
fn test_macro_arg() {
    assert_eq!(
        macro_positional_arg("\\1"),
        Ok(("", Token::MacroPositionalArg(1)))
    )
}

fn complement_operator(input: &str) -> IResult<&str, Token> {
    map(char('~'), |_| Token::ComplementOperator)(input)
}

fn or_operator(input: &str) -> IResult<&str, Token> {
    map(char('|'), |_| Token::OrOperator)(input)
}

fn and_operator(input: &str) -> IResult<&str, Token> {
    map(char('&'), |_| Token::AndOperator)(input)
}

fn plus_operator(input: &str) -> IResult<&str, Token> {
    map(char('+'), |_| Token::PlusOperator)(input)
}

fn star_operator(input: &str) -> IResult<&str, Token> {
    map(char('*'), |_| Token::StarOperator)(input)
}

fn less_than_operator(input: &str) -> IResult<&str, Token> {
    map(char('<'), |_| Token::LessThanOperator)(input)
}

fn greater_than_operator(input: &str) -> IResult<&str, Token> {
    map(char('>'), |_| Token::GreaterThanOperator)(input)
}

fn less_than_or_equal_operator(input: &str) -> IResult<&str, Token> {
    map(tag("<="), |_| Token::LessThanOrEqualToOperator)(input)
}

fn greater_than_or_equal_operator(input: &str) -> IResult<&str, Token> {
    map(tag(">="), |_| Token::GreaterThanOrEqualToOperator)(input)
}

fn right_shift_operator(input: &str) -> IResult<&str, Token> {
    map(tag(">>"), |_| Token::RightShiftOperator)(input)
}

fn left_shift_operator(input: &str) -> IResult<&str, Token> {
    map(tag("<<"), |_| Token::LeftShiftOperator)(input)
}

fn noopt_directive(input: &str) -> IResult<&str, Token> {
    map(tag_no_case("noopt"), |_| Token::NoOptDirective)(input)
}
fn sub_expr_start(input: &str) -> IResult<&str, Token> {
    map(char('('), |_| Token::SubExprStart)(input)
}

fn sub_expr_end(input: &str) -> IResult<&str, Token> {
    map(char(')'), |_| Token::SubExprEnd)(input)
}

fn equ_directive(input: &str) -> IResult<&str, Token> {
    map(tag_no_case("equ"), |_| Token::EquDirective)(input)
}

fn hex_literal(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(char('$'), hex_digit1), parse_i32_hex),
        |val| Token::HexLiteral(val),
    )(input)
}

#[test]
fn test_hex_literal() {
    assert_eq!(hex_literal("$1"), Ok(("", Token::HexLiteral(1))));
    assert_eq!(hex_literal("$ff"), Ok(("", Token::HexLiteral(255))));
}
fn dec_literal(input: &str) -> IResult<&str, Token> {
    map(
        map_res(recognize(preceded(opt(char('-')), digit1)), parse_i32_dec),
        |val| Token::DecLiteral(val),
    )(input)
}

#[test]
fn test_dec_literal() {
    assert_eq!(dec_literal("-234"), Ok(("", Token::DecLiteral(-234))));
    assert_eq!(dec_literal("8234"), Ok(("", Token::DecLiteral(8234))));
}

fn oct_literal(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(char('@'), oct_digit1), parse_i32_oct),
        |val| Token::OctLiteral(val),
    )(input)
}

#[test]
fn test_oct_literal() {
    assert_eq!(oct_literal("@1"), Ok(("", Token::OctLiteral(1))));
    assert_eq!(oct_literal("@10"), Ok(("", Token::OctLiteral(8))));
}

fn bin_literal(input: &str) -> IResult<&str, Token> {
    map(
        map_res(
            preceded(char('%'), take_while1(|chr| chr == '0' || chr == '1')),
            parse_i32_bin,
        ),
        |val| Token::BinLiteral(val),
    )(input)
}

#[test]
fn test_bin_literal() {
    assert_eq!(bin_literal("%00000001"), Ok(("", Token::BinLiteral(1))));
    assert_eq!(bin_literal("%11111111"), Ok(("", Token::BinLiteral(255))));
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal(input: &str) -> IResult<&str, Token> {
    map(
        delimited(
            char('"'),
            take_while(|chr| chr != '"' && is_valid_ascii(chr)),
            char('"'),
        ),
        |string| Token::StringLiteral(string),
    )(input)
}

#[test]
fn test_string_literal() {
    assert_eq!(
        string_literal("\"Why ~hello~ there!\" abc"),
        Ok((" abc", Token::StringLiteral("Why ~hello~ there!")))
    );
}

fn identifier(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            valid_identifier_start,
            take_while1(|chr: char| chr.is_ascii_alphanumeric() || chr == '_'),
        ),
        |identifier| Token::Identifier(identifier),
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

#[test]
fn test_identifier() {
    assert_eq!(
        identifier("some_thing123 abc"),
        Ok((" abc", Token::Identifier("some_thing123")))
    );

    assert_eq!(
        identifier("123some_thing abc"),
        Err(Err::Error(("123some_thing abc", MapRes)))
    );
}

fn comment(input: &str) -> IResult<&str, Token> {
    map(preceded(tag(";"), take_while(is_valid_ascii)), |comment| {
        Token::Comment(comment)
    })(input)
}

#[test]
fn test_comment() {
    assert_eq!(comment(";"), Ok(("", Token::Comment(""))));
    assert_eq!(
        comment("; hello world!"),
        Ok(("", Token::Comment(" hello world!")))
    );
}

fn is_valid_ascii(chr: char) -> bool {
    chr >= ' ' && chr <= '~'
}

fn parse_i32_hex(input: &str) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input, 16)
}

fn parse_i32_oct(input: &str) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input, 8)
}

fn parse_i32_dec(input: &str) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input, 10)
}

fn parse_i32_bin(input: &str) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input, 2)
}

fn parse_u8_dec(input: char) -> Result<u8, std::num::ParseIntError> {
    let mut tmp = [0_u8; 1];
    u8::from_str_radix(char::encode_utf8(input, &mut tmp), 10)
}
