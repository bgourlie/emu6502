use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while, take_while1};
use nom::character::complete::{char, digit1, hex_digit1, oct_digit1, one_of, space1};
use nom::combinator::{map, map_res, opt, recognize};
use nom::sequence::{delimited, preceded, terminated};
use nom::IResult;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Token<'a> {
    Comment(&'a str),
    Identifier(&'a str),
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
        take_while1(|chr: char| chr.is_ascii_alphanumeric() || chr == '_'),
        |identifier| Token::Identifier(identifier),
    )(input)
}

#[test]
fn test_identifier() {
    assert_eq!(
        identifier("some_thing123 abc"),
        Ok((" abc", Token::Identifier("some_thing123")))
    )
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
