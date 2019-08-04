use nom::IResult;
use nom::combinator::{map, map_res, recognize, opt};
use nom::sequence::{preceded, tuple, pair, separated_pair};
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alphanumeric0, space0, hex_digit1, digit1};
use nom::branch::alt;
use nom::character::is_alphanumeric;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Token<'a> {
    Comment{text: &'a str},
    VariableDeclaration {name: &'a str, value: i32}
}

fn comment_token(input: &str) -> IResult<&str, Token> {
    map(preceded(tag(";"), alphanumeric0), |text| {
        Token::Comment{text}
    })(input)
}

fn variable_declaration_token(input: &str) -> IResult<&str, Token> {
    map(separated_pair(label_or_variable_name, tuple((space0, tag("="), space0)), i32_literal), |(name, value)| {
        Token::VariableDeclaration { name, value }
    })(input)
}

#[test]
fn test_variable_declaration_token() {
    assert_eq!(variable_declaration_token("foo = 1"), Ok(("", Token::VariableDeclaration{ name: "foo", value: 1})));
    assert_eq!(variable_declaration_token("foo_bar = %11111111"), Ok(("", Token::VariableDeclaration{ name: "foo_bar", value: 255})));
}

fn label_or_variable_name(input: &str) -> IResult<&str, &str> {
    take_while1(|chr: char| chr.is_ascii_alphanumeric() || chr == '_')(input)

}

fn i32_literal(input: &str) -> IResult<&str, i32> {
    alt((i32_hex_literal, i32_dec_literal, i32_bin_literal))(input)
}

fn i32_dec_literal(input: &str) -> IResult<&str, i32> {
    map_res(recognize(pair(opt(tag("-")), digit1)), parse_i32_dec)(input)
}

#[test]
fn test_i32_dec_literals() {
    assert_eq!(i32_dec_literal("1"), Ok(("", 1)));
    assert_eq!(i32_dec_literal("2"), Ok(("", 2)));
    assert_eq!(i32_dec_literal("255"), Ok(("", 255)));
    assert_eq!(i32_dec_literal("-1"), Ok(("", -1)));
}

fn i32_hex_literal(input: &str) -> IResult<&str, i32> {
    map_res(preceded(tag("$"), hex_digit1), parse_i32_hex)(input)
}

#[test]
fn test_i32_hex_literals() {
    assert_eq!(i32_hex_literal("$1"), Ok(("", 1)));
    assert_eq!(i32_hex_literal("$2"), Ok(("", 2)));
    assert_eq!(i32_hex_literal("$FF"), Ok(("", 255)));
    assert_eq!(i32_hex_literal("$FFFFFFFF"), Ok(("", -1)));
}

fn i32_bin_literal(input: &str) -> IResult<&str, i32> {
    map_res(preceded(tag("%"), take_while1(|chr| chr == '0' || chr == '1')), parse_i32_bin)(input)
}

#[test]
fn test_i32_bin_literals() {
    assert_eq!(i32_bin_literal("%1"), Ok(("", 1)));
    assert_eq!(i32_bin_literal("%10"), Ok(("", 2)));
    assert_eq!(i32_bin_literal("%11111111"), Ok(("", 255)));
    assert_eq!(i32_bin_literal("%11111111111111111111111111111111"), Ok(("", -1)));
}


fn parse_i32_hex(input: &str) -> Result<i32, std::num::ParseIntError> {
    u32::from_str_radix(input, 16).map(|val| val as i32)
}

fn parse_i32_bin(input: &str) -> Result<i32, std::num::ParseIntError> {
    u32::from_str_radix(input, 2).map(|val| val as i32)
}

fn parse_i32_dec(input: &str) -> Result<i32, std::num::ParseIntError> {
    i32::from_str_radix(input, 10)
}

