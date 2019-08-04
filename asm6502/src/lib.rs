#![allow(dead_code)]
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take, take_while1};
use nom::character::complete::{alphanumeric0, digit1, hex_digit1, space0, space1};
use nom::combinator::{map, map_res, opt, recognize};
use nom::sequence::{pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Token<'a> {
    Comment { text: &'a str },
    SymbolDecl { name: &'a str, value: i32 },
    MacroDecl { name: &'a str },
    MacroPositionalArg { pos: u8 },
    MacroInvocationCountArg,
}

fn macro_invocation_count_arg(input: &str) -> IResult<&str, Token> {
    map(preceded(tag("\\"), tag("?")), |_| {
        Token::MacroInvocationCountArg
    })(input)
}

#[test]
fn test_macro_invocation_count_arg() {
    assert_eq!(
        macro_invocation_count_arg("\\?"),
        Ok(("", Token::MacroInvocationCountArg))
    );
    assert_eq!(
        macro_invocation_count_arg("\\??"),
        Ok(("?", Token::MacroInvocationCountArg))
    );
}

fn macro_positional_arg(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(tag("\\"), take(1usize)), parse_u8_dec),
        |pos| Token::MacroPositionalArg { pos },
    )(input)
}

#[test]
fn test_macro_positional_arg() {
    assert_eq!(
        macro_positional_arg("\\0"),
        Ok(("", Token::MacroPositionalArg { pos: 0 }))
    );
    assert_eq!(
        macro_positional_arg("\\9"),
        Ok(("", Token::MacroPositionalArg { pos: 9 }))
    );
    assert_eq!(
        macro_positional_arg("\\91"),
        Ok(("1", Token::MacroPositionalArg { pos: 9 }))
    );
}

fn comment(input: &str) -> IResult<&str, Token> {
    map(preceded(tag(";"), alphanumeric0), |text| Token::Comment {
        text,
    })(input)
}

fn macro_declaration(input: &str) -> IResult<&str, Token> {
    map(
        terminated(
            terminated(label_or_symbol_name, space1),
            tag_no_case("macro"),
        ),
        |name| Token::MacroDecl { name },
    )(input)
}

#[test]
fn test_macro_declaration() {
    assert_eq!(
        macro_declaration("foo_bar macro"),
        Ok(("", Token::MacroDecl { name: "foo_bar" }))
    )
}

fn symbol_declaration(input: &str) -> IResult<&str, Token> {
    map(
        separated_pair(
            label_or_symbol_name,
            alt((
                tuple((space0, tag("="), space0)),
                tuple((space1, tag_no_case("equ"), space1)),
            )),
            i32_literal,
        ),
        |(name, value)| Token::SymbolDecl { name, value },
    )(input)
}

#[test]
fn test_symbol_declaration() {
    assert_eq!(
        symbol_declaration("foo = 1"),
        Ok((
            "",
            Token::SymbolDecl {
                name: "foo",
                value: 1
            }
        ))
    );
    assert_eq!(
        symbol_declaration("foo_bar = %11111111"),
        Ok((
            "",
            Token::SymbolDecl {
                name: "foo_bar",
                value: 255
            }
        ))
    );

    assert_eq!(
        symbol_declaration("baz equ $fe"),
        Ok((
            "",
            Token::SymbolDecl {
                name: "baz",
                value: 254
            }
        ))
    );
}

fn label_or_symbol_name(input: &str) -> IResult<&str, &str> {
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
    map_res(
        preceded(tag("%"), take_while1(|chr| chr == '0' || chr == '1')),
        parse_i32_bin,
    )(input)
}

#[test]
fn test_i32_bin_literals() {
    assert_eq!(i32_bin_literal("%1"), Ok(("", 1)));
    assert_eq!(i32_bin_literal("%10"), Ok(("", 2)));
    assert_eq!(i32_bin_literal("%11111111"), Ok(("", 255)));
    assert_eq!(
        i32_bin_literal("%11111111111111111111111111111111"),
        Ok(("", -1))
    );
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

fn parse_u8_dec(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 10)
}
