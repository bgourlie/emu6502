use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while1};
use nom::character::complete::{alphanumeric0, digit1, hex_digit1, space0, space1};
use nom::character::is_alphanumeric;
use nom::combinator::{map, map_res, opt, recognize};
use nom::sequence::{pair, preceded, separated_pair, tuple};
use nom::IResult;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Token<'a> {
    Comment { text: &'a str },
    SymbolDeclaration { name: &'a str, value: i32 },
}

fn comment(input: &str) -> IResult<&str, Token> {
    map(preceded(tag(";"), alphanumeric0), |text| Token::Comment {
        text,
    })(input)
}

fn symbol_declaration(input: &str) -> IResult<&str, Token> {
    map(
        separated_pair(
            label_or_variable_name,
            alt((
                tuple((space0, tag("="), space0)),
                tuple((space1, tag_no_case("equ"), space1)),
            )),
            i32_literal,
        ),
        |(name, value)| Token::SymbolDeclaration { name, value },
    )(input)
}

#[test]
fn test_symbol_declaration() {
    assert_eq!(
        symbol_declaration("foo = 1"),
        Ok((
            "",
            Token::SymbolDeclaration {
                name: "foo",
                value: 1
            }
        ))
    );
    assert_eq!(
        symbol_declaration("foo_bar = %11111111"),
        Ok((
            "",
            Token::SymbolDeclaration {
                name: "foo_bar",
                value: 255
            }
        ))
    );

    assert_eq!(
        symbol_declaration("baz equ $fe"),
        Ok((
            "",
            Token::SymbolDeclaration {
                name: "baz",
                value: 254
            }
        ))
    );
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
