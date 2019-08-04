#![allow(dead_code)]
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take, take_while1};
use nom::character::complete::{alphanumeric0, digit1, hex_digit1, space1};
use nom::combinator::{map, map_res, opt, recognize};
use nom::sequence::{pair, preceded, terminated};
use nom::IResult;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BinaryOperator {
    Addition,
    Equals,
    NotEquals,
    Subtraction,
    RightShift,
    LeftShift,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Expression<'a> {
    Literal(Literal<'a>),
    Unary(UnaryOperator, Rc<Box<Expression<'a>>>),
    Binary(
        Rc<Box<Expression<'a>>>,
        BinaryOperator,
        Rc<Box<Expression<'a>>>,
    ),
    Grouping(Rc<Box<Expression<'a>>>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Literal<'a> {
    Str(&'a str),
    Num(i32),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Directive {
    NoOptimization,
    Org,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Token<'a> {
    Comment(&'a str),
    MacroDecl(&'a str),
    MacroPositionalArg(u8),
    MacroInvocationCountArg,
    Directive(Directive),
}

fn binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    map(
        alt((
            tag("+"),
            tag("="),
            tag("!="),
            tag("-"),
            tag(">>"),
            tag("<<"),
            tag("<="),
            tag("<"),
            tag(">="),
            tag(">"),
        )),
        |operator| match operator {
            "+" => BinaryOperator::Addition,
            "=" => BinaryOperator::Equals,
            "!=" => BinaryOperator::NotEquals,
            "-" => BinaryOperator::Subtraction,
            ">>" => BinaryOperator::RightShift,
            "<<" => BinaryOperator::LeftShift,
            "<=" => BinaryOperator::LessThanOrEqual,
            "<" => BinaryOperator::LessThan,
            ">=" => BinaryOperator::GreaterThanOrEqual,
            ">" => BinaryOperator::GreaterThan,
            _ => unreachable!(),
        },
    )(input)
}
fn org_directive(input: &str) -> IResult<&str, Directive> {
    map(tag_no_case("org"), |_| Directive::Org)(input)
}

#[test]
fn test_org_directive() {
    assert_eq!(org_directive("org $1234"), Ok((" $1234", Directive::Org)));
}

fn noopt_directive(input: &str) -> IResult<&str, Directive> {
    map(tag_no_case("noopt"), |_| Directive::NoOptimization)(input)
}

#[test]
fn test_noopt_directive() {
    assert_eq!(
        noopt_directive("noopt"),
        Ok(("", Directive::NoOptimization))
    )
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
        Token::MacroPositionalArg,
    )(input)
}

#[test]
fn test_macro_positional_arg() {
    assert_eq!(
        macro_positional_arg("\\0"),
        Ok(("", Token::MacroPositionalArg(0)))
    );
    assert_eq!(
        macro_positional_arg("\\9"),
        Ok(("", Token::MacroPositionalArg(9)))
    );
    assert_eq!(
        macro_positional_arg("\\91"),
        Ok(("1", Token::MacroPositionalArg(9)))
    );
}

fn comment(input: &str) -> IResult<&str, Token> {
    map(preceded(tag(";"), alphanumeric0), |text| {
        Token::Comment(text)
    })(input)
}

fn macro_declaration(input: &str) -> IResult<&str, Token> {
    map(
        terminated(
            terminated(label_symbol_macro_name, space1),
            tag_no_case("macro"),
        ),
        |name| Token::MacroDecl(name),
    )(input)
}

#[test]
fn test_macro_declaration() {
    assert_eq!(
        macro_declaration("foo_bar macro"),
        Ok(("", Token::MacroDecl("foo_bar")))
    )
}

fn label_symbol_macro_name(input: &str) -> IResult<&str, &str> {
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

fn u16_literal(input: &str) -> IResult<&str, u16> {
    alt((u16_hex_literal, u16_dec_literal, u16_bin_literal))(input)
}

fn u16_dec_literal(input: &str) -> IResult<&str, u16> {
    map_res(digit1, parse_u16_dec)(input)
}

#[test]
fn test_u16_dec_literals() {
    assert_eq!(u16_dec_literal("1"), Ok(("", 1)));
    assert_eq!(u16_dec_literal("2"), Ok(("", 2)));
    assert_eq!(u16_dec_literal("255"), Ok(("", 255)));
}

fn u16_hex_literal(input: &str) -> IResult<&str, u16> {
    map_res(preceded(tag("$"), hex_digit1), parse_u16_hex)(input)
}

#[test]
fn test_u16_hex_literals() {
    assert_eq!(u16_hex_literal("$1"), Ok(("", 1)));
    assert_eq!(u16_hex_literal("$2"), Ok(("", 2)));
    assert_eq!(u16_hex_literal("$FF"), Ok(("", 255)));
}

fn u16_bin_literal(input: &str) -> IResult<&str, u16> {
    map_res(
        preceded(tag("%"), take_while1(|chr| chr == '0' || chr == '1')),
        parse_u16_bin,
    )(input)
}

#[test]
fn test_u16_bin_literals() {
    assert_eq!(u16_bin_literal("%1"), Ok(("", 1)));
    assert_eq!(u16_bin_literal("%10"), Ok(("", 2)));
    assert_eq!(u16_bin_literal("%11111111"), Ok(("", 255)));
}
// TODO: abstract these

fn parse_u16_hex(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 16)
}

fn parse_u16_bin(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 2)
}

fn parse_u16_dec(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 10)
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
