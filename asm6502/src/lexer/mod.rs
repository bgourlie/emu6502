use crate::Token;
use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
    character::{
        complete::{char, digit1, hex_digit1, newline, oct_digit1, space0, space1},
        is_digit, is_space,
    },
    combinator::{map, map_res, not, peek},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use regex::Regex;
use shared6502::Op;
use std::{convert::TryFrom, iter::FusedIterator};

lazy_static! {
    static ref IDENT_REGEX: Regex = Regex::new("^[a-zA-Z_]+(:?[a-zA-Z0-9_]+|\\\\\\?)*$").unwrap();
}

#[derive(Debug, Default)]
pub struct Lexer<'a> {
    cur_line: u32,
    cur_column: u16,
    remaining: &'a str,
    remaining_len: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            cur_line: 1,
            cur_column: 1,
            remaining_len: input.chars().count(),
            remaining: input,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token<'a>, u32, u16, u16);

    fn next(&mut self) -> Option<Self::Item> {
        match lex(self.remaining) {
            Ok((remaining, (token, left_padding, right_padding))) => {
                let (token_start, token_end, token_line) = if let Token::Invalid(chars) = token {
                    // Invalid tokens may contain non-ascii characters, so we need to count chars
                    // instead of bytes here.
                    let consumed =
                        u16::try_from(self.remaining_len - chars.chars().count()).unwrap();
                    self.remaining_len -= usize::from(consumed);
                    let token_start = self.cur_column + left_padding;
                    let token_end = token_start + consumed - right_padding;
                    self.cur_column += consumed;
                    (token_start, token_end, self.cur_line)
                } else {
                    let consumed = u16::try_from(self.remaining_len - remaining.len()).unwrap();
                    self.remaining_len -= usize::from(consumed);
                    let token_start = self.cur_column + left_padding;
                    let token_end = token_start + consumed - right_padding;
                    let token_line = self.cur_line;

                    if let Token::Newline = token {
                        self.cur_line += 1;
                        self.cur_column = 1;
                    } else {
                        self.cur_column += consumed;
                    }
                    (token_start, token_end, token_line)
                };

                self.remaining = remaining;
                Some((token, token_line, token_start, token_end))
            }
            Err(nom::Err::Error(("", _))) => None,
            _ => unreachable!(),
        }
    }
}

impl<'a> FusedIterator for Lexer<'a> {}

fn lex(input: &str) -> IResult<&str, (Token, u16, u16)> {
    alt((
        alt((
            pl(space0, comment_token),
            pl(space0, error_directive_token),
            pb(
                space0,
                map(tag_no_case("equ"), |_| Token::EquDirective),
                space1,
            ),
            pl(space0, map(tag_no_case("noopt"), |_| Token::NoOptDirective)),
            pl(space1, map(tag_no_case("macro"), |_| Token::MacroStart)),
            pb(
                space0,
                map(tag_no_case("endm"), |_| Token::MacroEnd),
                space0,
            ),
            pl(space0, macro_positional_arg_token),
            pl(space0, map(tag("\\?"), |_| Token::MacroExpansionCount)),
            pl(space0, include_directive_token),
            pb(space0, map(tag_no_case("if"), |_| Token::IfStart), space1),
            pb(space0, map(tag_no_case("else"), |_| Token::Else), space0),
            pb(space0, map(tag_no_case("endif"), |_| Token::IfEnd), space0),
            pl(space0, mnemonic_token),
            pn(map(char('#'), |_| Token::ImmediatePrefix)),
            pb(
                space1,
                alt((
                    map(tag_no_case("db"), |_| Token::DbDirective),
                    map(tag_no_case("dw"), |_| Token::DwDirective),
                    map(tag_no_case("ds"), |_| Token::DsDirective),
                )),
                space1,
            ),
            pl(
                space0,
                alt((
                    offset_operand_token("x", |_| Token::OffsetByXOperand),
                    offset_operand_token("y", |_| Token::OffsetByYOperand),
                )),
            ),
            pl(space0, map(tag_no_case("end"), |_| Token::EndDirective)),
            pl(space0, identifier_token),
        )),
        alt((
            pl(
                space0,
                terminated(
                    alt((
                        dec_literal_token,
                        hex_literal_token,
                        oct_literal_token,
                        bin_literal_token,
                    )),
                    not(identifier_token),
                ),
            ),
            pb(
                space0,
                alt((
                    map(tag("="), |_| Token::EqualsOperator),
                    map(tag("!="), |_| Token::NotEqualsOperator),
                    map(tag("!"), |_| Token::BangOperator),
                    map(tag("~"), |_| Token::ComplementOperator),
                    map(tag("|"), |_| Token::OrOperator),
                    map(tag("^"), |_| Token::XorOperator),
                    map(tag("&"), |_| Token::AndOperator),
                    map(tag("+"), |_| Token::PlusOperator),
                    map(tag("-"), |_| Token::MinusOperator),
                    map(tag("*"), |_| Token::StarOperator),
                    map(tag("<<"), |_| Token::LeftShiftOperator),
                    map(tag(">>"), |_| Token::RightShiftOperator),
                    map(tag(">="), |_| Token::GreaterThanOrEqualToOperator),
                    map(tag("<="), |_| Token::LessThanOrEqualToOperator),
                    map(tag(">"), |_| Token::GreaterThanOperator),
                    map(tag("<"), |_| Token::LessThanOperator),
                )),
                space0,
            ),
            pn(map(char('('), |_| Token::OpenParen)),
            pn(map(char(')'), |_| Token::CloseParen)),
            pl(space0, string_literal_token),
            pn(character_literal_token),
            pn(map(tag(","), |_| Token::Comma)),
            pl(space0, map(newline, |_| Token::Newline)),
            pl(space0, invalid_token),
        )),
    ))(input)
}

/// Returns the token alongside the left and right padding in spaces. This method is for left-padded
/// tokens and unconditionally returns 0 right padding.
fn pl<'a, F, G>(
    space_parser: F,
    token_parser: G,
) -> impl Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16, u16)>
where
    F: Fn(&'a str) -> IResult<&'a str, &'a str>,
    G: Fn(&'a str) -> IResult<&'a str, Token<'a>>,
{
    map(
        pair(
            map(space_parser, |padding| {
                u16::try_from(padding.len()).unwrap()
            }),
            token_parser,
        ),
        |(padding_left, token)| (token, padding_left, 0),
    )
}

/// Returns the token alongside the left and right padding in spaces. This method is for tokens that
/// may have padding on both sides of it.
fn pb<'a, F, F2, G>(
    space_parser_left: F,
    token_parser: G,
    space_parser_right: F2,
) -> impl Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16, u16)>
where
    F: Fn(&'a str) -> IResult<&'a str, &'a str>,
    F2: Fn(&'a str) -> IResult<&'a str, &'a str>,
    G: Fn(&'a str) -> IResult<&'a str, Token<'a>>,
{
    map(
        tuple((
            map(space_parser_left, |padding| {
                u16::try_from(padding.len()).unwrap()
            }),
            token_parser,
            map(space_parser_right, |padding| {
                u16::try_from(padding.len()).unwrap()
            }),
        )),
        |(padding_left, token, padding_right)| (token, padding_left, padding_right),
    )
}

/// Returns the token alongside the left and right padding in spaces. This method is for tokens that
/// have no padding on either side, and unconditionally return zero for left and right padding.
fn pn<'a, G>(token_parser: G) -> impl Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16, u16)>
where
    G: Fn(&'a str) -> IResult<&'a str, Token<'a>>,
{
    map(token_parser, |token| (token, 0, 0))
}

fn invalid_token(input: &str) -> IResult<&str, Token> {
    map(
        take_while1(|chr: char| !chr.is_ascii() || !(is_space(chr as u8) || chr == '\n')),
        Token::Invalid,
    )(input)
}

fn comment_token(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            char(';'),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        Token::Comment,
    )(input)
}

fn error_directive_token(input: &str) -> IResult<&str, Token> {
    map(
        preceded(
            terminated(tag("ERROR ERROR ERROR"), space1),
            take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        Token::ErrorDirective,
    )(input)
}

fn offset_operand_token<'a, F>(
    register: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, Token<'a>>
where
    F: Fn(&'a str) -> Token<'a> + Copy,
{
    map(preceded(char(','), tag_no_case(register)), mapper)
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

fn mnemonic_implied<'a, F>(
    mnemonic: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, Op>
where
    F: Copy + Fn(&'a str) -> Op,
{
    map(
        terminated(tag_no_case(mnemonic), peek_comment_or_newline),
        mapper,
    )
}

fn mnemonic_operand<'a, F>(
    mnemonic: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, Op>
where
    F: Copy + Fn(&str) -> Op,
{
    map(terminated(tag_no_case(mnemonic), space1), mapper)
}

fn mnemonic_token(input: &str) -> IResult<&str, Token> {
    map(
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
        Token::Mnemonic,
    )(input)
}

fn peek_comment_or_newline(input: &str) -> IResult<&str, ()> {
    peek(alt((map(newline, |_| ()), map(comment_token, |_| ()))))(input)
}

fn macro_positional_arg_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(
            preceded(char('\\'), take_while1(|chr: char| is_digit(chr as u8))),
            |digits| u8::from_str_radix(digits, 10),
        ),
        Token::MacroPositionalArg,
    )(input)
}

fn hex_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(char('$'), hex_digit1), |val| {
            u16::from_str_radix(val, 16)
        }),
        Token::HexLiteral,
    )(input)
}

fn dec_literal_token(input: &str) -> IResult<&str, Token> {
    map(map_res(digit1, |val| u16::from_str_radix(val, 10)), |val| {
        Token::DecLiteral(val)
    })(input)
}

fn oct_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(preceded(char('@'), oct_digit1), |val| {
            u16::from_str_radix(val, 8)
        }),
        Token::OctLiteral,
    )(input)
}

fn bin_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        map_res(
            preceded(char('%'), take_while1(|chr| chr == '0' || chr == '1')),
            |val| u16::from_str_radix(val, 2),
        ),
        Token::BinLiteral,
    )(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal_token(input: &str) -> IResult<&str, Token> {
    map(
        delimited(
            char('"'),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            char('"'),
        ),
        Token::StringLiteral,
    )(input)
}

fn identifier_token(input: &str) -> IResult<&str, Token> {
    map_res(
        take_while1(|chr: char| {
            chr.is_ascii_alphanumeric() || chr == '_' || chr == '\\' || chr == '?'
        }),
        |identifier: &str| {
            if IDENT_REGEX.is_match(identifier) {
                Ok(Token::Identifier(identifier))
            } else {
                Err(())
            }
        },
    )(input)
}

fn include_directive_token(input: &str) -> IResult<&str, Token> {
    map(
        delimited(
            tuple((tag_no_case("include"), space1, char('"'))),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            char('"'),
        ),
        |path: &str| Token::IncludeDirective(path),
    )(input)
}
