use crate::{PositionedToken, Token};
use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
    character::{
        complete::{char, digit1, hex_digit1, newline, oct_digit1, space0, space1},
        is_digit, is_space,
    },
    combinator::{map, map_res, not, peek},
    sequence::{delimited, pair, preceded, terminated},
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
    type Item = PositionedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match lex(self.remaining) {
            Ok((remaining, (token, token_length, left_padding))) => {
                let (token_start, token_end, token_line) = {
                    let consumed = left_padding + token_length;
                    self.remaining_len -= usize::from(consumed);
                    let token_start = self.cur_column + left_padding;
                    let token_end = token_start + consumed - left_padding;
                    let token_line = self.cur_line;

                    if token == Token::Newline {
                        self.cur_line += 1;
                        self.cur_column = 1;
                    } else {
                        self.cur_column += consumed;
                    }
                    (token_start, token_end, token_line)
                };

                self.remaining = remaining;
                Some(PositionedToken {
                    token,
                    line: token_line,
                    span: (token_start, token_end),
                })
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
            pl(
                space1,
                terminated(
                    map(tag_no_case("equ"), |_| (Token::EquDirective, 3)),
                    peek(space1),
                ),
            ),
            pl(
                space0,
                map(tag_no_case("noopt"), |_| (Token::NoOptDirective, 5)),
            ),
            pl(
                space1,
                map(tag_no_case("macro"), |_| (Token::MacroStart, 5)),
            ),
            pl(space0, map(tag_no_case("endm"), |_| (Token::MacroEnd, 4))),
            pl(space0, macro_positional_arg_token),
            pl(space0, map(tag("\\?"), |_| (Token::MacroExpansionCount, 2))),
            pl(space0, include_directive_token),
            pl(
                space0,
                map(terminated(tag_no_case("if"), peek(space1)), |_| {
                    (Token::IfStart, 2)
                }),
            ),
            pl(space0, map(tag_no_case("else"), |_| (Token::Else, 4))),
            pl(space0, map(tag_no_case("endif"), |_| (Token::IfEnd, 5))),
            pl(space0, mnemonic_token),
            pl(space1, map(char('#'), |_| (Token::ImmediatePrefix, 1))),
            pl(
                space1,
                terminated(
                    alt((
                        map(tag_no_case("db"), |_| (Token::DbDirective, 2)),
                        map(tag_no_case("dw"), |_| (Token::DwDirective, 2)),
                        map(tag_no_case("ds"), |_| (Token::DsDirective, 2)),
                    )),
                    peek(space1),
                ),
            ),
            pn(alt((
                offset_operand_token("x", |_| Token::OffsetByXOperand),
                offset_operand_token("y", |_| Token::OffsetByYOperand),
            ))),
            pl(
                space0,
                map(tag_no_case("end"), |_| (Token::EndDirective, 3)),
            ),
            pl(space0, identifier_token),
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
            pl(
                space0,
                alt((
                    map(tag("="), |_| (Token::EqualsOperator, 1)),
                    map(tag("!="), |_| (Token::NotEqualsOperator, 2)),
                    map(tag("!"), |_| (Token::BangOperator, 1)),
                    map(tag("~"), |_| (Token::ComplementOperator, 1)),
                    map(tag("|"), |_| (Token::OrOperator, 1)),
                    map(tag("^"), |_| (Token::XorOperator, 1)),
                    map(tag("&"), |_| (Token::AndOperator, 1)),
                    map(tag("+"), |_| (Token::PlusOperator, 1)),
                    map(tag("-"), |_| (Token::MinusOperator, 1)),
                    map(tag("*"), |_| (Token::StarOperator, 1)),
                    map(tag("<<"), |_| (Token::LeftShiftOperator, 2)),
                    map(tag(">>"), |_| (Token::RightShiftOperator, 2)),
                    map(tag(">="), |_| (Token::GreaterThanOrEqualToOperator, 2)),
                    map(tag("<="), |_| (Token::LessThanOrEqualToOperator, 2)),
                    map(tag(">"), |_| (Token::GreaterThanOperator, 1)),
                    map(tag("<"), |_| (Token::LessThanOperator, 1)),
                )),
            ),
            pl(space0, map(char('('), |_| (Token::OpenParen, 1))),
        )),
        alt((
            pl(space0, map(char(')'), |_| (Token::CloseParen, 1))),
            pl(space0, string_literal_token),
            pn(map(character_literal_token, |lit| (lit, 3))),
            pn(map(tag(","), |_| (Token::Comma, 1))),
            pl(space0, map(newline, |_| (Token::Newline, 1))),
            pl(space0, invalid_token),
        )),
    ))(input)
}

/// Returns the token alongside the length in bytes consumed by the token, and left and right
/// padding in spaces. This method is for left-padded tokens and unconditionally returns 0 right
/// padding.
fn pl<'a, F, G>(
    space_parser: F,
    token_parser: G,
) -> impl Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16, u16)>
where
    F: Fn(&'a str) -> IResult<&'a str, &'a str>,
    G: Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16)>,
{
    map(
        pair(
            map(space_parser, |padding| {
                u16::try_from(padding.len()).unwrap()
            }),
            token_parser,
        ),
        |(padding_left, (token, token_length))| (token, token_length, padding_left),
    )
}

/// Returns the token alongside the left and right padding in spaces. This method is for tokens that
/// have no padding on either side, and unconditionally return zero for left and right padding.
fn pn<'a, G>(token_parser: G) -> impl Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16, u16)>
where
    G: Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16)>,
{
    map(token_parser, |(token, token_len)| (token, token_len, 0))
}

fn invalid_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        take_while1(|chr: char| !chr.is_ascii() || !(is_space(chr as u8) || chr == '\n')),
        |invalid| {
            (
                Token::Invalid(invalid),
                u16::try_from(invalid.chars().count()).unwrap(),
            )
        },
    )(input)
}

fn comment_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        preceded(
            char(';'),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        |comment| {
            (
                Token::Comment(comment),
                u16::try_from(1 + comment.len()).unwrap(),
            )
        },
    )(input)
}

fn error_directive_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        pair(
            terminated(tag("ERROR ERROR ERROR"), space1),
            take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        |(taken, err)| {
            (
                Token::ErrorDirective(err),
                u16::try_from(taken.len() + err.len()).unwrap(),
            )
        },
    )(input)
}

fn offset_operand_token<'a, F>(
    register: &'static str,
    mapper: F,
) -> impl Fn(&'a str) -> IResult<&'a str, (Token<'a>, u16)>
where
    F: Fn(&'a str) -> Token<'a>,
{
    map(
        preceded(char(','), tag_no_case(register)),
        move |register| (mapper(register), u16::try_from(register.len()).unwrap()),
    )
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
    F: Fn(&'a str) -> Op,
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
    F: Fn(&str) -> Op,
{
    map(terminated(tag_no_case(mnemonic), peek(space1)), mapper)
}

fn mnemonic_token(input: &str) -> IResult<&str, (Token, u16)> {
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
        |mnemonic| (Token::Mnemonic(mnemonic), 3),
    )(input)
}

fn peek_comment_or_newline(input: &str) -> IResult<&str, ()> {
    peek(alt((map(newline, |_| ()), map(comment_token, |_| ()))))(input)
}

fn macro_positional_arg_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        map_res(
            preceded(char('\\'), take_while1(|chr: char| is_digit(chr as u8))),
            |digits| {
                u8::from_str_radix(digits, 10)
                    .map(|val| (val, u16::try_from(digits.len() + 1).unwrap()))
            },
        ),
        |(val, digits_len)| (Token::MacroPositionalArg(val), digits_len),
    )(input)
}

fn hex_literal_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        map_res(preceded(char('$'), hex_digit1), |digits| {
            u16::from_str_radix(digits, 16)
                .map(|val| (val, u16::try_from(digits.len() + 1).unwrap()))
        }),
        |(val, token_length)| (Token::HexLiteral(val), token_length),
    )(input)
}

fn dec_literal_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        map_res(digit1, |digits| {
            u16::from_str_radix(digits, 10).map(|val| (val, u16::try_from(digits.len()).unwrap()))
        }),
        |(val, token_length)| (Token::DecLiteral(val), token_length),
    )(input)
}

fn oct_literal_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        map_res(preceded(char('@'), oct_digit1), |digits| {
            u16::from_str_radix(digits, 8).map(|val| (val, u16::try_from(digits.len()).unwrap()))
        }),
        |(val, token_length)| (Token::OctLiteral(val), token_length),
    )(input)
}

fn bin_literal_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        map_res(
            preceded(char('%'), take_while1(|chr| chr == '0' || chr == '1')),
            |digits| {
                u16::from_str_radix(digits, 2)
                    .map(|val| (val, u16::try_from(digits.len() + 1).unwrap()))
            },
        ),
        |(val, token_length)| (Token::BinLiteral(val), token_length),
    )(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn string_literal_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        delimited(
            char('"'),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            char('"'),
        ),
        |lit| {
            (
                Token::StringLiteral(lit),
                u16::try_from(lit.len() + 2).unwrap(),
            )
        },
    )(input)
}

fn identifier_token(input: &str) -> IResult<&str, (Token, u16)> {
    map_res(
        take_while1(|chr: char| {
            chr.is_ascii_alphanumeric() || chr == '_' || chr == '\\' || chr == '?'
        }),
        |identifier: &str| {
            if IDENT_REGEX.is_match(identifier) {
                Ok((
                    Token::Identifier(identifier),
                    u16::try_from(identifier.len()).unwrap(),
                ))
            } else {
                Err(())
            }
        },
    )(input)
}

fn include_directive_token(input: &str) -> IResult<&str, (Token, u16)> {
    map(
        terminated(
            pair(
                delimited(tag_no_case("include"), space1, char('"')),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            ),
            char('"'),
        ),
        |(spaces, path)| {
            (
                Token::IncludeDirective(path),
                u16::try_from(spaces.len() + path.len() + 9).unwrap(),
            )
        },
    )(input)
}
