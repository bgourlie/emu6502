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
            offset_by_x_operand_token,
            offset_by_y_operand_token,
            end_directive_token,
            identifier_token,
            equals_operator_token,
            not_equals_operator_token,
            complement_operator_token,
            or_operator_token,
            xor_operator_token,
        )),
        alt((
            and_operator_token,
            plus_operator_token,
            minus_operator_token,
            star_operator_token,
            left_shift_operator_token,
            right_shift_operator_token,
            greater_than_or_equal_operator_token,
            less_than_or_equal_operator_token,
            greater_than_operator_token,
            less_than_operator_token,
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

fn equals_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('='), space0)),
        |(pos, _)| Token::EqualsOperator(pos),
    )(input)
}

fn not_equals_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag("!="), space0)),
        |(pos, _)| Token::EqualsOperator(pos),
    )(input)
}

fn comma_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, tag(",")), |(pos, _)| Token::Comma(pos))(input)
}

fn offset_by_x_operand_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, tag_no_case(",x")), |(pos, _)| {
        Token::OffsetByXOperand(pos)
    })(input)
}

fn offset_by_y_operand_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, tag_no_case(",y")), |(pos, _)| {
        Token::OffsetByYOperand(pos)
    })(input)
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

#[test]
fn test_character_literal_token() {
    assert_eq!(
        character_literal_token(Span::new("'a'")),
        Ok((
            Span {
                offset: 3,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::CharacterLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                'a'
            )
        ))
    );
    assert_eq!(
        character_literal_token(Span::new("'\n'")),
        Err(nom::Err::Error((
            Span {
                offset: 0,
                line: 1,
                fragment: "'\n'",
                extra: ()
            },
            nom::error::ErrorKind::MapRes
        )))
    );
}

fn immediate_prefix_token(input: Span) -> IResult<Span, Token> {
    map(pair(position, char('#')), |(pos, _)| {
        Token::ImmediatePrefix(pos)
    })(input)
}

fn mnemonic_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(
            position,
            delimited(
                space0,
                alt((
                    alt((
                        map(terminated(tag_no_case("adc"), space1), |_| Op::Adc),
                        map(terminated(tag_no_case("and"), space1), |_| Op::And),
                        map(terminated(tag_no_case("asl"), space1), |_| Op::Asl),
                        map(terminated(tag_no_case("bcc"), comment_or_newline), |_| {
                            Op::Bcc
                        }),
                        map(terminated(tag_no_case("bcs"), comment_or_newline), |_| {
                            Op::Bcs
                        }),
                        map(terminated(tag_no_case("beq"), comment_or_newline), |_| {
                            Op::Beq
                        }),
                        map(terminated(tag_no_case("bit"), comment_or_newline), |_| {
                            Op::Bit
                        }),
                        map(terminated(tag_no_case("bmi"), comment_or_newline), |_| {
                            Op::Bmi
                        }),
                        map(terminated(tag_no_case("bne"), comment_or_newline), |_| {
                            Op::Bne
                        }),
                        map(terminated(tag_no_case("bpl"), comment_or_newline), |_| {
                            Op::Bpl
                        }),
                        map(terminated(tag_no_case("brk"), comment_or_newline), |_| {
                            Op::Brk
                        }),
                        map(terminated(tag_no_case("bvc"), comment_or_newline), |_| {
                            Op::Bvc
                        }),
                        map(terminated(tag_no_case("bvs"), comment_or_newline), |_| {
                            Op::Bvs
                        }),
                        map(terminated(tag_no_case("clc"), comment_or_newline), |_| {
                            Op::Clc
                        }),
                        map(terminated(tag_no_case("cld"), comment_or_newline), |_| {
                            Op::Cld
                        }),
                        map(terminated(tag_no_case("cli"), comment_or_newline), |_| {
                            Op::Cli
                        }),
                        map(terminated(tag_no_case("clv"), comment_or_newline), |_| {
                            Op::Clv
                        }),
                        map(terminated(tag_no_case("cmp"), space1), |_| Op::Cmp),
                        map(terminated(tag_no_case("cpx"), space1), |_| Op::Cpx),
                        map(terminated(tag_no_case("cpy"), space1), |_| Op::Cpy),
                        map(terminated(tag_no_case("dec"), space1), |_| Op::Dec),
                    )),
                    alt((
                        map(terminated(tag_no_case("dex"), comment_or_newline), |_| {
                            Op::Dex
                        }),
                        map(terminated(tag_no_case("dey"), comment_or_newline), |_| {
                            Op::Dey
                        }),
                        map(terminated(tag_no_case("eor"), space1), |_| Op::Eor),
                        map(terminated(tag_no_case("inc"), space1), |_| Op::Inc),
                        map(terminated(tag_no_case("inx"), comment_or_newline), |_| {
                            Op::Inx
                        }),
                        map(terminated(tag_no_case("iny"), comment_or_newline), |_| {
                            Op::Iny
                        }),
                        map(terminated(tag_no_case("jmp"), space1), |_| Op::Jmp),
                        map(terminated(tag_no_case("jsr"), space1), |_| Op::Jsr),
                        map(terminated(tag_no_case("lda"), space1), |_| Op::Lda),
                        map(terminated(tag_no_case("ldx"), space1), |_| Op::Ldx),
                        map(terminated(tag_no_case("ldy"), space1), |_| Op::Ldy),
                        map(terminated(tag_no_case("lsr"), space1), |_| Op::Lsr),
                        map(terminated(tag_no_case("nop"), comment_or_newline), |_| {
                            Op::Nop
                        }),
                        map(terminated(tag_no_case("ora"), space1), |_| Op::Ora),
                        map(terminated(tag_no_case("pha"), comment_or_newline), |_| {
                            Op::Pha
                        }),
                        map(terminated(tag_no_case("php"), comment_or_newline), |_| {
                            Op::Php
                        }),
                        map(terminated(tag_no_case("pla"), comment_or_newline), |_| {
                            Op::Pla
                        }),
                        map(terminated(tag_no_case("plp"), comment_or_newline), |_| {
                            Op::Plp
                        }),
                        map(terminated(tag_no_case("rol"), space1), |_| Op::Rol),
                        map(terminated(tag_no_case("ror"), space1), |_| Op::Ror),
                        map(terminated(tag_no_case("rti"), comment_or_newline), |_| {
                            Op::Rti
                        }),
                    )),
                    alt((
                        map(terminated(tag_no_case("rts"), comment_or_newline), |_| {
                            Op::Rts
                        }),
                        map(terminated(tag_no_case("sbc"), space1), |_| Op::Sbc),
                        map(terminated(tag_no_case("sec"), comment_or_newline), |_| {
                            Op::Sec
                        }),
                        map(terminated(tag_no_case("sed"), comment_or_newline), |_| {
                            Op::Sed
                        }),
                        map(terminated(tag_no_case("sei"), comment_or_newline), |_| {
                            Op::Sei
                        }),
                        map(terminated(tag_no_case("sta"), space1), |_| Op::Sta),
                        map(terminated(tag_no_case("stx"), space1), |_| Op::Stx),
                        map(terminated(tag_no_case("sty"), space1), |_| Op::Sty),
                        map(terminated(tag_no_case("tax"), comment_or_newline), |_| {
                            Op::Tax
                        }),
                        map(terminated(tag_no_case("tay"), comment_or_newline), |_| {
                            Op::Tay
                        }),
                        map(terminated(tag_no_case("tsx"), comment_or_newline), |_| {
                            Op::Tsx
                        }),
                        map(terminated(tag_no_case("txa"), comment_or_newline), |_| {
                            Op::Txa
                        }),
                        map(terminated(tag_no_case("txs"), comment_or_newline), |_| {
                            Op::Txs
                        }),
                        map(terminated(tag_no_case("tya"), comment_or_newline), |_| {
                            Op::Tya
                        }),
                    )),
                )),
                space0,
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

#[test]
fn test_if_start_token() {
    assert_eq!(
        if_start_token(Span::new("if ")),
        Ok((
            Span {
                offset: 3,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::IfStart(Span {
                offset: 0,
                line: 1,
                fragment: "",
                extra: ()
            })
        ))
    );
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

#[test]
fn test_macro_arg_token() {
    assert_eq!(
        macro_positional_arg_token(Span::new("\\1")),
        Ok((
            Span {
                offset: 2,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::MacroPositionalArg(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                1
            )
        ))
    )
}

fn complement_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('~'), space0)),
        |(pos, _)| Token::ComplementOperator(pos),
    )(input)
}

fn or_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('|'), space0)),
        |(pos, _)| Token::OrOperator(pos),
    )(input)
}

fn xor_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('^'), space0)),
        |(pos, _)| Token::XorOperator(pos),
    )(input)
}

fn and_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('&'), space0)),
        |(pos, _)| Token::AndOperator(pos),
    )(input)
}

fn plus_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('+'), space0)),
        |(pos, _)| Token::PlusOperator(pos),
    )(input)
}

fn minus_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('-'), space0)),
        |(pos, _)| Token::MinusOperator(pos),
    )(input)
}

fn star_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('*'), space0)),
        |(pos, _)| Token::StarOperator(pos),
    )(input)
}

fn less_than_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('<'), space0)),
        |(pos, _)| Token::LessThanOperator(pos),
    )(input)
}

fn greater_than_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, char('>'), space0)),
        |(pos, _)| Token::GreaterThanOperator(pos),
    )(input)
}

fn less_than_or_equal_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag("<="), space0)),
        |(pos, _)| Token::LessThanOrEqualToOperator(pos),
    )(input)
}

fn greater_than_or_equal_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag(">="), space0)),
        |(pos, _)| Token::GreaterThanOrEqualToOperator(pos),
    )(input)
}

fn right_shift_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag(">>"), space0)),
        |(pos, _)| Token::RightShiftOperator(pos),
    )(input)
}

fn left_shift_operator_token(input: Span) -> IResult<Span, Token> {
    map(
        pair(position, delimited(space0, tag("<<"), space0)),
        |(pos, _)| Token::LeftShiftOperator(pos),
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

#[test]
fn test_hex_literal_token() {
    assert_eq!(
        hex_literal_token(Span::new("$1")),
        Ok((
            Span {
                offset: 2,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::HexLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                1
            )
        ))
    );
    assert_eq!(
        hex_literal_token(Span::new("$ff")),
        Ok((
            Span {
                offset: 3,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::HexLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                255
            )
        ))
    );
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

#[test]
fn test_dec_literal_token() {
    assert_eq!(
        dec_literal_token(Span::new("-234")),
        Ok((
            Span {
                offset: 4,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::DecLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                -234
            )
        ))
    );
    assert_eq!(
        dec_literal_token(Span::new("8234")),
        Ok((
            Span {
                offset: 4,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::DecLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                8234
            )
        ))
    );
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

#[test]
fn test_oct_literal_token() {
    assert_eq!(
        oct_literal_token(Span::new("@1")),
        Ok((
            Span {
                offset: 2,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::OctLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                1
            )
        ))
    );
    assert_eq!(
        oct_literal_token(Span::new("@10")),
        Ok((
            Span {
                offset: 3,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::OctLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                8
            )
        ))
    );
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

#[test]
fn test_bin_literal_token() {
    assert_eq!(
        bin_literal_token(Span::new("%00000001")),
        Ok((
            Span {
                offset: 9,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::BinLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                1
            )
        ))
    );
    assert_eq!(
        bin_literal_token(Span::new("%11111111")),
        Ok((
            Span {
                offset: 9,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::BinLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                255
            )
        ))
    );
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

#[test]
fn test_string_literal_token() {
    assert_eq!(
        string_literal_token(Span::new("\"Why ~hello~ there!\" abc")),
        Ok((
            Span {
                offset: 20,
                line: 1,
                fragment: " abc",
                extra: ()
            },
            Token::StringLiteral(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                "Why ~hello~ there!"
            )
        ))
    );
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

#[test]
fn test_identifier_token() {
    assert_eq!(
        identifier_token(Span::new("some_thing123 abc")),
        Ok((
            Span {
                offset: 14,
                line: 1,
                fragment: "abc",
                extra: ()
            },
            Token::Identifier(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                "some_thing123"
            )
        ))
    );

    assert_eq!(
        identifier_token(Span::new("123some_thing abc")),
        Err(nom::Err::Error((
            Span {
                offset: 0,
                line: 1,
                fragment: "123some_thing abc",
                extra: ()
            },
            nom::error::ErrorKind::MapRes
        )))
    );
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

#[test]
fn test_comment_token() {
    assert_eq!(
        comment_token(Span::new(";")),
        Ok((
            Span {
                offset: 1,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::Comment(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                ""
            )
        ))
    );
    assert_eq!(
        comment_token(Span::new("; hello world!")),
        Ok((
            Span {
                offset: 14,
                line: 1,
                fragment: "",
                extra: ()
            },
            Token::Comment(
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                " hello world!"
            )
        ))
    );
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
