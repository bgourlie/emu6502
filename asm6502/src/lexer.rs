use {
    nom::{
        branch::alt,
        bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
        character::complete::{char, digit1, hex_digit1, oct_digit1, one_of, space0, space1},
        combinator::{map, map_res, opt, peek, recognize},
        multi::many0,
        sequence::{delimited, preceded},
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
}

pub fn parse(input: Span) -> IResult<Span, Vec<Token>> {
    many0(alt((
        alt((
            comment,
            error_directive,
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
            equals_operator,
            not_equals_operator,
            complement_operator,
            or_operator,
            and_operator,
        )),
        alt((
            plus_operator,
            star_operator,
            left_shift_operator,
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
            newline,
        )),
    )))(input)
}

fn newline(input: Span) -> IResult<Span, Token> {
    map(
        preceded(
            space0,
            preceded(opt(char('\r')), nom::character::complete::newline),
        ),
        |_| Token::Newline,
    )(input)
}

fn error_directive(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        preceded(
            delimited(space0, tag("ERROR ERROR ERROR"), space1),
            take_while1(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
        ),
        move |msg: Span| Token::ErrorDirective(span, msg.fragment),
    )(input)
}

fn equals_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('='), space0), move |_| {
        Token::EqualsOperator(span)
    })(input)
}

fn not_equals_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag("!="), space0), move |_| {
        Token::EqualsOperator(span)
    })(input)
}

fn comma(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(tag(","), move |_| Token::Comma(span))(input)
}

fn offset_by_x_operand(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(tag_no_case(",x"), move |_| Token::OffsetByXOperand(span))(input)
}

fn offset_by_y_operand(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(tag_no_case(",y"), move |_| Token::OffsetByYOperand(span))(input)
}

// TODO: Add escaping https://github.com/Geal/nom/issues/1014
fn character_literal(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map_res(
        delimited(char('\''), take(1_usize), char('\'')),
        move |chr: Span| {
            let chr: char = chr.fragment.chars().nth(0_usize).unwrap();
            if chr.is_ascii() && !chr.is_ascii_control() {
                Ok(Token::CharacterLiteral(span, chr))
            } else {
                Err(())
            }
        },
    )(input)
}

#[test]
fn test_character_literal() {
    assert_eq!(
        character_literal(Span::new("'a'")),
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
        character_literal(Span::new("'\n'")),
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

fn immediate_prefix(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(char('#'), move |_| Token::ImmediatePrefix(span))(input)
}

fn mnemonic(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        delimited(
            space0,
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
            space0, // TODO: use space0 for implied instructions, space1 for non-implied
        ),
        move |op| Token::Mnemonic(span, op),
    )(input)
}

fn if_start(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag_no_case("if"), space1), move |_| {
        Token::IfStart(span)
    })(input)
}

#[test]
fn test_if_start() {
    assert_eq!(
        if_start(Span::new("if ")),
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

fn if_end(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag_no_case("endif"), space0), move |_| {
        Token::IfEnd(span)
    })(input)
}

fn macro_start(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(preceded(space1, tag_no_case("macro")), move |_| {
        Token::MacroStart(span)
    })(input)
}

fn macro_end(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag_no_case("endm"), space0), move |_| {
        Token::MacroEnd(span)
    })(input)
}

fn macro_invocation_count_arg(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(tag("\\?"), move |_| Token::MacroInvokeCountArg(span))(input)
}

fn macro_positional_arg(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        map_res(preceded(char('\\'), one_of("123456789")), parse_u8_dec),
        move |arg| Token::MacroPositionalArg(span, arg),
    )(input)
}

#[test]
fn test_macro_arg() {
    assert_eq!(
        macro_positional_arg(Span::new("\\1")),
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

fn complement_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('~'), space0), move |_| {
        Token::ComplementOperator(span)
    })(input)
}

fn or_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('|'), space0), move |_| {
        Token::OrOperator(span)
    })(input)
}

fn and_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('&'), space0), move |_| {
        Token::AndOperator(span)
    })(input)
}

fn plus_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('+'), space0), move |_| {
        Token::PlusOperator(span)
    })(input)
}

fn star_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('*'), space0), move |_| {
        Token::StarOperator(span)
    })(input)
}

fn less_than_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('<'), space0), move |_| {
        Token::LessThanOperator(span)
    })(input)
}

fn greater_than_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, char('>'), space0), move |_| {
        Token::GreaterThanOperator(span)
    })(input)
}

fn less_than_or_equal_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag("<="), space0), move |_| {
        Token::LessThanOrEqualToOperator(span)
    })(input)
}

fn greater_than_or_equal_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag(">="), space0), move |_| {
        Token::GreaterThanOrEqualToOperator(span)
    })(input)
}

fn right_shift_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag(">>"), space0), move |_| {
        Token::RightShiftOperator(span)
    })(input)
}

fn left_shift_operator(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag("<<"), space0), move |_| {
        Token::LeftShiftOperator(span)
    })(input)
}

fn noopt_directive(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(preceded(space0, tag_no_case("noopt")), move |_| {
        Token::NoOptDirective(span)
    })(input)
}
fn sub_expr_start(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(char('('), move |_| Token::SubExprStart(span))(input)
}

fn sub_expr_end(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(char(')'), move |_| Token::SubExprEnd(span))(input)
}

fn equ_directive(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(delimited(space0, tag_no_case("equ"), space1), move |_| {
        Token::EquDirective(span)
    })(input)
}

fn hex_literal(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        map_res(preceded(char('$'), hex_digit1), parse_i32_hex),
        move |val| Token::HexLiteral(span, val),
    )(input)
}

#[test]
fn test_hex_literal() {
    assert_eq!(
        hex_literal(Span::new("$1")),
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
        hex_literal(Span::new("$ff")),
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

fn dec_literal(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        map_res(recognize(preceded(opt(char('-')), digit1)), parse_i32_dec),
        move |val| Token::DecLiteral(span, val),
    )(input)
}

#[test]
fn test_dec_literal() {
    assert_eq!(
        dec_literal(Span::new("-234")),
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
        dec_literal(Span::new("8234")),
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

fn oct_literal(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        map_res(preceded(char('@'), oct_digit1), parse_i32_oct),
        move |val| Token::OctLiteral(span, val),
    )(input)
}

#[test]
fn test_oct_literal() {
    assert_eq!(
        oct_literal(Span::new("@1")),
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
        oct_literal(Span::new("@10")),
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

fn bin_literal(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        map_res(
            preceded(char('%'), take_while1(|chr| chr == '0' || chr == '1')),
            parse_i32_bin,
        ),
        move |val| Token::BinLiteral(span, val),
    )(input)
}

#[test]
fn test_bin_literal() {
    assert_eq!(
        bin_literal(Span::new("%00000001")),
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
        bin_literal(Span::new("%11111111")),
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
fn string_literal(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        delimited(
            char('"'),
            take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control() && chr != '\"'),
            char('"'),
        ),
        move |string: Span| Token::StringLiteral(span, string.fragment),
    )(input)
}

#[test]
fn test_string_literal() {
    assert_eq!(
        string_literal(Span::new("\"Why ~hello~ there!\" abc")),
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

fn identifier(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        delimited(
            space0,
            preceded(
                valid_identifier_start,
                take_while1(|chr: char| chr.is_ascii_alphanumeric() || chr == '_'),
            ),
            space0,
        ),
        move |identifier| Token::Identifier(span, identifier.fragment),
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
fn test_identifier() {
    assert_eq!(
        identifier(Span::new("some_thing123 abc")),
        Ok((
            Span {
                offset: 13,
                line: 1,
                fragment: " abc",
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
        identifier(Span::new("123some_thing abc")),
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

fn comment(input: Span) -> IResult<Span, Token> {
    let (input, span) = position(input)?;
    map(
        preceded(
            space0,
            preceded(
                char(';'),
                take_while(|chr: char| chr.is_ascii() && !chr.is_ascii_control()),
            ),
        ),
        move |comment: Span| Token::Comment(span, comment.fragment),
    )(input)
}

#[test]
fn test_comment() {
    assert_eq!(
        comment(Span::new(";")),
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
        comment(Span::new("; hello world!")),
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
