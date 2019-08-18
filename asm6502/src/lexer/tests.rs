use super::*;

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