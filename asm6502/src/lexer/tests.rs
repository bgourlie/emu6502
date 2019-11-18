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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::CharacterLiteral('a')
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::IfStart
            )
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::MacroPositionalArg(1)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::HexLiteral(1)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::HexLiteral(255)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::DecLiteral(-234)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::DecLiteral(8234)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::OctLiteral(1)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::OctLiteral(8)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::BinLiteral(1)
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::BinLiteral(255)
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
                offset: 13,
                line: 1,
                fragment: " abc",
                extra: ()
            },
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::Identifier("some_thing123")
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::Comment("")
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::Comment(" hello world!")
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
            (
                Span {
                    offset: 0,
                    line: 1,
                    fragment: "",
                    extra: ()
                },
                Token::StringLiteral("Why ~hello~ there!")
            )
        ))
    );
}

#[test]
fn test_negative_precedence_issue() {
    let tokens: Vec<Token> = lex(Span::new("ram_top = -1"))
        .unwrap()
        .1
        .into_iter()
        .map(|(_, token)| token)
        .collect();
    assert_eq!(3, tokens.len());
    assert_eq!(Token::Identifier("ram_top"), tokens[0]);
    assert_eq!(Token::EqualsOperator, tokens[1]);
    assert_eq!(Token::DecLiteral(-1), tokens[2]);
}
