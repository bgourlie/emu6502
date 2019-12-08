use super::*;

#[test]
fn test_character_literal_token() {
    let mut lexer = Lexer::new("'a'");
    assert_eq!(Some(Token::CharacterLiteral('a')), lexer.next());
}

#[test]
fn test_if_start_token() {
    let mut lexer = Lexer::new("if ");
    assert_eq!(Some(Token::IfStart), lexer.next());
}

#[test]
fn test_macro_arg_token() {
    let mut lexer = Lexer::new("\\1");
    assert_eq!(Some(Token::MacroPositionalArg(1)), lexer.next());
}

#[test]
fn test_hex_literal_token() {
    let mut lexer = Lexer::new("$1 $ff");
    assert_eq!(Some(Token::HexLiteral(1)), lexer.next());
    assert_eq!(Some(Token::HexLiteral(255)), lexer.next());
}

#[test]
fn test_dec_literal_token() {
    let mut lexer = Lexer::new("234 8234");
    assert_eq!(Some(Token::DecLiteral(234)), lexer.next());
    assert_eq!(Some(Token::DecLiteral(8234)), lexer.next());
}

#[test]
fn test_oct_literal_token() {
    let mut lexer = Lexer::new("@1 @10");
    assert_eq!(Some(Token::OctLiteral(1)), lexer.next());
    assert_eq!(Some(Token::OctLiteral(8)), lexer.next());
}

#[test]
fn test_bin_literal_token() {
    let mut lexer = Lexer::new("%00000001 %11111111");
    assert_eq!(Some(Token::BinLiteral(1)), lexer.next());
    assert_eq!(Some(Token::BinLiteral(255)), lexer.next());
}

#[test]
fn test_identifier_token() {
    let mut lexer = Lexer::new("some_thing123\\? 123some_thing");
    assert_eq!(Some(Token::Identifier("some_thing123\\?")), lexer.next());
    assert_eq!(Some(Token::Invalid("123some_thing")), lexer.next());
}

#[test]
fn test_comment_token() {
    let mut lexer = Lexer::new("; hello");
    assert_eq!(Some(Token::Comment(" hello")), lexer.next());
}

#[test]
fn test_string_literal_token() {
    let mut lexer = Lexer::new("\"Why ~hello~ there!\"");
    assert_eq!(
        Some(Token::StringLiteral("Why ~hello~ there!")),
        lexer.next()
    );
}

#[test]
fn test_negative_precedence_issue() {
    let mut lexer = Lexer::new("ram_top = -1");
    assert_eq!(Some(Token::Identifier("ram_top")), lexer.next());
    assert_eq!(Some(Token::EqualsOperator), lexer.next());
    assert_eq!(Some(Token::MinusOperator), lexer.next());
    assert_eq!(Some(Token::DecLiteral(1)), lexer.next());
}
