use crate::parser::{
    types::{BinaryOperator, Expression, Line},
    *,
};

#[test]
fn test_macro_decl() {
    let tokens = tparse("foo macro\n");
    let (_, line) = macro_decl(&tokens).unwrap();
    assert_eq!(Line::MacroStart("foo"), line);
}

#[test]
fn test_macro_end() {
    let tokens = tparse("endm ; This is a comment\n");
    let (_, line) = macro_end(&tokens).unwrap();
    assert_eq!(Line::MacroEnd, line);
}

#[test]
fn test_macro_invocation() {
    let tokens = tparse("some_macro\n");
    let (_, line) = macro_invocation(&tokens).unwrap();
    assert_eq!(Line::MacroInvocation("some_macro", None), line);
}

#[test]
fn test_macro_invocation_with_args() {
    let tokens = tparse("some_macro 1,2+2,3,4\n");
    let (_, line) = macro_invocation(&tokens).unwrap();
    let expected_args = vec![
        Expression::Literal(1),
        Expression::Binary(
            Box::new(Expression::Literal(2)),
            BinaryOperator::Addition,
            Box::new(Expression::Literal(2)),
        ),
        Expression::Literal(3),
        Expression::Literal(4),
    ];

    if let Line::MacroInvocation(macro_name, Some(args)) = line {
        assert_eq!("some_macro", macro_name);
        expected_args
            .iter()
            .zip(args.iter())
            .for_each(|(expected, actual)| {
                assert_eq!(expected, actual);
            })
    } else {
        assert!(false, "Not a macro invocation line or args list was none");
    }
}
