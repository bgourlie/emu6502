use crate::parser::{
    parse, tlex,
    types::{BinaryOperator, Expression, Line},
};
use std::rc::Rc;

#[test]
fn test_macro_decl() {
    let tokens = tlex("foo macro\n");
    let (_, lines) = parse(&tokens).unwrap();
    assert_eq!(Line::MacroStart("foo"), lines[0]);
}

#[test]
fn test_macro_end() {
    let tokens = tlex("endm ; This is a comment\n");
    let (_, lines) = parse(&tokens).unwrap();
    assert_eq!(Line::MacroEnd, lines[0]);
}

#[test]
fn test_macro_invocation() {
    let tokens = tlex("some_macro_or_label\n");
    let (_, lines) = parse(&tokens).unwrap();
    assert_eq!(
        Line::MacroInvocationOrLabel("some_macro_or_label"),
        lines[0]
    );
}

#[test]
fn test_macro_invocation_with_args() {
    let tokens = tlex("some_macro 1,2+2,3,4\n");
    let (_, lines) = parse(&tokens).unwrap();
    let expected_args = vec![
        Rc::new(Expression::Literal(1)),
        Rc::new(Expression::Binary(
            Rc::new(Expression::Literal(2)),
            BinaryOperator::Addition,
            Rc::new(Expression::Literal(2)),
        )),
        Rc::new(Expression::Literal(3)),
        Rc::new(Expression::Literal(4)),
    ];

    if let Line::MacroInvocation(macro_name, args) = &lines[0] {
        assert_eq!(&"some_macro", macro_name);
        expected_args
            .iter()
            .zip(args.iter())
            .for_each(|(expected, actual)| {
                assert_eq!(expected, actual);
            })
    } else {
        panic!("Not a macro invocation line with args");
    }
}

#[test]
fn test_eq_directive2() {
    let tokens = tlex("carry   equ %00000001\n");
    let (_, lines) = parse(&tokens).unwrap();
    assert_eq!(1, lines.len());
    assert_eq!(
        Line::Equ("carry", Rc::new(Expression::Literal(1))),
        lines[0]
    );
}
