use super::instruction;
use crate::{
    lex,
    types::{BinaryOperator, Expression, Operand, Symbol},
};
use shared6502::Op;
use std::rc::Rc;

#[test]
fn test_implied() {
    let (_, tokens) = lex("BRK\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Brk, opcode);
    assert_eq!(Operand::Implied, operand);
}

#[test]
fn test_immediate() {
    let (_, tokens) = lex("LDA #20\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::Immediate(Rc::new(Expression::Literal(20))),
        operand
    );
}

#[test]
fn test_indirect() {
    let (_, tokens) = lex("JMP ($ff)\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Jmp, opcode);
    assert_eq!(
        Operand::Indirect(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indexed_indirect() {
    let (_, tokens) = lex("LDA ($ff,x)\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndexedIndirect(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indirect_indexed() {
    let (_, tokens) = lex("LDA ($ff),y\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_absolute_x() {
    let (_, tokens) = lex("LDA $ffff,x\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteX(Rc::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_absolute_y() {
    let (_, tokens) = lex("LDA $ffff,y\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteY(Rc::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_indirect_indexed_with_complex_expr() {
    let (_, tokens) = lex("LDA ((addr - 1)),y\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Rc::new(Expression::Grouping(Rc::new(Expression::Binary(
            Rc::new(Expression::Symbol(Symbol::Named("addr"))),
            BinaryOperator::Subtraction,
            Rc::new(Expression::Literal(1))
        ))))),
        operand
    );
}

#[test]
fn test_absolute_or_relative() {
    let (_, tokens) = lex("LDA $ff\n").unwrap();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteOrRelative(Rc::new(Expression::Literal(0xff))),
        operand
    );
}
