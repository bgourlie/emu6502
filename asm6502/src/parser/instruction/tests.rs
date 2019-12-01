use super::instruction;
use crate::parser::{
    tlex,
    types::{BinaryOperator, Expression, Operand, Symbol},
};
use shared6502::Op;
use std::rc::Rc;

#[test]
fn test_implied() {
    let tokens = tlex("BRK\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Brk, opcode);
    assert_eq!(Operand::Implied, operand);
}

#[test]
fn test_immediate() {
    let tokens = tlex("LDA #20\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::Immediate(Rc::new(Expression::Literal(20))),
        operand
    );
}

#[test]
fn test_indirect() {
    let tokens = tlex("JMP ($ff)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Jmp, opcode);
    assert_eq!(
        Operand::Indirect(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indexed_indirect() {
    let tokens = tlex("LDA ($ff,x)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndexedIndirect(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indirect_indexed() {
    let tokens = tlex("LDA ($ff),y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_absolute_x() {
    let tokens = tlex("LDA $ffff,x\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteX(Rc::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_absolute_y() {
    let tokens = tlex("LDA $ffff,y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteY(Rc::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_indirect_indexed_with_complex_expr() {
    let tokens = tlex("LDA ((addr - 1)),y\n");
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
    let tokens = tlex("LDA $ff\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteOrRelative(Rc::new(Expression::Literal(0xff))),
        operand
    );
}
