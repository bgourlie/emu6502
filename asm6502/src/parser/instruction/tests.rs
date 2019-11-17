use super::instruction;
use crate::parser::{tparse, BinaryOperator, Expression, Operand};
use shared6502::Op;

#[test]
fn test_implied() {
    let tokens = tparse("BRK\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Brk, opcode);
    assert_eq!(Operand::Implied, operand);
}

#[test]
fn test_immediate() {
    let tokens = tparse("LDA #20\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::Immediate(Box::new(Expression::Literal(20))),
        operand
    );
}

#[test]
fn test_indirect() {
    let tokens = tparse("JMP ($ff)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Jmp, opcode);
    assert_eq!(
        Operand::Indirect(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indexed_indirect() {
    let tokens = tparse("LDA ($ff,x)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndexedIndirect(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indirect_indexed() {
    let tokens = tparse("LDA ($ff),y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_absolute_x() {
    let tokens = tparse("LDA $ffff,x\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteX(Box::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_absolute_y() {
    let tokens = tparse("LDA $ffff,y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteY(Box::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_indirect_indexed_with_complex_expr() {
    let tokens = tparse("LDA ((addr - 1)),y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Box::new(Expression::Grouping(Box::new(
            Expression::Binary(
                Box::new(Expression::Symbol("addr")),
                BinaryOperator::Subtraction,
                Box::new(Expression::Literal(1))
            )
        )))),
        operand
    );
}

#[test]
fn test_absolute_or_relative() {
    let tokens = tparse("LDA $ff\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteOrRelative(Box::new(Expression::Literal(0xff))),
        operand
    );
}
