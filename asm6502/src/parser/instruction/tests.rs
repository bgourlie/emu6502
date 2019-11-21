use super::instruction;
use crate::parser::{
    tlex,
    types::{BinaryOperator, Expression, Operand},
};
use shared6502::Op;

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
        Operand::Immediate(Box::new(Expression::Literal(20))),
        operand
    );
}

#[test]
fn test_indirect() {
    let tokens = tlex("JMP ($ff)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Jmp, opcode);
    assert_eq!(
        Operand::Indirect(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indexed_indirect() {
    let tokens = tlex("LDA ($ff,x)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndexedIndirect(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indirect_indexed() {
    let tokens = tlex("LDA ($ff),y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_absolute_x() {
    let tokens = tlex("LDA $ffff,x\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteX(Box::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_absolute_y() {
    let tokens = tlex("LDA $ffff,y\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteY(Box::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_indirect_indexed_with_complex_expr() {
    let tokens = tlex("LDA ((addr - 1)),y\n");
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
    let tokens = tlex("LDA $ff\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteOrRelative(Box::new(Expression::Literal(0xff))),
        operand
    );
}
