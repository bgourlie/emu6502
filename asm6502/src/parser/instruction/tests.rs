use super::instruction;
use crate::parser::{parse, Expression, Operand};
use shared6502::Op;

#[test]
fn test_implied() {
    let tokens = parse("BRK\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Brk, opcode);
    assert_eq!(Operand::Implied, operand);
}

#[test]
fn test_immediate() {
    let tokens = parse("LDA #20\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::Immediate(Box::new(Expression::Literal(20))),
        operand
    );
}

#[test]
fn test_indirect() {
    let tokens = parse("JMP ($ff)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Jmp, opcode);
    assert_eq!(
        Operand::Indirect(Box::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indexed_indirect() {
    let tokens = parse("LDA ($ff,x)\n");
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndexedIndirect(Box::new(Expression::Literal(0xff))),
        operand
    );
}
