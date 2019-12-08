use super::instruction;
use crate::{
    Token, Lexer,
    types::{BinaryOperator, Expression, Operand, Symbol},
};
use shared6502::Op;
use std::rc::Rc;

#[test]
fn test_implied() {
    let tokens: Vec<Token> = Lexer::new("BRK\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Brk, opcode);
    assert_eq!(Operand::Implied, operand);
}

#[test]
fn test_immediate() {
    let tokens: Vec<Token> = Lexer::new("LDA #20\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::Immediate(Rc::new(Expression::Literal(20))),
        operand
    );
}

#[test]
fn test_indirect() {
    let tokens: Vec<Token> = Lexer::new("JMP ($ff)\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Jmp, opcode);
    assert_eq!(
        Operand::Indirect(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indexed_indirect() {
    let tokens: Vec<Token> = Lexer::new("LDA ($ff,x)\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndexedIndirect(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_indirect_indexed() {
    let tokens: Vec<Token> = Lexer::new("LDA ($ff),y\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::IndirectIndexed(Rc::new(Expression::Literal(0xff))),
        operand
    );
}

#[test]
fn test_absolute_x() {
    let tokens: Vec<Token> = Lexer::new("LDA $ffff,x\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteX(Rc::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_absolute_y() {
    let tokens: Vec<Token> = Lexer::new("LDA $ffff,y\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteY(Rc::new(Expression::Literal(0xffff))),
        operand
    );
}

#[test]
fn test_indirect_indexed_with_complex_expr() {
    let tokens: Vec<Token> = Lexer::new("LDA ((addr - 1)),y\n").collect();
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
    let tokens: Vec<Token> = Lexer::new("LDA $ff\n").collect();
    let (_, (opcode, operand)) = instruction(&tokens).unwrap();
    assert_eq!(Op::Lda, opcode);
    assert_eq!(
        Operand::AbsoluteOrRelative(Rc::new(Expression::Literal(0xff))),
        operand
    );
}
