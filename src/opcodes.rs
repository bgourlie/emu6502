use crate::{
    addressing_modes::*,
    cpu::{Cpu, Mapper},
    util::to_u16,
};

pub trait Instruction<M: Mapper> {
    type Operand: Copy + Clone + std::fmt::Debug;
    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>);
}

pub struct Sta;

impl<M: Mapper> Instruction<M> for Sta {
    type Operand = ();

    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>) {
        AM::write(cpu, cpu.acc)
    }
}

pub struct Jmp;

impl<M: Mapper> Instruction<M> for Jmp {
    type Operand = u16;

    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>) {
        let target = AM::read_operand(cpu);
        cpu.pc = target;
    }
}
