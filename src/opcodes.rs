use crate::{
    addressing_modes::*,
    cpu::{Cpu, Mapper},
};

pub trait Instruction<M: Mapper> {
    type Operand: Copy + Clone + std::fmt::Debug;
    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>);
}

pub struct Asl;

impl<M: Mapper> Instruction<M> for Asl {
    type Operand = u8;

    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, val| {
            let carry = (val & 0b1000_0000) > 0;
            let res = val << 1;
            cpu.set_carry(carry);
            cpu.apply_sign_and_zero_flags(res);
            res
        });
    }
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
        let target = AM::read(cpu);
        cpu.pc = target;
    }
}
