use crate::{
    addressing_modes::*,
    cpu::{Cpu, Mapper},
};

pub trait Instruction<M: Mapper> {
    type Operand: Copy + Clone + std::fmt::Debug;
    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>);
}

pub struct Adc;

impl Adc {
    fn execute<M: Mapper>(cpu: &mut Cpu<M>, lhs: u8, rhs: u8) {
        if cpu.decimal_flag() {
            panic!("Attempted decimal mode arithmetic");
        } else {
            // See http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            let carry = if cpu.carry_flag() { 1 } else { 0 };

            // add using the native word size
            let res = carry + lhs as isize + rhs as isize;

            // if the operation carries into the 8th bit, carry flag will be 1,
            // and zero otherwise.
            let has_carry = res & 0x100 != 0;

            let res = res as u8;

            // Set the overflow flag when both operands have the same sign bit AND
            // the sign bit of the result differs from the two.
            let has_overflow = (lhs ^ rhs) & 0x80 == 0 && (lhs ^ res) & 0x80 != 0;

            cpu.set_carry(has_carry);
            cpu.set_overflow(has_overflow);
            cpu.set_acc_and_apply_flags(res);
        }
    }
}

impl<M: Mapper> Instruction<M> for Adc {
    type Operand = u8;

    fn execute<AM: AddressingMode<M, u8>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = AM::read(cpu);
        Self::execute(cpu, lhs, rhs);
    }
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

pub struct Sbc;

impl<M: Mapper> Instruction<M> for Sbc {
    type Operand = u8;

    fn execute<AM: AddressingMode<M, u8>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = !AM::read(cpu);
        Adc::execute(cpu, lhs, rhs)
    }
}

pub struct Sta;

impl<M: Mapper> Instruction<M> for Sta {
    type Operand = ();

    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>) {
        AM::write(cpu, cpu.acc())
    }
}

pub struct Jmp;

impl<M: Mapper> Instruction<M> for Jmp {
    type Operand = u16;

    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>) {
        let target = AM::read(cpu);
        cpu.set_pc(target);
    }
}
