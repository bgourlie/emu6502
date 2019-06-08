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
        if cpu.decimal_mode() {
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

pub struct Clc;

impl<M: Mapper> Instruction<M> for Clc {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_carry(false);
    }
}

pub struct Cld;

impl<M: Mapper> Instruction<M> for Cld {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_decimal_mode(false);
    }
}

pub struct Cli;

impl<M: Mapper> Instruction<M> for Cli {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_interrupt_disable(false);
    }
}

pub struct Clv;

impl<M: Mapper> Instruction<M> for Clv {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_overflow(false);
    }
}

pub struct Dex;

impl<M: Mapper> Instruction<M> for Dex {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.x().wrapping_sub(1);
        cpu.set_x(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Dey;

impl<M: Mapper> Instruction<M> for Dey {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.y().wrapping_sub(1);
        cpu.set_y(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Inx;

impl<M: Mapper> Instruction<M> for Inx {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.x().wrapping_add(1);
        cpu.set_x(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Iny;

impl<M: Mapper> Instruction<M> for Iny {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.y().wrapping_add(1);
        cpu.set_y(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Lda;

impl<M: Mapper> Instruction<M> for Lda {
    type Operand = u8;

    fn execute<AM: AddressingMode<M, u8>>(cpu: &mut Cpu<M>) {
        let val = AM::read(cpu);
        cpu.set_acc_and_apply_flags(val);
    }
}

pub struct Ldx;

impl<M: Mapper> Instruction<M> for Ldx {
    type Operand = u8;

    fn execute<AM: AddressingMode<M, u8>>(cpu: &mut Cpu<M>) {
        let val = AM::read(cpu);
        cpu.set_x(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Ldy;

impl<M: Mapper> Instruction<M> for Ldy {
    type Operand = u8;

    fn execute<AM: AddressingMode<M, u8>>(cpu: &mut Cpu<M>) {
        let val = AM::read(cpu);
        cpu.set_y(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Nop;

impl<M: Mapper> Instruction<M> for Nop {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(_cpu: &mut Cpu<M>) {}
}

pub struct Pha;

impl<M: Mapper> Instruction<M> for Pha {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.push_stack(cpu.acc())
    }
}

pub struct Php;

impl<M: Mapper> Instruction<M> for Php {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let stat = cpu.status();
        cpu.push_stack(stat)
    }
}

pub struct Pla;

impl<M: Mapper> Instruction<M> for Pla {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.pop_stack();
        cpu.set_acc_and_apply_flags(val);
    }
}

pub struct Plp;

impl<M: Mapper> Instruction<M> for Plp {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.pop_stack();
        cpu.set_status(val);
    }
}

pub struct Rts;

impl<M: Mapper> Instruction<M> for Rts {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let pc = cpu.pop_stack16();
        cpu.set_pc(pc.wrapping_add(1));
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

pub struct Sec;

impl<M: Mapper> Instruction<M> for Sec {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_carry(true);
    }
}

pub struct Sed;

impl<M: Mapper> Instruction<M> for Sed {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_decimal_mode(true);
    }
}

pub struct Sei;

impl<M: Mapper> Instruction<M> for Sei {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_interrupt_disable(true);
    }
}

pub struct Sta;

impl<M: Mapper> Instruction<M> for Sta {
    type Operand = ();

    fn execute<AM: AddressingMode<M, Self::Operand>>(cpu: &mut Cpu<M>) {
        AM::write(cpu, cpu.acc())
    }
}

pub struct Tax;

impl<M: Mapper> Instruction<M> for Tax {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let x = cpu.acc();
        cpu.set_x(x);
        cpu.apply_sign_and_zero_flags(x);
    }
}

pub struct Tay;

impl<M: Mapper> Instruction<M> for Tay {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let y = cpu.acc();
        cpu.set_y(y);
        cpu.apply_sign_and_zero_flags(y);
    }
}

pub struct Tsx;

impl<M: Mapper> Instruction<M> for Tsx {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let x = cpu.sp();
        cpu.set_x(x);
        cpu.apply_sign_and_zero_flags(x);
    }
}

pub struct Txa;

impl<M: Mapper> Instruction<M> for Txa {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let acc = cpu.x();
        cpu.set_acc(acc);
        cpu.apply_sign_and_zero_flags(acc);
    }
}

pub struct Txs;

impl<M: Mapper> Instruction<M> for Txs {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        cpu.set_sp(cpu.x());
    }
}

pub struct Tya;

impl<M: Mapper> Instruction<M> for Tya {
    type Operand = ();

    fn execute<AM: AddressingMode<M, ()>>(cpu: &mut Cpu<M>) {
        let acc = cpu.y();
        cpu.set_acc(acc);
        cpu.apply_sign_and_zero_flags(acc);
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
