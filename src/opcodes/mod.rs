#[cfg(test)]
mod tests;

use {
    crate::{
        addressing_modes::*,
        cpu::{Cpu, Mapper},
    },
    std::fmt::Debug,
};

pub trait Instruction<M: Mapper, R: Copy + Clone + Debug, W: Copy + Clone + Debug> {
    fn execute<AM: AddressingMode<M, R, W>>(cpu: &mut Cpu<M>);
}

pub trait Branch<M: Mapper>: Instruction<M, i8, ()> {
    fn condition(cpu: &Cpu<M>) -> bool;

    fn branch<AM: AddressingMode<M, i8, ()>>(cpu: &mut Cpu<M>) {
        if Self::condition(cpu) {
            let rel_addr = AM::read(cpu);
            cpu.set_pc((i32::from(cpu.pc()) + i32::from(rel_addr)) as u16);
        }
    }
}

impl<M: Mapper, B: Branch<M>> Instruction<M, i8, ()> for B {
    fn execute<AM: AddressingMode<M, i8, ()>>(cpu: &mut Cpu<M>) {
        B::branch::<AM>(cpu)
    }
}

pub struct Adc;

impl Adc {
    fn adc<M: Mapper>(cpu: &mut Cpu<M>, lhs: u8, rhs: u8) {
        if cpu.decimal_mode() {
            panic!("Attempted decimal mode arithmetic");
        } else {
            // See http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            let carry = if cpu.carry() { 1 } else { 0 };

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

impl<M: Mapper> Instruction<M, u8, ()> for Adc {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = AM::read(cpu);
        Self::adc(cpu, lhs, rhs);
    }
}

pub struct And;

impl<M: Mapper> Instruction<M, u8, ()> for And {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = AM::read(cpu);
        let res = lhs & rhs;
        cpu.set_acc_and_apply_flags(res);
    }
}

pub struct Asl;

impl<M: Mapper> Instruction<M, (u16, u8), (u16, u8)> for Asl {
    fn execute<AM: AddressingMode<M, (u16, u8), (u16, u8)>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, (addr, val)| {
            let carry = (val & 0b1000_0000) > 0;
            let res = val << 1;
            cpu.set_carry(carry);
            cpu.apply_sign_and_zero_flags(res);
            (addr, res)
        });
    }
}

pub struct Bit;

impl<M: Mapper> Instruction<M, u8, ()> for Bit {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = AM::read(cpu);
        let res = lhs & rhs;

        cpu.set_zero(res == 0);
        cpu.set_overflow(rhs & 0x40 != 0);
        cpu.set_sign(rhs & 0x80 > 0);
    }
}

pub struct Bcc;

impl<M: Mapper> Branch<M> for Bcc {
    fn condition(cpu: &Cpu<M>) -> bool {
        !cpu.carry()
    }
}

pub struct Bcs;

impl<M: Mapper> Branch<M> for Bcs {
    fn condition(cpu: &Cpu<M>) -> bool {
        cpu.carry()
    }
}

pub struct Beq;

impl<M: Mapper> Branch<M> for Beq {
    fn condition(cpu: &Cpu<M>) -> bool {
        cpu.zero()
    }
}

pub struct Bmi;

impl<M: Mapper> Branch<M> for Bmi {
    fn condition(cpu: &Cpu<M>) -> bool {
        cpu.sign()
    }
}

pub struct Bne;

impl<M: Mapper> Branch<M> for Bne {
    fn condition(cpu: &Cpu<M>) -> bool {
        !cpu.zero()
    }
}

pub struct Bpl;

impl<M: Mapper> Branch<M> for Bpl {
    fn condition(cpu: &Cpu<M>) -> bool {
        !cpu.sign()
    }
}

pub struct Bvc;

impl<M: Mapper> Branch<M> for Bvc {
    fn condition(cpu: &Cpu<M>) -> bool {
        !cpu.overflow()
    }
}

pub struct Bvs;

impl<M: Mapper> Branch<M> for Bvs {
    fn condition(cpu: &Cpu<M>) -> bool {
        cpu.overflow()
    }
}

pub struct Clc;

impl<M: Mapper> Instruction<M, (), ()> for Clc {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_carry(false);
    }
}

pub struct Cld;

impl<M: Mapper> Instruction<M, (), ()> for Cld {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_decimal_mode(false);
    }
}

pub struct Cli;

impl<M: Mapper> Instruction<M, (), ()> for Cli {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_interrupt_disable(false);
    }
}

pub struct Clv;

impl<M: Mapper> Instruction<M, (), ()> for Clv {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_overflow(false);
    }
}

pub struct Dec;

impl<M: Mapper> Instruction<M, (u16, u8), (u16, u8)> for Dec {
    fn execute<AM: AddressingMode<M, (u16, u8), (u16, u8)>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, (addr, val)| {
            let res = val.wrapping_sub(1);
            cpu.apply_sign_and_zero_flags(res);
            (addr, res)
        })
    }
}

pub struct Dex;

impl<M: Mapper> Instruction<M, (), ()> for Dex {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.x().wrapping_sub(1);
        cpu.set_x(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Dey;

impl<M: Mapper> Instruction<M, (), ()> for Dey {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.y().wrapping_sub(1);
        cpu.set_y(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Eor;

impl<M: Mapper> Instruction<M, u8, ()> for Eor {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let rhs = AM::read(cpu);
        let lhs = cpu.acc();
        let res = lhs ^ rhs;
        cpu.set_acc_and_apply_flags(res);
    }
}

pub struct Inc;

impl<M: Mapper> Instruction<M, (u16, u8), (u16, u8)> for Inc {
    fn execute<AM: AddressingMode<M, (u16, u8), (u16, u8)>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, (addr, val)| {
            let res = val.wrapping_add(1);
            cpu.apply_sign_and_zero_flags(res);
            (addr, res)
        })
    }
}

pub struct Inx;

impl<M: Mapper> Instruction<M, (), ()> for Inx {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.x().wrapping_add(1);
        cpu.set_x(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Iny;

impl<M: Mapper> Instruction<M, (), ()> for Iny {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.y().wrapping_add(1);
        cpu.set_y(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Lda;

impl<M: Mapper> Instruction<M, u8, ()> for Lda {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let val = AM::read(cpu);
        cpu.set_acc_and_apply_flags(val);
    }
}

pub struct Ldx;

impl<M: Mapper> Instruction<M, u8, ()> for Ldx {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let val = AM::read(cpu);
        cpu.set_x(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Ldy;

impl<M: Mapper> Instruction<M, u8, ()> for Ldy {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let val = AM::read(cpu);
        cpu.set_y(val);
        cpu.apply_sign_and_zero_flags(val);
    }
}

pub struct Lsr;

impl<M: Mapper> Instruction<M, (u16, u8), (u16, u8)> for Lsr {
    fn execute<AM: AddressingMode<M, (u16, u8), (u16, u8)>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, (addr, val)| {
            let carry = (val & 0x1) > 0;
            let res = val >> 1 ;
            cpu.set_carry(carry);
            cpu.apply_sign_and_zero_flags(res);
            (addr, res)
        });
    }
}

pub struct Nop;

impl<M: Mapper> Instruction<M, (), ()> for Nop {
    fn execute<AM: AddressingMode<M, (), ()>>(_cpu: &mut Cpu<M>) {}
}

pub struct Ora;

impl<M: Mapper> Instruction<M, u8, ()> for Ora {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = AM::read(cpu);
        let res = lhs | rhs;
        cpu.set_acc_and_apply_flags(res);
    }
}

pub struct Pha;

impl<M: Mapper> Instruction<M, (), ()> for Pha {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.push_stack(cpu.acc())
    }
}

pub struct Php;

impl<M: Mapper> Instruction<M, (), ()> for Php {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let stat = cpu.status();
        cpu.push_stack(stat)
    }
}

pub struct Pla;

impl<M: Mapper> Instruction<M, (), ()> for Pla {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.pop_stack();
        cpu.set_acc_and_apply_flags(val);
    }
}

pub struct Plp;

impl<M: Mapper> Instruction<M, (), ()> for Plp {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let val = cpu.pop_stack();
        cpu.set_status(val);
    }
}

pub struct Rol;

impl<M: Mapper> Instruction<M, (u16, u8), (u16, u8)> for Rol {
    fn execute<AM: AddressingMode<M, (u16, u8), (u16, u8)>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, (addr, val)| {
            let carry_set = cpu.carry();
            let carry = (val & 0x80) != 0;
            let res = if carry_set {
                (val << 1) | 0x1
            } else {
                val << 1
            };
            cpu.set_carry(carry);
            cpu.apply_sign_and_zero_flags(res);
            (addr, res)
        });
    }
}

pub struct Ror;

impl<M: Mapper> Instruction<M, (u16, u8), (u16, u8)> for Ror {
    fn execute<AM: AddressingMode<M, (u16, u8), (u16, u8)>>(cpu: &mut Cpu<M>) {
        AM::read_modify_write(cpu, |cpu, (addr, val)| {
            let carry_set = cpu.carry();
            let carry = (val & 0x1) != 0;
            let res = if carry_set {
                (val >> 1) | 0x80
            } else {
                val >> 1
            };
            cpu.set_carry(carry);
            cpu.apply_sign_and_zero_flags(res);
            (addr, res)
        });
    }
}

pub struct Rts;

impl<M: Mapper> Instruction<M, (), ()> for Rts {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let pc = cpu.pop_stack16();
        cpu.set_pc(pc.wrapping_add(1));
    }
}

pub struct Sbc;

impl<M: Mapper> Instruction<M, u8, ()> for Sbc {
    fn execute<AM: AddressingMode<M, u8, ()>>(cpu: &mut Cpu<M>) {
        let lhs = cpu.acc();
        let rhs = !AM::read(cpu);
        Adc::adc(cpu, lhs, rhs)
    }
}

pub struct Sec;

impl<M: Mapper> Instruction<M, (), ()> for Sec {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_carry(true);
    }
}

pub struct Sed;

impl<M: Mapper> Instruction<M, (), ()> for Sed {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_decimal_mode(true);
    }
}

pub struct Sei;

impl<M: Mapper> Instruction<M, (), ()> for Sei {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_interrupt_disable(true);
    }
}

pub struct Sta;

impl<M: Mapper> Instruction<M, (), u8> for Sta {
    fn execute<AM: AddressingMode<M, (), u8>>(cpu: &mut Cpu<M>) {
        AM::write(cpu, cpu.acc())
    }
}

pub struct Tax;

impl<M: Mapper> Instruction<M, (), ()> for Tax {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let x = cpu.acc();
        cpu.set_x(x);
        cpu.apply_sign_and_zero_flags(x);
    }
}

pub struct Tay;

impl<M: Mapper> Instruction<M, (), ()> for Tay {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let y = cpu.acc();
        cpu.set_y(y);
        cpu.apply_sign_and_zero_flags(y);
    }
}

pub struct Tsx;

impl<M: Mapper> Instruction<M, (), ()> for Tsx {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let x = cpu.sp();
        cpu.set_x(x);
        cpu.apply_sign_and_zero_flags(x);
    }
}

pub struct Txa;

impl<M: Mapper> Instruction<M, (), ()> for Txa {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let acc = cpu.x();
        cpu.set_acc(acc);
        cpu.apply_sign_and_zero_flags(acc);
    }
}

pub struct Txs;

impl<M: Mapper> Instruction<M, (), ()> for Txs {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        cpu.set_sp(cpu.x());
    }
}

pub struct Tya;

impl<M: Mapper> Instruction<M, (), ()> for Tya {
    fn execute<AM: AddressingMode<M, (), ()>>(cpu: &mut Cpu<M>) {
        let acc = cpu.y();
        cpu.set_acc(acc);
        cpu.apply_sign_and_zero_flags(acc);
    }
}

pub struct Jmp;

impl<M: Mapper> Instruction<M, u16, ()> for Jmp {
    fn execute<AM: AddressingMode<M, u16, ()>>(cpu: &mut Cpu<M>) {
        let target = AM::read(cpu);
        cpu.set_pc(target);
    }
}
