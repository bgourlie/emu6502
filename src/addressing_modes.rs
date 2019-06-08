use crate::{
    cpu::{Cpu, Mapper},
    util::to_u16,
};

pub trait AddressingMode<M: Mapper, O: Copy + Clone + std::fmt::Debug> {
    fn read_operand(cpu: &mut Cpu<M>) -> O;
    fn write(cpu: &mut Cpu<M>, _data: u8) {}
}

pub trait MemoryAddressing {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16;
}

pub struct Absolute;

impl MemoryAddressing for Absolute {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        cpu.fetch_pc16()
    }
}

impl<M: Mapper, A: MemoryAddressing> AddressingMode<M, ()> for A {
    fn read_operand(_cpu: &mut Cpu<M>) -> () {}

    fn write(cpu: &mut Cpu<M>, data: u8) {
        let target = A::fetch_target_addr(cpu);
        cpu.write(target, data);
    }
}

impl<M: Mapper> AddressingMode<M, u16> for Absolute {
    fn read_operand(cpu: &mut Cpu<M>) -> u16 {
        Self::fetch_target_addr(cpu)
    }
}

pub struct AbsoluteX;

impl MemoryAddressing for AbsoluteX {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let base_addr = cpu.fetch_pc16();
        base_addr.wrapping_add(u16::from(cpu.x))
    }
}

pub struct AbsoluteY;

impl MemoryAddressing for AbsoluteY {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let base_addr = cpu.fetch_pc16();
        base_addr.wrapping_add(u16::from(cpu.y))
    }
}

pub struct ZeroPage;

impl MemoryAddressing for ZeroPage {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        u16::from(cpu.fetch_pc())
    }
}

pub struct ZeroPageX;

impl MemoryAddressing for ZeroPageX {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        u16::from(cpu.fetch_pc().wrapping_add(cpu.x))
    }
}

pub struct ZeroPageY;

impl MemoryAddressing for ZeroPageY {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        u16::from(cpu.fetch_pc().wrapping_add(cpu.y))
    }
}

pub struct AbsoluteIndirect;

impl<M: Mapper> AddressingMode<M, u16> for AbsoluteIndirect {
    fn read_operand(cpu: &mut Cpu<M>) -> u16 {
        let indirect_target = cpu.fetch_pc16();
        let target_low = cpu.read(indirect_target);

        // recreate indirect jump bug in nmos 6502
        let target_high = if indirect_target & 0x00ff == 0x00ff {
            cpu.read(indirect_target & 0xff00)
        } else {
            cpu.read(indirect_target.wrapping_add(1))
        };

        to_u16(target_low, target_high)
    }
}

pub struct IndexedIndirect;

impl MemoryAddressing for IndexedIndirect {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let indirect_addr = cpu.fetch_pc();
        let base_addr = u16::from(indirect_addr.wrapping_add(cpu.x));
        cpu.read16(base_addr)
    }
}

pub struct IndirectIndexed;

impl MemoryAddressing for IndirectIndexed {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let indirect_addr = cpu.fetch_pc();
        let y = cpu.y;
        let base_addr = cpu.read16_zp(indirect_addr);
        base_addr.wrapping_add(u16::from(y))
    }
}
