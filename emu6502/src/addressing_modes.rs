use {
    crate::{
        cpu::{Cpu, Mapper},
        util::to_u16,
    },
    std::fmt::Debug,
};

pub trait AddressingMode<M: Mapper, R: Copy + Clone + Debug, W: Copy + Clone + Debug> {
    fn read(cpu: &mut Cpu<M>) -> R;
    fn write(_cpu: &mut Cpu<M>, _data: W) { unimplemented!() }
    fn read_modify_write<F>(cpu: &mut Cpu<M>, modifier: F)
    where
        F: FnOnce(&mut Cpu<M>, R) -> W,
    {
        let input = Self::read(cpu);
        let modified = modifier(cpu, input);
        Self::write(cpu, modified);
    }
}

pub trait MemoryAddressing {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16;

    fn read<M: Mapper>(cpu: &mut Cpu<M>) -> u8 {
        let addr = Self::fetch_target_addr(cpu);
        cpu.read(addr)
    }
}

impl<M: Mapper, A: MemoryAddressing> AddressingMode<M, (u16, u8), (u16, u8)> for A {
    fn read(cpu: &mut Cpu<M>) -> (u16, u8) {
        let addr = A::fetch_target_addr(cpu);
        let val = cpu.read(addr);
        (addr, val)
    }

    fn write(cpu: &mut Cpu<M>, data: (u16, u8)) {
        let (target, data) = data;
        cpu.write(target, data);
    }
}

impl<M: Mapper, A: MemoryAddressing> AddressingMode<M, u8, ()> for A {
    fn read(cpu: &mut Cpu<M>) -> u8 {
        let addr = A::fetch_target_addr(cpu);
        cpu.read(addr)
    }

    fn write(_cpu: &mut Cpu<M>, _data: ()) {}
}

impl<M: Mapper, A: MemoryAddressing> AddressingMode<M, (), u8> for A {
    fn read(_cpu: &mut Cpu<M>) {}

    fn write(cpu: &mut Cpu<M>, data: u8) {
        let target = A::fetch_target_addr(cpu);
        cpu.write(target, data);
    }
}

pub struct Absolute;

impl MemoryAddressing for Absolute {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        cpu.fetch_pc16()
    }
}

impl<M: Mapper> AddressingMode<M, u16, ()> for Absolute {
    fn read(cpu: &mut Cpu<M>) -> u16 {
        Self::fetch_target_addr(cpu)
    }
}

pub struct AbsoluteX;

impl MemoryAddressing for AbsoluteX {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let base_addr = cpu.fetch_pc16();
        base_addr.wrapping_add(u16::from(cpu.x()))
    }
}

pub struct AbsoluteY;

impl MemoryAddressing for AbsoluteY {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let base_addr = cpu.fetch_pc16();
        base_addr.wrapping_add(u16::from(cpu.y()))
    }
}

pub struct Accumulator;

impl<M: Mapper> AddressingMode<M, u8, u8> for Accumulator {
    fn read(cpu: &mut Cpu<M>) -> u8 {
        cpu.acc()
    }

    fn write(cpu: &mut Cpu<M>, data: u8) {
        cpu.set_acc(data);
    }
}

impl<M: Mapper> AddressingMode<M, (u16, u8), (u16, u8)> for Accumulator {
    fn read(cpu: &mut Cpu<M>) -> (u16, u8) {
        (0, cpu.acc())
    }

    fn write(cpu: &mut Cpu<M>, data: (u16, u8)) {
        let (_, data) = data;
        cpu.set_acc(data);
    }
}

pub struct Immediate;

impl<M: Mapper> AddressingMode<M, u8, ()> for Immediate {
    fn read(cpu: &mut Cpu<M>) -> u8 {
        cpu.fetch_pc()
    }
}

pub struct Implied;

impl<M: Mapper> AddressingMode<M, (), ()> for Implied {
    fn read(_cpu: &mut Cpu<M>) {}
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
        u16::from(cpu.fetch_pc().wrapping_add(cpu.x()))
    }
}

pub struct ZeroPageY;

impl MemoryAddressing for ZeroPageY {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        u16::from(cpu.fetch_pc().wrapping_add(cpu.y()))
    }
}

pub struct AbsoluteIndirect;

impl<M: Mapper> AddressingMode<M, u16, ()> for AbsoluteIndirect {
    fn read(cpu: &mut Cpu<M>) -> u16 {
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
        let base_addr = u16::from(indirect_addr.wrapping_add(cpu.x()));
        cpu.read16(base_addr)
    }
}

pub struct IndirectIndexed;

impl MemoryAddressing for IndirectIndexed {
    fn fetch_target_addr<M: Mapper>(cpu: &mut Cpu<M>) -> u16 {
        let indirect_addr = cpu.fetch_pc();
        let y = cpu.y();
        let base_addr = cpu.read16_zp(indirect_addr);
        base_addr.wrapping_add(u16::from(y))
    }
}

pub struct Relative;

impl<M: Mapper> AddressingMode<M, i8, ()> for Relative {
    fn read(cpu: &mut Cpu<M>) -> i8 {
        cpu.fetch_pc() as i8
    }
}
