use crate::{addressing_modes::*, opcodes::*, util::to_u16};

pub struct Cpu<M: Mapper> {
    pub(crate) pc: u16,    // Program Counter
    pub(crate) sp: u8,     // Stack Pointer
    pub(crate) acc: u8,    // Accumulator
    pub(crate) x: u8,      // Index Register X
    pub(crate) y: u8,      // Index Register Y
    pub(crate) status: u8, // Processor Status Flags
    mapper: M,             // Memory Mapper
}

impl<M: Mapper> Cpu<M> {
    pub fn new(mapper: M) -> Self {
        Cpu {
            pc: 0,
            sp: 0,
            acc: 0,
            x: 0,
            y: 0,
            status: 0,
            mapper,
        }
    }

    pub fn step(&mut self) {
        let opcode = self.fetch_pc();

        match opcode {
            0x4c => Jmp::execute::<Absolute>(self),
            0x6c => Jmp::execute::<AbsoluteIndirect>(self),
            0x85 => Sta::execute::<ZeroPage>(self),
            0x95 => Sta::execute::<ZeroPageX>(self),
            0x81 => Sta::execute::<IndexedIndirect>(self),
            0x91 => Sta::execute::<IndirectIndexed>(self),
            0x8d => Sta::execute::<Absolute>(self),
            0x9d => Sta::execute::<AbsoluteX>(self),
            0x99 => Sta::execute::<AbsoluteY>(self),
            _ => panic!("Unexpected opcode: {:0>2X}", opcode),
        }
    }

    pub(crate) fn fetch_pc(&mut self) -> u8 {
        let data = self.mapper.peek(self.pc);
        self.pc += 1;
        data
    }

    pub(crate) fn fetch_pc16(&mut self) -> u16 {
        let low = self.fetch_pc();
        let high = self.fetch_pc();
        to_u16(low, high)
    }

    pub(crate) fn read(&mut self, addr: u16) -> u8 {
        self.mapper.peek(addr)
    }

    pub(crate) fn read16(&mut self, addr: u16) -> u16 {
        let low = self.mapper.peek(addr);
        let high = self.mapper.peek(addr.wrapping_add(1));
        to_u16(low, high)
    }

    pub(crate) fn read16_zp(&mut self, addr: u8) -> u16 {
        let low = self.read(u16::from(addr));
        let high = self.read(u16::from(addr.wrapping_add(1)));
        to_u16(low, high)
    }

    pub(crate) fn write(&mut self, addr: u16, data: u8) {
        self.mapper.poke(addr, data);
    }
}

pub trait Mapper {
    fn peek(&self, addr: u16) -> u8;
    fn poke(&mut self, addr: u16, value: u8);
}
