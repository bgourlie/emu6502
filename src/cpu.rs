use crate::{addressing_modes::*, opcodes::*, util::to_u16};

const FL_CARRY: u8 = 0b0000_0001;
const FL_DECIMAL: u8 = 0b0000_1000;
const FL_OVERFLOW: u8 = 0b0100_0000;
const FL_SIGN: u8 = 0b1000_0000;
const FL_ZERO: u8 = 0b0000_0010;

pub struct Cpu<M: Mapper> {
    pc: u16,    // Program Counter
    sp: u8,     // Stack Pointer
    acc: u8,    // Accumulator
    x: u8,      // Index Register X
    y: u8,      // Index Register Y
    status: u8, // Processor Status Flags
    mapper: M,  // Memory Mapper
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
            0x06 => Asl::execute::<ZeroPage>(self),
            0x0a => Asl::execute::<Accumulator>(self),
            0x0e => Asl::execute::<Absolute>(self),
            0x16 => Asl::execute::<ZeroPageX>(self),
            0x1e => Asl::execute::<AbsoluteX>(self),
            0x4c => Jmp::execute::<Absolute>(self),
            0x61 => Adc::execute::<IndexedIndirect>(self),
            0x65 => Adc::execute::<ZeroPage>(self),
            0x69 => Adc::execute::<Immediate>(self),
            0x6d => Adc::execute::<Absolute>(self),
            0x71 => Adc::execute::<IndirectIndexed>(self),
            0x75 => Adc::execute::<ZeroPageX>(self),
            0x79 => Adc::execute::<AbsoluteY>(self),
            0x7d => Adc::execute::<AbsoluteX>(self),
            0x6c => Jmp::execute::<AbsoluteIndirect>(self),
            0x85 => Sta::execute::<ZeroPage>(self),
            0x95 => Sta::execute::<ZeroPageX>(self),
            0x81 => Sta::execute::<IndexedIndirect>(self),
            0x91 => Sta::execute::<IndirectIndexed>(self),
            0x8d => Sta::execute::<Absolute>(self),
            0x9d => Sta::execute::<AbsoluteX>(self),
            0x99 => Sta::execute::<AbsoluteY>(self),
            0xa0 => Ldy::execute::<Immediate>(self),
            0xa1 => Lda::execute::<IndirectIndexed>(self),
            0xa2 => Ldx::execute::<Immediate>(self),
            0xa4 => Ldy::execute::<ZeroPage>(self),
            0xa5 => Lda::execute::<ZeroPage>(self),
            0xa6 => Ldx::execute::<ZeroPage>(self),
            0xa9 => Lda::execute::<Immediate>(self),
            0xac => Ldy::execute::<Absolute>(self),
            0xad => Lda::execute::<Absolute>(self),
            0xae => Ldx::execute::<Absolute>(self),
            0xb1 => Lda::execute::<IndirectIndexed>(self),
            0xb4 => Ldy::execute::<ZeroPageX>(self),
            0xb5 => Lda::execute::<ZeroPageX>(self),
            0xb6 => Ldx::execute::<ZeroPageY>(self),
            0xb9 => Lda::execute::<AbsoluteY>(self),
            0xbc => Ldy::execute::<AbsoluteX>(self),
            0xbd => Lda::execute::<AbsoluteX>(self),
            0xbe => Ldx::execute::<AbsoluteY>(self),
            0xe1 => Sbc::execute::<IndexedIndirect>(self),
            0xe5 => Sbc::execute::<ZeroPage>(self),
            0xe9 => Sbc::execute::<Immediate>(self),
            0xed => Sbc::execute::<Absolute>(self),
            0xf1 => Sbc::execute::<IndirectIndexed>(self),
            0xf5 => Sbc::execute::<ZeroPageX>(self),
            0xf9 => Sbc::execute::<AbsoluteY>(self),
            0xfd => Sbc::execute::<AbsoluteX>(self),
            _ => panic!("Unexpected opcode: {:0>2X}", opcode),
        }
    }

    pub fn acc(&self) -> u8 {
        self.acc
    }

    pub fn set_acc(&mut self, val: u8) {
        self.acc = val
    }

    pub fn set_acc_and_apply_flags(&mut self, val: u8) {
        self.apply_sign_and_zero_flags(val);
        self.acc = val;
    }

    pub fn set_pc(&mut self, addr: u16) {
        self.pc = addr;
    }

    pub fn x(&self) -> u8 {
        self.x
    }

    pub fn set_x(&mut self, val: u8) {
        self.x = val;
    }

    pub fn y(&self) -> u8 {
        self.y
    }

    pub fn set_y(&mut self, val: u8) {
        self.y = val;
    }

    pub fn carry_flag(&self) -> bool {
        self.status & FL_CARRY != 0
    }

    pub(crate) fn set_carry(&mut self, val: bool) {
        if val {
            self.status |= FL_CARRY;
        } else {
            self.status &= !FL_CARRY;
        }
    }

    pub(crate) fn decimal_flag(&self) -> bool {
        self.status & FL_DECIMAL > 0
    }

    pub fn set_overflow(&mut self, val: bool) {
        if val {
            self.status |= FL_OVERFLOW;
        } else {
            self.status &= !FL_OVERFLOW;
        }
    }

    pub(crate) fn set_sign(&mut self, val: bool) {
        if val {
            self.status |= FL_SIGN;
        } else {
            self.status &= !FL_SIGN;
        }
    }

    pub(crate) fn set_zero(&mut self, val: bool) {
        if val {
            self.status |= FL_ZERO;
        } else {
            self.status &= !FL_ZERO;
        }
    }

    pub(crate) fn apply_sign_and_zero_flags(&mut self, val: u8) {
        self.set_sign(val & 0x80 > 0);
        self.set_zero(val == 0);
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
