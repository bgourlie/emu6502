use crate::{
    addressing_modes::*,
    opcodes::*,
    util::{from_u16, to_u16},
};

const FL_BREAK: u8 = 0b0001_0000;
const FL_CARRY: u8 = 0b0000_0001;
const FL_DECIMAL: u8 = 0b0000_1000;
const FL_INTERRUPT_DISABLE: u8 = 0b0000_0100;
const FL_OVERFLOW: u8 = 0b0100_0000;
const FL_SIGN: u8 = 0b1000_0000;
const FL_UNUSED: u8 = 0b0010_0000;
const FL_ZERO: u8 = 0b0000_0010;
const STACK_LOC: u16 = 0x100;

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
            0x00 => Brk::execute::<Implied>(self),
            0x01 => Ora::execute::<IndexedIndirect>(self),
            0x05 => Ora::execute::<ZeroPage>(self),
            0x09 => Ora::execute::<Immediate>(self),
            0x0d => Ora::execute::<Absolute>(self),
            0x06 => Asl::execute::<ZeroPage>(self),
            0x08 => Php::execute::<Implied>(self),
            0x0a => Asl::execute::<Accumulator>(self),
            0x0e => Asl::execute::<Absolute>(self),
            0x10 => Bpl::execute::<Relative>(self),
            0x11 => Ora::execute::<IndirectIndexed>(self),
            0x15 => Ora::execute::<ZeroPageX>(self),
            0x16 => Asl::execute::<ZeroPageX>(self),
            0x18 => Clc::execute::<Implied>(self),
            0x19 => Ora::execute::<AbsoluteY>(self),
            0x1d => Ora::execute::<AbsoluteX>(self),
            0x1e => Asl::execute::<AbsoluteX>(self),
            0x20 => Jsr::execute::<Absolute>(self),
            0x21 => And::execute::<IndexedIndirect>(self),
            0x24 => Bit::execute::<ZeroPage>(self),
            0x25 => And::execute::<ZeroPage>(self),
            0x26 => Rol::execute::<ZeroPage>(self),
            0x28 => Plp::execute::<Implied>(self),
            0x29 => And::execute::<Immediate>(self),
            0x30 => Bmi::execute::<Relative>(self),
            0x2a => Rol::execute::<Accumulator>(self),
            0x2c => Bit::execute::<Absolute>(self),
            0x2d => And::execute::<Absolute>(self),
            0x2e => Rol::execute::<Absolute>(self),
            0x31 => And::execute::<IndirectIndexed>(self),
            0x35 => And::execute::<ZeroPageX>(self),
            0x36 => Rol::execute::<ZeroPageX>(self),
            0x38 => Sec::execute::<Implied>(self),
            0x39 => And::execute::<AbsoluteY>(self),
            0x3d => And::execute::<AbsoluteX>(self),
            0x3e => Rol::execute::<AbsoluteX>(self),
            0x40 => Rti::execute::<Implied>(self),
            0x41 => Eor::execute::<IndexedIndirect>(self),
            0x45 => Eor::execute::<ZeroPage>(self),
            0x46 => Lsr::execute::<ZeroPage>(self),
            0x48 => Pha::execute::<Implied>(self),
            0x49 => Eor::execute::<Immediate>(self),
            0x4a => Lsr::execute::<Accumulator>(self),
            0x4e => Lsr::execute::<Absolute>(self),
            0x4c => Jmp::execute::<Absolute>(self),
            0x4d => Eor::execute::<Absolute>(self),
            0x50 => Bvc::execute::<Relative>(self),
            0x51 => Eor::execute::<IndirectIndexed>(self),
            0x55 => Eor::execute::<ZeroPageX>(self),
            0x56 => Lsr::execute::<ZeroPageX>(self),
            0x58 => Cli::execute::<Implied>(self),
            0x59 => Eor::execute::<AbsoluteY>(self),
            0x5d => Eor::execute::<AbsoluteX>(self),
            0x5e => Lsr::execute::<AbsoluteX>(self),
            0x60 => Rts::execute::<Implied>(self),
            0x61 => Adc::execute::<IndexedIndirect>(self),
            0x65 => Adc::execute::<ZeroPage>(self),
            0x66 => Ror::execute::<ZeroPage>(self),
            0x68 => Pla::execute::<Implied>(self),
            0x69 => Adc::execute::<Immediate>(self),
            0x70 => Bvs::execute::<Relative>(self),
            0x6a => Ror::execute::<Accumulator>(self),
            0x6c => Jmp::execute::<AbsoluteIndirect>(self),
            0x6d => Adc::execute::<Absolute>(self),
            0x6e => Ror::execute::<Absolute>(self),
            0x71 => Adc::execute::<IndirectIndexed>(self),
            0x75 => Adc::execute::<ZeroPageX>(self),
            0x76 => Ror::execute::<ZeroPageX>(self),
            0x78 => Sei::execute::<Implied>(self),
            0x79 => Adc::execute::<AbsoluteY>(self),
            0x7e => Ror::execute::<AbsoluteX>(self),
            0x7d => Adc::execute::<AbsoluteX>(self),
            0x81 => Sta::execute::<IndexedIndirect>(self),
            0x84 => Sty::execute::<ZeroPage>(self),
            0x85 => Sta::execute::<ZeroPage>(self),
            0x86 => Stx::execute::<ZeroPage>(self),
            0x88 => Dey::execute::<Implied>(self),
            0x8a => Txa::execute::<Implied>(self),
            0x8c => Sty::execute::<Absolute>(self),
            0x8d => Sta::execute::<Absolute>(self),
            0x8e => Stx::execute::<Absolute>(self),
            0x90 => Bcc::execute::<Relative>(self),
            0x91 => Sta::execute::<IndirectIndexed>(self),
            0x94 => Sty::execute::<ZeroPageX>(self),
            0x95 => Sta::execute::<ZeroPageX>(self),
            0x96 => Stx::execute::<ZeroPageY>(self),
            0x98 => Tya::execute::<Implied>(self),
            0x99 => Sta::execute::<AbsoluteY>(self),
            0x9a => Txs::execute::<Implied>(self),
            0x9d => Sta::execute::<AbsoluteX>(self),
            0xa0 => Ldy::execute::<Immediate>(self),
            0xa1 => Lda::execute::<IndirectIndexed>(self),
            0xa2 => Ldx::execute::<Immediate>(self),
            0xa4 => Ldy::execute::<ZeroPage>(self),
            0xa5 => Lda::execute::<ZeroPage>(self),
            0xa6 => Ldx::execute::<ZeroPage>(self),
            0xa8 => Tay::execute::<Implied>(self),
            0xa9 => Lda::execute::<Immediate>(self),
            0xaa => Tax::execute::<Implied>(self),
            0xac => Ldy::execute::<Absolute>(self),
            0xad => Lda::execute::<Absolute>(self),
            0xae => Ldx::execute::<Absolute>(self),
            0xb0 => Bcs::execute::<Relative>(self),
            0xb1 => Lda::execute::<IndirectIndexed>(self),
            0xb4 => Ldy::execute::<ZeroPageX>(self),
            0xb5 => Lda::execute::<ZeroPageX>(self),
            0xb6 => Ldx::execute::<ZeroPageY>(self),
            0xb8 => Clv::execute::<Implied>(self),
            0xb9 => Lda::execute::<AbsoluteY>(self),
            0xba => Tsx::execute::<Implied>(self),
            0xbc => Ldy::execute::<AbsoluteX>(self),
            0xbd => Lda::execute::<AbsoluteX>(self),
            0xbe => Ldx::execute::<AbsoluteY>(self),
            0xce => Dec::execute::<Absolute>(self),
            0xc6 => Dec::execute::<ZeroPage>(self),
            0xca => Dex::execute::<Implied>(self),
            0xc8 => Iny::execute::<Implied>(self),
            0xd0 => Bne::execute::<Relative>(self),
            0xd6 => Dec::execute::<ZeroPageX>(self),
            0xd8 => Cld::execute::<Implied>(self),
            0xde => Dec::execute::<AbsoluteX>(self),
            0xe1 => Sbc::execute::<IndexedIndirect>(self),
            0xe5 => Sbc::execute::<ZeroPage>(self),
            0xe6 => Inc::execute::<ZeroPage>(self),
            0xe8 => Inx::execute::<Implied>(self),
            0xe9 => Sbc::execute::<Immediate>(self),
            0xea => Nop::execute::<Implied>(self),
            0xee => Inc::execute::<Absolute>(self),
            0xed => Sbc::execute::<Absolute>(self),
            0xf0 => Beq::execute::<Relative>(self),
            0xf1 => Sbc::execute::<IndirectIndexed>(self),
            0xf5 => Sbc::execute::<ZeroPageX>(self),
            0xf6 => Inc::execute::<ZeroPageX>(self),
            0xf8 => Sed::execute::<Implied>(self),
            0xf9 => Sbc::execute::<AbsoluteY>(self),
            0xfd => Sbc::execute::<AbsoluteX>(self),
            0xfe => Inc::execute::<AbsoluteX>(self),
            _ => panic!("Unexpected opcode: {:0>2X}", opcode),
        }
    }

    pub fn acc(&self) -> u8 {
        self.acc
    }

    pub(crate) fn set_acc(&mut self, val: u8) {
        self.acc = val
    }

    pub(crate) fn set_acc_and_apply_flags(&mut self, val: u8) {
        self.apply_sign_and_zero_flags(val);
        self.acc = val;
    }

    pub(crate) fn set_pc(&mut self, addr: u16) {
        self.pc = addr;
    }

    pub fn sp(&self) -> u8 {
        self.sp
    }

    pub(crate) fn set_sp(&mut self, val: u8) {
        self.sp = val;
    }

    pub fn status(&self) -> u8 {
        self.status
    }

    pub fn x(&self) -> u8 {
        self.x
    }

    pub(crate) fn set_x(&mut self, val: u8) {
        self.x = val;
    }

    pub fn y(&self) -> u8 {
        self.y
    }

    pub(crate) fn set_y(&mut self, val: u8) {
        self.y = val;
    }

    pub fn carry(&self) -> bool {
        self.status & FL_CARRY != 0
    }

    pub(crate) fn set_carry(&mut self, val: bool) {
        if val {
            self.status |= FL_CARRY;
        } else {
            self.status &= !FL_CARRY;
        }
    }

    pub fn decimal_mode(&self) -> bool {
        self.status & FL_DECIMAL > 0
    }

    pub(crate) fn set_decimal_mode(&mut self, val: bool) {
        if val {
            self.status |= FL_DECIMAL;
        } else {
            self.status &= !FL_DECIMAL;
        }
    }

    pub fn interrupt_disable(&self) -> bool {
        self.status & FL_INTERRUPT_DISABLE > 0
    }

    pub(crate) fn set_interrupt_disable(&mut self, val: bool) {
        if val {
            self.status |= FL_INTERRUPT_DISABLE;
        } else {
            self.status &= !FL_INTERRUPT_DISABLE;
        }
    }

    pub fn overflow(&self) -> bool {
        self.status & FL_OVERFLOW != 0
    }

    pub(crate) fn set_overflow(&mut self, val: bool) {
        if val {
            self.status |= FL_OVERFLOW;
        } else {
            self.status &= !FL_OVERFLOW;
        }
    }

    pub fn pc(&self) -> u16 {
        self.pc
    }

    pub fn sign(&self) -> bool {
        self.status & FL_SIGN != 0
    }

    pub(crate) fn set_sign(&mut self, val: bool) {
        if val {
            self.status |= FL_SIGN;
        } else {
            self.status &= !FL_SIGN;
        }
    }

    pub fn set_status(&mut self, val: u8) {
        const FL_ALWAYS_SET: u8 = FL_UNUSED | FL_BREAK;
        self.status = val | FL_ALWAYS_SET;
    }

    pub fn zero(&self) -> bool {
        self.status & FL_ZERO != 0
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
        let data = self.mapper.peek_mut(self.pc);
        self.pc += 1;
        data
    }

    pub(crate) fn fetch_pc16(&mut self) -> u16 {
        let low = self.fetch_pc();
        let high = self.fetch_pc();
        to_u16(low, high)
    }

    pub(crate) fn read(&mut self, addr: u16) -> u8 {
        self.mapper.peek_mut(addr)
    }

    pub(crate) fn read16(&mut self, addr: u16) -> u16 {
        let low = self.mapper.peek_mut(addr);
        let high = self.mapper.peek_mut(addr.wrapping_add(1));
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

    pub(crate) fn push_stack(&mut self, value: u8) {
        let sp = u16::from(self.sp);
        self.mapper.poke(sp.wrapping_add(STACK_LOC), value);
        self.sp = self.sp.wrapping_sub(1);
    }

    pub(crate) fn push_stack16(&mut self, value: u16) {
        let (low, high) = from_u16(value);
        self.push_stack(high);
        self.push_stack(low);
    }

    pub(crate) fn pop_stack(&mut self) -> u8 {
        let sp = self.sp.wrapping_add(1);
        let val = self.read(u16::from(sp).wrapping_add(STACK_LOC));
        self.sp = sp;
        val
    }

    pub(crate) fn pop_stack16(&mut self) -> u16 {
        let low = self.pop_stack();
        let high = self.pop_stack();
        to_u16(low, high)
    }
}

pub trait Mapper {
    fn peek(&self, addr: u16) -> u8;
    fn peek_mut(&mut self, addr: u16) -> u8 {
        self.peek(addr)
    }
    fn poke(&mut self, addr: u16, value: u8);
}
