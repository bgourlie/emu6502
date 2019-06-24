#![feature(seek_convenience)]

use {
    bit_set::BitSet,
    byteorder::{LittleEndian, ReadBytesExt},
    failure::{Error, Fail},
    log::info,
    std::io::{Seek, SeekFrom},
};

const OPCODES: [Option<(Opcode, AddressingMode)>; 0x100] = [
    Some((Opcode::Brk, AddressingMode::Implied)),
    Some((Opcode::Ora, AddressingMode::IndexedIndirect)),
    None,
    None,
    None,
    Some((Opcode::Ora, AddressingMode::ZeroPage)),
    Some((Opcode::Asl, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Php, AddressingMode::Implied)),
    Some((Opcode::Ora, AddressingMode::Immediate)),
    Some((Opcode::Asl, AddressingMode::Accumulator)),
    None,
    None,
    Some((Opcode::Ora, AddressingMode::Absolute)),
    Some((Opcode::Asl, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bpl, AddressingMode::Relative)),
    Some((Opcode::Ora, AddressingMode::IndirectIndexed)),
    None,
    None,
    None,
    Some((Opcode::Ora, AddressingMode::ZeroPageX)),
    Some((Opcode::Asl, AddressingMode::ZeroPageX)),
    None,
    Some((Opcode::Clc, AddressingMode::Implied)),
    Some((Opcode::Ora, AddressingMode::AbsoluteY)),
    None,
    None,
    None,
    Some((Opcode::Ora, AddressingMode::AbsoluteX)),
    Some((Opcode::Asl, AddressingMode::AbsoluteX)),
    None,
    Some((Opcode::Jsr, AddressingMode::Absolute)),
    Some((Opcode::And, AddressingMode::IndexedIndirect)),
    None,
    None,
    Some((Opcode::Bit, AddressingMode::ZeroPage)),
    Some((Opcode::And, AddressingMode::ZeroPage)),
    Some((Opcode::Rol, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Plp, AddressingMode::Implied)),
    Some((Opcode::And, AddressingMode::Immediate)),
    Some((Opcode::Rol, AddressingMode::Accumulator)),
    None,
    Some((Opcode::Bit, AddressingMode::Absolute)),
    Some((Opcode::And, AddressingMode::Absolute)),
    Some((Opcode::Rol, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bmi, AddressingMode::Relative)),
    Some((Opcode::And, AddressingMode::IndirectIndexed)),
    None,
    None,
    None,
    Some((Opcode::And, AddressingMode::ZeroPageX)),
    Some((Opcode::Rol, AddressingMode::ZeroPageX)),
    None,
    Some((Opcode::Sec, AddressingMode::Implied)),
    Some((Opcode::And, AddressingMode::AbsoluteY)),
    None,
    None,
    None,
    Some((Opcode::And, AddressingMode::AbsoluteX)),
    Some((Opcode::Rol, AddressingMode::AbsoluteX)),
    None,
    Some((Opcode::Rti, AddressingMode::Implied)),
    Some((Opcode::Eor, AddressingMode::IndexedIndirect)),
    None,
    None,
    None,
    Some((Opcode::Eor, AddressingMode::ZeroPage)),
    Some((Opcode::Lsr, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Pha, AddressingMode::Implied)),
    Some((Opcode::Eor, AddressingMode::Immediate)),
    Some((Opcode::Lsr, AddressingMode::Accumulator)),
    None,
    Some((Opcode::Jmp, AddressingMode::Absolute)),
    Some((Opcode::Eor, AddressingMode::Absolute)),
    Some((Opcode::Lsr, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bvc, AddressingMode::Relative)),
    Some((Opcode::Eor, AddressingMode::IndirectIndexed)),
    None,
    None,
    None,
    Some((Opcode::Eor, AddressingMode::ZeroPageX)),
    Some((Opcode::Lsr, AddressingMode::ZeroPageX)),
    None,
    Some((Opcode::Cli, AddressingMode::Implied)),
    Some((Opcode::Eor, AddressingMode::AbsoluteY)),
    None,
    None,
    None,
    Some((Opcode::Eor, AddressingMode::AbsoluteX)),
    Some((Opcode::Lsr, AddressingMode::AbsoluteX)),
    None,
    Some((Opcode::Rts, AddressingMode::Implied)),
    Some((Opcode::Adc, AddressingMode::IndexedIndirect)),
    None,
    None,
    None,
    Some((Opcode::Adc, AddressingMode::ZeroPage)),
    Some((Opcode::Ror, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Pla, AddressingMode::Implied)),
    Some((Opcode::Adc, AddressingMode::Immediate)),
    Some((Opcode::Ror, AddressingMode::Accumulator)),
    None,
    Some((Opcode::Jmp, AddressingMode::Indirect)),
    Some((Opcode::Adc, AddressingMode::Absolute)),
    Some((Opcode::Ror, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bvs, AddressingMode::Relative)),
    Some((Opcode::Adc, AddressingMode::IndirectIndexed)),
    None,
    None,
    None,
    Some((Opcode::Adc, AddressingMode::ZeroPageX)),
    Some((Opcode::Ror, AddressingMode::ZeroPageX)),
    None,
    Some((Opcode::Sei, AddressingMode::Implied)),
    Some((Opcode::Adc, AddressingMode::AbsoluteY)),
    None,
    None,
    None,
    Some((Opcode::Adc, AddressingMode::AbsoluteX)),
    Some((Opcode::Ror, AddressingMode::AbsoluteX)),
    None,
    None,
    Some((Opcode::Sta, AddressingMode::IndexedIndirect)),
    None,
    None,
    Some((Opcode::Sty, AddressingMode::ZeroPage)),
    Some((Opcode::Sta, AddressingMode::ZeroPage)),
    Some((Opcode::Stx, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Dey, AddressingMode::Implied)),
    None,
    Some((Opcode::Txa, AddressingMode::Implied)),
    None,
    Some((Opcode::Sty, AddressingMode::Absolute)),
    Some((Opcode::Sta, AddressingMode::Absolute)),
    Some((Opcode::Stx, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bcc, AddressingMode::Relative)),
    Some((Opcode::Sta, AddressingMode::IndirectIndexed)),
    None,
    None,
    Some((Opcode::Sty, AddressingMode::ZeroPageX)),
    Some((Opcode::Sta, AddressingMode::ZeroPageX)),
    Some((Opcode::Stx, AddressingMode::ZeroPageY)),
    None,
    Some((Opcode::Tya, AddressingMode::Implied)),
    Some((Opcode::Sta, AddressingMode::AbsoluteY)),
    Some((Opcode::Txs, AddressingMode::Implied)),
    None,
    None,
    Some((Opcode::Sta, AddressingMode::AbsoluteX)),
    None,
    None,
    Some((Opcode::Ldy, AddressingMode::Immediate)),
    Some((Opcode::Lda, AddressingMode::IndexedIndirect)),
    Some((Opcode::Ldx, AddressingMode::Immediate)),
    None,
    Some((Opcode::Ldy, AddressingMode::ZeroPage)),
    Some((Opcode::Lda, AddressingMode::ZeroPage)),
    Some((Opcode::Ldx, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Tay, AddressingMode::Implied)),
    Some((Opcode::Lda, AddressingMode::Immediate)),
    Some((Opcode::Tax, AddressingMode::Implied)),
    None,
    Some((Opcode::Ldy, AddressingMode::Absolute)),
    Some((Opcode::Lda, AddressingMode::Absolute)),
    Some((Opcode::Ldx, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bcs, AddressingMode::Relative)),
    Some((Opcode::Lda, AddressingMode::IndirectIndexed)),
    None,
    None,
    Some((Opcode::Ldy, AddressingMode::ZeroPageX)),
    Some((Opcode::Lda, AddressingMode::ZeroPageX)),
    Some((Opcode::Ldx, AddressingMode::ZeroPageY)),
    None,
    Some((Opcode::Clv, AddressingMode::Implied)),
    Some((Opcode::Lda, AddressingMode::AbsoluteY)),
    Some((Opcode::Tsx, AddressingMode::Implied)),
    None,
    Some((Opcode::Ldy, AddressingMode::AbsoluteX)),
    Some((Opcode::Lda, AddressingMode::AbsoluteX)),
    Some((Opcode::Ldx, AddressingMode::AbsoluteY)),
    None,
    Some((Opcode::Cpy, AddressingMode::Immediate)),
    Some((Opcode::Cmp, AddressingMode::IndexedIndirect)),
    None,
    None,
    Some((Opcode::Cpy, AddressingMode::ZeroPage)),
    Some((Opcode::Cmp, AddressingMode::ZeroPage)),
    Some((Opcode::Dec, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Iny, AddressingMode::Implied)),
    Some((Opcode::Cmp, AddressingMode::Immediate)),
    Some((Opcode::Dex, AddressingMode::Implied)),
    None,
    Some((Opcode::Cpy, AddressingMode::Absolute)),
    Some((Opcode::Cmp, AddressingMode::Absolute)),
    Some((Opcode::Dec, AddressingMode::Absolute)),
    None,
    Some((Opcode::Bne, AddressingMode::Relative)),
    Some((Opcode::Cmp, AddressingMode::IndirectIndexed)),
    None,
    None,
    None,
    Some((Opcode::Cmp, AddressingMode::ZeroPageX)),
    Some((Opcode::Dec, AddressingMode::ZeroPageX)),
    None,
    Some((Opcode::Cld, AddressingMode::Implied)),
    Some((Opcode::Cmp, AddressingMode::AbsoluteY)),
    None,
    None,
    None,
    Some((Opcode::Cmp, AddressingMode::AbsoluteX)),
    Some((Opcode::Dec, AddressingMode::AbsoluteX)),
    None,
    Some((Opcode::Cpx, AddressingMode::Immediate)),
    Some((Opcode::Sbc, AddressingMode::IndexedIndirect)),
    None,
    None,
    Some((Opcode::Cpx, AddressingMode::ZeroPage)),
    Some((Opcode::Sbc, AddressingMode::ZeroPage)),
    Some((Opcode::Inc, AddressingMode::ZeroPage)),
    None,
    Some((Opcode::Inx, AddressingMode::Implied)),
    Some((Opcode::Sbc, AddressingMode::Immediate)),
    Some((Opcode::Nop, AddressingMode::Implied)),
    None,
    Some((Opcode::Cpx, AddressingMode::Absolute)),
    Some((Opcode::Sbc, AddressingMode::Absolute)),
    Some((Opcode::Inc, AddressingMode::Absolute)),
    None,
    Some((Opcode::Beq, AddressingMode::Relative)),
    Some((Opcode::Sbc, AddressingMode::IndirectIndexed)),
    None,
    None,
    None,
    Some((Opcode::Sbc, AddressingMode::ZeroPageX)),
    Some((Opcode::Inc, AddressingMode::ZeroPageX)),
    None,
    Some((Opcode::Sed, AddressingMode::Implied)),
    Some((Opcode::Sbc, AddressingMode::AbsoluteY)),
    None,
    None,
    None,
    Some((Opcode::Sbc, AddressingMode::AbsoluteX)),
    Some((Opcode::Inc, AddressingMode::AbsoluteX)),
    None,
];

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AddressingMode {
    Accumulator,
    Immediate,
    Implied,
    Relative,
    Absolute,
    ZeroPage,
    Indirect,
    AbsoluteX,
    AbsoluteY,
    ZeroPageX,
    ZeroPageY,
    IndexedIndirect,
    IndirectIndexed,
}

#[derive(Debug, Fail)]
enum DisassemblyError {
    #[fail(display = "Branch out of bounds: {:X?}", branch_location)]
    BranchOutOfBounds { branch_location: u16 },

    #[fail(display = "Offset out of bounds")]
    OffsetOutOfBounds,

    #[fail(display = "Illegal opcode: {:X?}", opcode_location)]
    IllegalOpcode { opcode_location: u16 },
}

/// Similar to AddressingMode, but includes the decoded values
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operand {
    Accumulator,
    Immediate(u8),
    Implied,
    Relative(i8),
    Absolute(u16),
    ZeroPage(u8),
    Indirect(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    ZeroPageX(u8),
    ZeroPageY(u8),
    IndexedIndirect(u8),
    IndirectIndexed(u8),
    BreakByte(u8),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Opcode {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
    opcode: Opcode,
    operand: Operand,
}

impl Instruction {
    fn new(opcode: Opcode, operand: Operand) -> Self {
        Instruction { opcode, operand }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Offset {
    Stream(u16),
    AddressSpace(u16),
}

impl std::ops::Add<u16> for Offset {
    type Output = Self;

    fn add(self, rhs: u16) -> Self::Output {
        match self {
            Offset::Stream(addr) => Offset::Stream(addr + rhs),
            Offset::AddressSpace(addr) => Offset::AddressSpace(addr + rhs),
        }
    }
}

impl Offset {
    fn to_address_space_offset(&self, stream_offset: u16) -> Result<u16, Error> {
        match *self {
            Offset::Stream(addr) => {
                if usize::from(addr) + usize::from(stream_offset) < usize::from(std::u16::MAX) {
                    Ok(addr + stream_offset)
                } else {
                    Err(DisassemblyError::OffsetOutOfBounds.into())
                }
            }
            Offset::AddressSpace(addr) => Ok(addr),
        }
    }

    fn to_stream_offset(&self, stream_offset: u16) -> Result<u16, Error> {
        match *self {
            Offset::Stream(addr) => Ok(addr),
            Offset::AddressSpace(addr) => {
                if addr as isize - (stream_offset as isize) < 0 {
                    Err(DisassemblyError::OffsetOutOfBounds.into())
                } else {
                    Ok(addr - stream_offset)
                }
            }
        }
    }
}

pub struct Disassembler<'a, R: ReadBytesExt + Seek> {
    rom: &'a mut R,
    decoded_bytes: BitSet,
    start_offset: u16,
    unexplored: Vec<Offset>,
}

impl<'a, R: ReadBytesExt + Seek> Disassembler<'a, R> {
    pub fn new(rom: &'a mut R, pc_start: u16) -> Self {
        Disassembler {
            rom,
            start_offset: pc_start,
            decoded_bytes: BitSet::new(),
            unexplored: Vec::new(),
        }
    }

    pub fn read(&mut self) -> Result<Option<(u16, Instruction)>, Error> {
        if let Some(opcode_offset) = self.next_opcode_offset()? {
            let opcode_index = self.read_u8()? as usize;
            if let Some((opcode, addressing_mode)) = OPCODES[opcode_index] {
                let operand: Result<Operand, Error> = match addressing_mode {
                    AddressingMode::IndirectIndexed => {
                        Ok(Operand::IndirectIndexed(self.read_u8()?))
                    }
                    AddressingMode::Indirect => Ok(Operand::Indirect(self.read_u16()?)),
                    AddressingMode::IndexedIndirect => {
                        Ok(Operand::IndexedIndirect(self.read_u8()?))
                    }
                    AddressingMode::Immediate => Ok(Operand::Immediate(self.read_u8()?)),
                    AddressingMode::Relative => {
                        let relative_offset = self.read_i8()?;

                        let target_offset: Result<Offset, Error> = {
                            let addr = self.cur_offset()?.to_stream_offset(self.start_offset)?
                                as isize
                                + relative_offset as isize;

                            if addr < 0 || addr >= std::u16::MAX as isize {
                                Err(DisassemblyError::BranchOutOfBounds {
                                    branch_location: self.to_address_space_offset(opcode_offset)?,
                                }
                                .into())
                            } else {
                                Ok(Offset::Stream(addr as u16))
                            }
                        };

                        let target_offset = target_offset?;

                        if !self.is_decoded(target_offset)? {
                            info!(
                                "Pushed branch target onto return stack: {:04X}",
                                self.to_address_space_offset(target_offset)?
                            );
                            self.unexplored.push(target_offset);
                        }

                        Ok(Operand::Relative(relative_offset))
                    }
                    AddressingMode::Accumulator => Ok(Operand::Accumulator),
                    AddressingMode::Absolute => {
                        let addr = Offset::AddressSpace(self.read_u16()?);
                        if opcode == Opcode::Jsr && !self.is_decoded(addr)? {
                            info!(
                                "Pushed routine pointer onto routine stack: {:04X}",
                                self.to_address_space_offset(addr)?
                            );
                            self.unexplored.push(addr);
                        }
                        Ok(Operand::Absolute(self.to_address_space_offset(addr)?))
                    }
                    AddressingMode::AbsoluteX => Ok(Operand::AbsoluteX(self.read_u16()?)),
                    AddressingMode::AbsoluteY => Ok(Operand::AbsoluteY(self.read_u16()?)),
                    AddressingMode::ZeroPage => Ok(Operand::ZeroPage(self.read_u8()?)),
                    AddressingMode::ZeroPageX => Ok(Operand::ZeroPageX(self.read_u8()?)),
                    AddressingMode::ZeroPageY => Ok(Operand::ZeroPageY(self.read_u8()?)),
                    AddressingMode::Implied => match opcode {
                        Opcode::Brk => Ok(Operand::BreakByte(self.read_u8()?)),
                        Opcode::Rts => {
                            if let Some(next_routine) = self.unexplored.pop() {
                                info!(
                                    "End subroutine, popped next routine at {:04X}",
                                    self.to_address_space_offset(next_routine)?
                                );
                                self.jump_to(next_routine)?;
                            }
                            Ok(Operand::Implied)
                        }
                        _ => Ok(Operand::Implied),
                    },
                };

                Ok(Some((
                    self.to_address_space_offset(opcode_offset)?,
                    Instruction::new(opcode, operand?),
                )))
            } else {
                Err(DisassemblyError::IllegalOpcode {
                    opcode_location: self.to_address_space_offset(opcode_offset)?,
                }
                .into())
            }
        } else {
            Ok(None)
        }
    }

    fn to_stream_offset(&self, offset: Offset) -> Result<u16, Error> {
        offset.to_stream_offset(self.start_offset)
    }

    fn to_address_space_offset(&self, offset: Offset) -> Result<u16, Error> {
        offset.to_address_space_offset(self.start_offset)
    }

    fn next_opcode_offset(&mut self) -> Result<Option<Offset>, Error> {
        let opcode_location = self.cur_offset()?;

        if self.is_decoded(opcode_location)? {
            if let Some(next_opcode_location) = self.unexplored.pop() {
                info!(
                    "Already decoded {:04X}. Moving to {:04X}",
                    self.to_address_space_offset(opcode_location)?,
                    self.to_address_space_offset(next_opcode_location)?
                );
                self.jump_to(next_opcode_location)?;
                Ok(Some(next_opcode_location))
            } else {
                Ok(None)
            }
        } else {
            Ok(Some(opcode_location))
        }
    }

    fn jump_to(&mut self, addr: Offset) -> Result<(), Error> {
        self.rom
            .seek(SeekFrom::Start(u64::from(self.to_stream_offset(addr)?)))?;
        Ok(())
    }

    fn read_u8(&mut self) -> Result<u8, Error> {
        let cur_pointer = self.cur_offset()?;
        self.mark_decoded(cur_pointer)?;
        Ok(self.rom.read_u8()?)
    }

    fn read_i8(&mut self) -> Result<i8, Error> {
        let cur_pointer = self.cur_offset()?;
        self.mark_decoded(cur_pointer)?;
        Ok(self.rom.read_i8()?)
    }

    fn read_u16(&mut self) -> Result<u16, Error> {
        let cur_pointer = self.cur_offset()?;
        self.mark_decoded(cur_pointer)?;
        self.mark_decoded(cur_pointer + 1)?;
        Ok(self.rom.read_u16::<LittleEndian>()?)
    }

    fn mark_decoded(&mut self, addr: Offset) -> Result<(), Error> {
        let addr = self.to_address_space_offset(addr)?;
        self.decoded_bytes.insert(usize::from(addr));
        Ok(())
    }

    fn is_decoded(&self, addr: Offset) -> Result<bool, Error> {
        let addr = self.to_address_space_offset(addr)?;
        Ok(self.decoded_bytes.contains(usize::from(addr)))
    }

    fn cur_offset(&mut self) -> Result<Offset, Error> {
        let cur_pointer = self.rom.stream_position()?;
        if cur_pointer < u64::from(std::u16::MAX) {
            Ok(Offset::Stream(cur_pointer as u16))
        } else {
            Err(DisassemblyError::OffsetOutOfBounds.into())
        }
    }
}
