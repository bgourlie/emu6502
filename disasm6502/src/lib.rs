#![feature(seek_convenience)]

use {
    bit_set::BitSet,
    byteorder::{LittleEndian, ReadBytesExt},
    failure::{Error, Fail},
    log::info,
    std::io::{Seek, SeekFrom},
};

#[rustfmt::skip]
const OPCODES: [Option<(Op, Addressing)>; 0x100] = [
    Some((Op::Brk, Addressing::Implied)),           Some((Op::Ora, Addressing::IndexedIndirect)),
    None,                                           None,
    None,                                           Some((Op::Ora, Addressing::ZeroPage)),
    Some((Op::Asl, Addressing::ZeroPage)),          None,
    Some((Op::Php, Addressing::Implied)),           Some((Op::Ora, Addressing::Immediate)),
    Some((Op::Asl, Addressing::Accumulator)),       None,
    None,                                           Some((Op::Ora, Addressing::Absolute)),
    Some((Op::Asl, Addressing::Absolute)),          None,
    Some((Op::Bpl, Addressing::Relative)),          Some((Op::Ora, Addressing::IndirectIndexed)),
    None,                                           None,
    None,                                           Some((Op::Ora, Addressing::ZeroPageX)),
    Some((Op::Asl, Addressing::ZeroPageX)),         None,
    Some((Op::Clc, Addressing::Implied)),           Some((Op::Ora, Addressing::AbsoluteY)),
    None,                                           None,
    None,                                           Some((Op::Ora, Addressing::AbsoluteX)),
    Some((Op::Asl, Addressing::AbsoluteX)),         None,
    Some((Op::Jsr, Addressing::Absolute)),          Some((Op::And, Addressing::IndexedIndirect)),
    None,                                           None,
    Some((Op::Bit, Addressing::ZeroPage)),          Some((Op::And, Addressing::ZeroPage)),
    Some((Op::Rol, Addressing::ZeroPage)),          None,
    Some((Op::Plp, Addressing::Implied)),           Some((Op::And, Addressing::Immediate)),
    Some((Op::Rol, Addressing::Accumulator)),       None,
    Some((Op::Bit, Addressing::Absolute)),          Some((Op::And, Addressing::Absolute)),
    Some((Op::Rol, Addressing::Absolute)),          None,
    Some((Op::Bmi, Addressing::Relative)),          Some((Op::And, Addressing::IndirectIndexed)),
    None,                                           None,
    None,                                           Some((Op::And, Addressing::ZeroPageX)),
    Some((Op::Rol, Addressing::ZeroPageX)),         None,
    Some((Op::Sec, Addressing::Implied)),           Some((Op::And, Addressing::AbsoluteY)),
    None,                                           None,
    None,                                           Some((Op::And, Addressing::AbsoluteX)),
    Some((Op::Rol, Addressing::AbsoluteX)),         None,
    Some((Op::Rti, Addressing::Implied)),           Some((Op::Eor, Addressing::IndexedIndirect)),
    None,                                           None,
    None,                                           Some((Op::Eor, Addressing::ZeroPage)),
    Some((Op::Lsr, Addressing::ZeroPage)),          None,
    Some((Op::Pha, Addressing::Implied)),           Some((Op::Eor, Addressing::Immediate)),
    Some((Op::Lsr, Addressing::Accumulator)),       None,
    Some((Op::Jmp, Addressing::Absolute)),          Some((Op::Eor, Addressing::Absolute)),
    Some((Op::Lsr, Addressing::Absolute)),          None,
    Some((Op::Bvc, Addressing::Relative)),          Some((Op::Eor, Addressing::IndirectIndexed)),
    None,                                           None,
    None,                                           Some((Op::Eor, Addressing::ZeroPageX)),
    Some((Op::Lsr, Addressing::ZeroPageX)),         None,
    Some((Op::Cli, Addressing::Implied)),           Some((Op::Eor, Addressing::AbsoluteY)),
    None,                                           None,
    None,                                           Some((Op::Eor, Addressing::AbsoluteX)),
    Some((Op::Lsr, Addressing::AbsoluteX)),         None,
    Some((Op::Rts, Addressing::Implied)),           Some((Op::Adc, Addressing::IndexedIndirect)),
    None,                                           None,
    None,                                           Some((Op::Adc, Addressing::ZeroPage)),
    Some((Op::Ror, Addressing::ZeroPage)),          None,
    Some((Op::Pla, Addressing::Implied)),           Some((Op::Adc, Addressing::Immediate)),
    Some((Op::Ror, Addressing::Accumulator)),       None,
    Some((Op::Jmp, Addressing::Indirect)),          Some((Op::Adc, Addressing::Absolute)),
    Some((Op::Ror, Addressing::Absolute)),          None,
    Some((Op::Bvs, Addressing::Relative)),          Some((Op::Adc, Addressing::IndirectIndexed)),
    None,                                           None,
    None,                                           Some((Op::Adc, Addressing::ZeroPageX)),
    Some((Op::Ror, Addressing::ZeroPageX)),         None,
    Some((Op::Sei, Addressing::Implied)),           Some((Op::Adc, Addressing::AbsoluteY)),
    None,                                           None,
    None,                                           Some((Op::Adc, Addressing::AbsoluteX)),
    Some((Op::Ror, Addressing::AbsoluteX)),         None,
    None,                                           Some((Op::Sta, Addressing::IndexedIndirect)),
    None,                                           None,
    Some((Op::Sty, Addressing::ZeroPage)),          Some((Op::Sta, Addressing::ZeroPage)),
    Some((Op::Stx, Addressing::ZeroPage)),          None,
    Some((Op::Dey, Addressing::Implied)),           None,
    Some((Op::Txa, Addressing::Implied)),           None,
    Some((Op::Sty, Addressing::Absolute)),          Some((Op::Sta, Addressing::Absolute)),
    Some((Op::Stx, Addressing::Absolute)),          None,
    Some((Op::Bcc, Addressing::Relative)),          Some((Op::Sta, Addressing::IndirectIndexed)),
    None,                                           None,
    Some((Op::Sty, Addressing::ZeroPageX)),         Some((Op::Sta, Addressing::ZeroPageX)),
    Some((Op::Stx, Addressing::ZeroPageY)),         None,
    Some((Op::Tya, Addressing::Implied)),           Some((Op::Sta, Addressing::AbsoluteY)),
    Some((Op::Txs, Addressing::Implied)),           None,
    None,                                           Some((Op::Sta, Addressing::AbsoluteX)),
    None,                                           None,
    Some((Op::Ldy, Addressing::Immediate)),         Some((Op::Lda, Addressing::IndexedIndirect)),
    Some((Op::Ldx, Addressing::Immediate)),         None,
    Some((Op::Ldy, Addressing::ZeroPage)),          Some((Op::Lda, Addressing::ZeroPage)),
    Some((Op::Ldx, Addressing::ZeroPage)),          None,
    Some((Op::Tay, Addressing::Implied)),           Some((Op::Lda, Addressing::Immediate)),
    Some((Op::Tax, Addressing::Implied)),           None,
    Some((Op::Ldy, Addressing::Absolute)),          Some((Op::Lda, Addressing::Absolute)),
    Some((Op::Ldx, Addressing::Absolute)),          None,
    Some((Op::Bcs, Addressing::Relative)),          Some((Op::Lda, Addressing::IndirectIndexed)),
    None,                                           None,
    Some((Op::Ldy, Addressing::ZeroPageX)),         Some((Op::Lda, Addressing::ZeroPageX)),
    Some((Op::Ldx, Addressing::ZeroPageY)),         None,
    Some((Op::Clv, Addressing::Implied)),           Some((Op::Lda, Addressing::AbsoluteY)),
    Some((Op::Tsx, Addressing::Implied)),           None,
    Some((Op::Ldy, Addressing::AbsoluteX)),         Some((Op::Lda, Addressing::AbsoluteX)),
    Some((Op::Ldx, Addressing::AbsoluteY)),         None,
    Some((Op::Cpy, Addressing::Immediate)),         Some((Op::Cmp, Addressing::IndexedIndirect)),
    None,                                           None,
    Some((Op::Cpy, Addressing::ZeroPage)),          Some((Op::Cmp, Addressing::ZeroPage)),
    Some((Op::Dec, Addressing::ZeroPage)),          None,
    Some((Op::Iny, Addressing::Implied)),           Some((Op::Cmp, Addressing::Immediate)),
    Some((Op::Dex, Addressing::Implied)),           None,
    Some((Op::Cpy, Addressing::Absolute)),          Some((Op::Cmp, Addressing::Absolute)),
    Some((Op::Dec, Addressing::Absolute)),          None,
    Some((Op::Bne, Addressing::Relative)),          Some((Op::Cmp, Addressing::IndirectIndexed)),
    None,                                           None,
    None,                                           Some((Op::Cmp, Addressing::ZeroPageX)),
    Some((Op::Dec, Addressing::ZeroPageX)),         None,
    Some((Op::Cld, Addressing::Implied)),           Some((Op::Cmp, Addressing::AbsoluteY)),
    None,                                           None,
    None,                                           Some((Op::Cmp, Addressing::AbsoluteX)),
    Some((Op::Dec, Addressing::AbsoluteX)),         None,
    Some((Op::Cpx, Addressing::Immediate)),         Some((Op::Sbc, Addressing::IndexedIndirect)),
    None,                                           None,
    Some((Op::Cpx, Addressing::ZeroPage)),          Some((Op::Sbc, Addressing::ZeroPage)),
    Some((Op::Inc, Addressing::ZeroPage)),          None,
    Some((Op::Inx, Addressing::Implied)),           Some((Op::Sbc, Addressing::Immediate)),
    Some((Op::Nop, Addressing::Implied)),           None,
    Some((Op::Cpx, Addressing::Absolute)),          Some((Op::Sbc, Addressing::Absolute)),
    Some((Op::Inc, Addressing::Absolute)),          None,
    Some((Op::Beq, Addressing::Relative)),          Some((Op::Sbc, Addressing::IndirectIndexed)),
    None,                                           None,
    None,                                           Some((Op::Sbc, Addressing::ZeroPageX)),
    Some((Op::Inc, Addressing::ZeroPageX)),         None,
    Some((Op::Sed, Addressing::Implied)),           Some((Op::Sbc, Addressing::AbsoluteY)),
    None,                                           None,
    None,                                           Some((Op::Sbc, Addressing::AbsoluteX)),
    Some((Op::Inc, Addressing::AbsoluteX)),         None,
];

#[rustfmt::skip]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Op {
    Adc, And, Asl, Bcc, Bcs, Beq, Bit, Bmi, Bne, Bpl, Brk, Bvc, Bvs, Clc, Cld, Cli,
    Clv, Cmp, Cpx, Cpy, Dec, Dex, Dey, Eor, Inc, Inx, Iny, Jmp, Jsr, Lda, Ldx, Ldy,
    Lsr, Nop, Ora, Pha, Php, Pla, Plp, Rol, Ror, Rti, Rts, Sbc, Sec, Sed, Sei, Sta,
    Stx, Sty, Tax, Tay, Tsx, Txa, Txs, Tya,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Addressing {
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
    #[fail(display = "Decode start must be greater than the address space offset")]
    InvalidDecodeStart,

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
pub enum Decoded {
    Unmapped,
    Mapped(Instruction),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
    opcode: Op,
    operand: Operand,
}

impl Instruction {
    fn new(opcode: Op, operand: Operand) -> Self {
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
    address_space_offset: u16,
    unexplored: Vec<Offset>,
}

impl<'a, R: ReadBytesExt + Seek> Disassembler<'a, R> {
    pub fn new(
        rom: &'a mut R,
        address_space_offset: u16,
        decode_start: u16,
    ) -> Result<Self, Error> {
        if decode_start < address_space_offset {
            Err(DisassemblyError::InvalidDecodeStart.into())
        } else {
            rom.seek(SeekFrom::Start(
                u64::from(decode_start) - u64::from(address_space_offset),
            ))?;
            Ok(Disassembler {
                rom,
                address_space_offset,
                decoded_bytes: BitSet::new(),
                unexplored: Vec::new(),
            })
        }
    }

    pub fn read(&mut self) -> Result<Option<(u16, Decoded)>, Error> {
        if let Some(opcode_offset) = self.next_opcode_offset()? {
            if self.is_mapped(opcode_offset) {
                self.jump_to(opcode_offset)?;
                let opcode_index = self.read_u8()? as usize;
                if let Some((opcode, addressing_mode)) = OPCODES[opcode_index] {
                    let operand: Result<Operand, Error> = match addressing_mode {
                        Addressing::IndirectIndexed => {
                            Ok(Operand::IndirectIndexed(self.read_u8()?))
                        }
                        Addressing::Indirect => Ok(Operand::Indirect(self.read_u16()?)),
                        Addressing::IndexedIndirect => {
                            Ok(Operand::IndexedIndirect(self.read_u8()?))
                        }
                        Addressing::Immediate => Ok(Operand::Immediate(self.read_u8()?)),
                        Addressing::Relative => {
                            let relative_offset = self.read_i8()?;

                            let target_offset: Result<Offset, Error> = {
                                let addr = self
                                    .cur_offset()?
                                    .to_stream_offset(self.address_space_offset)?
                                    as isize
                                    + relative_offset as isize;

                                if addr < 0 || addr >= std::u16::MAX as isize {
                                    Err(DisassemblyError::BranchOutOfBounds {
                                        branch_location: self
                                            .to_address_space_offset(opcode_offset)?,
                                    }
                                    .into())
                                } else {
                                    Ok(Offset::Stream(addr as u16))
                                }
                            };

                            let target_offset = target_offset?;
                            self.push_unexplored(target_offset)?;
                            Ok(Operand::Relative(relative_offset))
                        }
                        Addressing::Accumulator => Ok(Operand::Accumulator),
                        Addressing::Absolute => {
                            let addr = Offset::AddressSpace(self.read_u16()?);
                            if opcode == Op::Jsr {
                                self.push_unexplored(addr)?;
                            }
                            Ok(Operand::Absolute(self.to_address_space_offset(addr)?))
                        }
                        Addressing::AbsoluteX => Ok(Operand::AbsoluteX(self.read_u16()?)),
                        Addressing::AbsoluteY => Ok(Operand::AbsoluteY(self.read_u16()?)),
                        Addressing::ZeroPage => Ok(Operand::ZeroPage(self.read_u8()?)),
                        Addressing::ZeroPageX => Ok(Operand::ZeroPageX(self.read_u8()?)),
                        Addressing::ZeroPageY => Ok(Operand::ZeroPageY(self.read_u8()?)),
                        Addressing::Implied => match opcode {
                            Op::Brk => Ok(Operand::BreakByte(self.read_u8()?)),
                            Op::Rts => {
                                // Seek back one byte so we don't decode past the Rts. The next call to
                                // read() will see that this offset has been decoded and pop the next
                                // unexplored offset.
                                self.rom.seek(SeekFrom::Current(-1))?;
                                Ok(Operand::Implied)
                            }
                            _ => Ok(Operand::Implied),
                        },
                    };

                    Ok(Some((
                        self.to_address_space_offset(opcode_offset)?,
                        Decoded::Mapped(Instruction::new(opcode, operand?)),
                    )))
                } else {
                    Err(DisassemblyError::IllegalOpcode {
                        opcode_location: self.to_address_space_offset(opcode_offset)?,
                    }
                    .into())
                }
            } else {
                self.mark_decoded(opcode_offset)?;
                Ok(Some((
                    self.to_address_space_offset(opcode_offset)?,
                    Decoded::Unmapped,
                )))
            }
        } else {
            Ok(None)
        }
    }

    fn is_mapped(&self, offset: Offset) -> bool {
        match offset {
            Offset::Stream(_) => true,
            Offset::AddressSpace(addr) => {
                // TODO: Check if the rom maps to the end of the address space
                if addr < self.address_space_offset {
                    false
                } else {
                    true
                }
            }
        }
    }

    fn push_unexplored(&mut self, offset: Offset) -> Result<(), Error> {
        if !self.is_decoded(offset)? {
            info!(
                "Pushed {:04X} onto the unexplored stack",
                self.to_address_space_offset(offset)?
            );
            self.unexplored.push(offset);
        }
        Ok(())
    }

    fn to_stream_offset(&self, offset: Offset) -> Result<u16, Error> {
        offset.to_stream_offset(self.address_space_offset)
    }

    fn to_address_space_offset(&self, offset: Offset) -> Result<u16, Error> {
        offset.to_address_space_offset(self.address_space_offset)
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
