#![feature(seek_convenience)]

use {
    bit_set::BitSet,
    byteorder::{LittleEndian, ReadBytesExt},
    failure::{Error, Fail},
    log::{info, warn},
    std::{
        cmp,
        collections::BTreeMap,
        io::{Seek, SeekFrom},
        iter::{IntoIterator, Iterator},
        u16,
    },
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

fn operand_length(instruction: Instruction) -> u16 {
    if instruction.opcode == Op::Brk {
        1
    } else {
        match instruction.operand {
            Operand::Implied | Operand::Accumulator => 0,
            Operand::ZeroPage(_)
            | Operand::ZeroPageX(_)
            | Operand::ZeroPageY(_)
            | Operand::Immediate(_)
            | Operand::Relative(_) => 1,
            _ => 2,
        }
    }
}

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

impl ToString for Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Absolute(addr) => format!("${:04X}", addr),
            Operand::AbsoluteX(addr) => format!("${:04X},X", addr),
            Operand::AbsoluteY(addr) => format!("${:04X},Y", addr),
            Operand::Accumulator => "A".to_owned(),
            Operand::Immediate(val) => format!("#${:02X}", val),
            Operand::Implied | Operand::BreakByte(_) => "".to_owned(),
            Operand::Indirect(addr) => format!("(${:04X})", addr),
            Operand::IndexedIndirect(addr) => format!("(${:04X},X)", addr),
            Operand::IndirectIndexed(addr) => format!("(${:04X}),Y", addr),
            Operand::Relative(offset) => format!("{}", offset), // TODO: label
            Operand::ZeroPage(addr) => format!("${:02X}", addr),
            Operand::ZeroPageX(addr) => format!("${:02X},X", addr),
            Operand::ZeroPageY(addr) => format!("${:02X},Y", addr),
        }
    }
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

    pub fn opcode(self) -> Op {
        self.opcode
    }

    pub fn operand(self) -> Operand {
        self.operand
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
    fn to_address_space_offset(self, stream_offset: u16) -> Result<u16, Error> {
        match self {
            Offset::Stream(addr) => {
                if usize::from(addr) + usize::from(stream_offset) <= usize::from(u16::MAX) {
                    Ok(addr + stream_offset)
                } else {
                    Err(DisassemblyError::OffsetOutOfBounds.into())
                }
            }
            Offset::AddressSpace(addr) => Ok(addr),
        }
    }

    fn to_stream_offset(self, stream_offset: u16) -> Result<u16, Error> {
        match self {
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

pub struct DisassemblerIterator<'a, R: ReadBytesExt + Seek> {
    inner: Disassembler<'a, R>,
    encountered_err: bool,
}

impl<'a, R: ReadBytesExt + Seek> Iterator for DisassemblerIterator<'a, R> {
    type Item = (u16, Instruction);

    fn next(&mut self) -> Option<Self::Item> {
        if self.encountered_err {
            None
        } else {
            match self.inner.decode_next() {
                Ok(val) => val,
                Err(err) => {
                    warn!("Disassembly ended due to error: {:?}", err);
                    None
                }
            }
        }
    }
}

pub struct Disassembler<'a, R: ReadBytesExt + Seek> {
    rom: &'a mut R,
    address_space_start_offset: u16,
    address_space_end_offset: u16,
    decoded_bytes: BitSet,
    unexplored: Vec<Offset>,
}

impl<'a, R: ReadBytesExt + Seek> IntoIterator for Disassembler<'a, R> {
    type Item = (u16, Instruction);
    type IntoIter = DisassemblerIterator<'a, R>;

    fn into_iter(self) -> Self::IntoIter {
        DisassemblerIterator {
            inner: self,
            encountered_err: false,
        }
    }
}

impl<'a, R: ReadBytesExt + Seek> Disassembler<'a, R> {
    pub fn new(
        rom: &'a mut R,
        address_space_start_offset: u16,
        decode_start: u16,
    ) -> Result<Self, Error> {
        if decode_start < address_space_start_offset {
            Err(DisassemblyError::InvalidDecodeStart.into())
        } else {
            let address_space_end_offset = cmp::min(
                rom.stream_len()? + u64::from(address_space_start_offset),
                u64::from(u16::MAX),
            ) as u16;

            info!(
                "Disassembling ROM mapped from {:04X} to {:04X}",
                address_space_start_offset, address_space_end_offset
            );

            let mut disassembler = Disassembler {
                rom,
                address_space_start_offset,
                address_space_end_offset,
                decoded_bytes: BitSet::new(),
                unexplored: Vec::new(),
            };

            if disassembler.is_mapped(Offset::AddressSpace(0xfffa)) {
                disassembler.jump_to(Offset::AddressSpace(0xfffa))?;
                let nmi_vector = disassembler.read_u16()?;
                disassembler.push_unexplored(Offset::AddressSpace(nmi_vector))?;
                info!("NMI vector detected at {:04X}", nmi_vector);
            } else {
                info!("NMI vector not mapped");
            }

            if disassembler.is_mapped(Offset::AddressSpace(0xfffc)) {
                disassembler.jump_to(Offset::AddressSpace(0xfffc))?;
                let reset_vector = disassembler.read_u16()?;
                disassembler.push_unexplored(Offset::AddressSpace(reset_vector))?;
                info!("Reset vector detected at {:04X}", reset_vector);
            } else {
                info!("Reset vector not mapped");
            }

            if disassembler.is_mapped(Offset::AddressSpace(0xfffe)) {
                disassembler.jump_to(Offset::AddressSpace(0xfffe))?;
                let irq_vector = disassembler.read_u16()?;
                disassembler.push_unexplored(Offset::AddressSpace(irq_vector))?;
                info!("IRQ vector detected at {:04X}", irq_vector);
            } else {
                info!("IRQ vector not mapped");
            }

            disassembler.jump_to(Offset::AddressSpace(decode_start))?;

            Ok(disassembler)
        }
    }

    pub fn decode_next(&mut self) -> Result<Option<(u16, Instruction)>, Error> {
        if let Some(opcode_offset) = self.next_opcode_offset()? {
            let opcode_index = self.read_u8()? as usize;
            if let Some((opcode, addressing_mode)) = OPCODES[opcode_index] {
                let operand: Result<Operand, Error> = match addressing_mode {
                    Addressing::IndirectIndexed => Ok(Operand::IndirectIndexed(self.read_u8()?)),
                    Addressing::Indirect => Ok(Operand::Indirect(self.read_u16()?)),
                    Addressing::IndexedIndirect => Ok(Operand::IndexedIndirect(self.read_u8()?)),
                    Addressing::Immediate => Ok(Operand::Immediate(self.read_u8()?)),
                    Addressing::Relative => {
                        let relative_offset = self.read_i8()?;

                        let target_offset: Result<Offset, Error> = {
                            let addr = self
                                .cur_offset()?
                                .to_stream_offset(self.address_space_start_offset)?
                                as isize
                                + relative_offset as isize;

                            if addr < 0 || addr >= u16::MAX as isize {
                                Err(DisassemblyError::BranchOutOfBounds {
                                    branch_location: self.to_address_space_offset(opcode_offset)?,
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
                        match opcode {
                            Op::Jsr => self.push_unexplored(addr)?,
                            Op::Jmp => {
                                self.push_unexplored(addr)?;

                                // Seek back three bytes so we don't decode past the jump. The next
                                // call to read() will see that this offset has been decoded and pop
                                // the next unexplored offset.
                                self.rom.seek(SeekFrom::Current(-1))?;
                            }
                            _ => {}
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
                        Op::Rts | Op::Rti => {
                            // Seek back one byte so we don't decode past the return. The next
                            // call to read() will see that this offset has been decoded and pop
                            // the next unexplored offset.
                            self.rom.seek(SeekFrom::Current(-1))?;
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

    fn is_mapped(&self, offset: Offset) -> bool {
        match offset {
            Offset::Stream(_) => true,
            Offset::AddressSpace(addr) => {
                !(addr < self.address_space_start_offset || addr >= self.address_space_end_offset)
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
        offset.to_stream_offset(self.address_space_start_offset)
    }

    fn to_address_space_offset(&self, offset: Offset) -> Result<u16, Error> {
        offset.to_address_space_offset(self.address_space_start_offset)
    }

    fn next_opcode_offset(&mut self) -> Result<Option<Offset>, Error> {
        let opcode_location = self.cur_offset()?;
        if !self.is_decoded(opcode_location)? {
            Ok(Some(opcode_location))
        } else {
            loop {
                if let Some(next_opcode_location) = self.unexplored.pop() {
                    if !self.is_decoded(next_opcode_location)? {
                        if self.is_mapped(next_opcode_location) {
                            self.jump_to(next_opcode_location)?;
                            break Ok(Some(next_opcode_location));
                        } else {
                            self.mark_decoded(next_opcode_location)?;
                            info!(
                                "{:04X} isn't mapped, skipping",
                                self.to_address_space_offset(next_opcode_location)?
                            );
                        }
                    } else {
                        info!(
                            "Already decoded {:04X}",
                            self.to_address_space_offset(opcode_location)?
                        );
                    }
                } else {
                    break Ok(None);
                }
            }
        }
    }

    fn jump_to(&mut self, offset: Offset) -> Result<(), Error> {
        info!(
            "Decoder jumped to {:04X}",
            self.to_address_space_offset(offset)?
        );

        let stream_offset = u64::from(self.to_stream_offset(offset)?);
        self.rom.seek(SeekFrom::Start(stream_offset))?;
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
        if cur_pointer < u64::from(u16::MAX) {
            Ok(Offset::Stream(cur_pointer as u16))
        } else {
            Err(DisassemblyError::OffsetOutOfBounds.into())
        }
    }
}

#[derive(Copy, Clone)]
pub enum Address {
    /// An address that is an instruction pointer, containing the fully decoded instruction.
    Instruction(Instruction),

    /// An address that is part part of an instruction, containing the instruction pointer.
    Operand(u16),
}

pub struct Disassembly {
    address_space: BTreeMap<u16, Address>,
}

impl Disassembly {
    pub fn from_rom<R: ReadBytesExt + Seek>(
        rom: &mut R,
        address_space_start_offset: u16,
        decode_start: u16,
    ) -> Result<Self, Error> {
        let mut address_space = BTreeMap::new();
        let disassembler = Disassembler::new(rom, address_space_start_offset, decode_start)?;

        for (addr, instr) in disassembler {
            let operand_len = operand_length(instr);
            address_space.insert(addr, Address::Instruction(instr));
            for i in 0..operand_len {
                address_space.insert(addr + i + 1, Address::Operand(addr));
            }
        }

        Ok(Disassembly { address_space })
    }

    pub fn window(&self, offset: u16, size: u16) -> impl Iterator<Item = (&u16, &Instruction)> {
        let half_size = usize::from(size / 2);
        self.address_space
            .range(..offset)
            .rev()
            .filter_map(|(offset, a)| {
                if let Address::Instruction(i) = a {
                    Some((offset, i))
                } else {
                    None
                }
            })
            .take(half_size)
            /* TODO: Find a way to avoid the following allocation */
            .collect::<Vec<(&u16, &Instruction)>>()
            .into_iter()
            .rev()
            .chain(
                self.address_space
                    .range(offset..)
                    .filter_map(|(offset, a)| {
                        if let Address::Instruction(i) = a {
                            Some((offset, i))
                        } else {
                            None
                        }
                    })
                    .take(half_size),
            )
    }

    pub fn instruction_at(&self, offset: u16) -> Option<Instruction> {
        self.address_space.get(&offset).map(|i| match i {
            Address::Instruction(instruction) => *instruction,
            Address::Operand(instruction_pointer) => {
                match self
                    .address_space
                    .get(instruction_pointer)
                    .expect("instruction pointer expected to point to decoded address")
                {
                    Address::Instruction(instruction) => *instruction,
                    _ => panic!("instruction pointer expected to point to instruction"),
                }
            }
        })
    }

    pub fn display_at(&self, offset: u16) -> Option<String> {
        self.instruction_at(offset)
            .map(|instr| format!("{:?} {}", instr.opcode, instr.operand.to_string()))
    }
}
