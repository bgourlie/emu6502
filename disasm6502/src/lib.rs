#![feature(seek_convenience)]

use {
    bit_set::BitSet,
    byteorder::{LittleEndian, ReadBytesExt},
    failure::{Error, Fail},
    log::{error, info},
    std::{
        cmp,
        collections::BTreeMap,
        io::{Seek, SeekFrom},
        iter::{FromIterator, IntoIterator, Iterator},
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
    #[fail(display = "Decode must map to an address in the supplied stream")]
    InvalidDecodeOffset,

    #[fail(display = "Branch out of bounds: {:X?}", branch_location)]
    BranchOutOfBounds { branch_location: u16 },

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
                    error!("Disassembly ended due to error: {:?}", err);
                    None
                }
            }
        }
    }
}

pub struct DisassemblerBuilder<'a, R: ReadBytesExt + Seek> {
    stream: &'a mut R,
    decode_offsets: Option<Vec<u16>>,
    mapping_start_offset: Option<u16>,
    visited_offsets: BitSet<u16>,
    detect_interrupt_vectors: bool,
}

impl<'a, R: ReadBytesExt + Seek> DisassemblerBuilder<'a, R> {
    /// Supply offsets to the disassembler that are instruction pointers from which disassembly
    /// should begin. In a typical scenario, you would supply an option containing the instruction
    /// pointer that represents the entry point. If none are supplied, disassembly will begin at
    /// the beginning of the supplied stream.
    pub fn with_decode_offsets<I: IntoIterator<Item = u16>>(mut self, offsets: I) -> Self {
        self.decode_offsets = Some(Vec::from_iter(offsets));
        self
    }

    /// In the event that the supplied stream doesn't map to the 16kB address space starting at
    /// offset zero, you can supply the starting offset that the stream maps to.
    pub fn with_mapping_start_offset(mut self, mapping_start_offset: u16) -> Self {
        self.mapping_start_offset = Some(mapping_start_offset);
        self
    }

    /// Indicate any offsets you want to be considered "visited," meaning the disassembler will not
    /// decode these offsets. This is useful if you want to update a few offsets of an existing
    /// disassembly.
    pub fn with_visited_offsets<I: IntoIterator<Item = u16>>(mut self, offsets: I) -> Self {
        self.visited_offsets
            .extend(offsets.into_iter().map(usize::from));
        self
    }

    /// Specify whether the disassembler should detect interrupt vectors and disassemble them
    /// starting from their respective offsets.
    pub fn detect_interrupt_vectors(mut self, val: bool) -> Self {
        self.detect_interrupt_vectors = val;
        self
    }

    pub fn build(mut self) -> Result<Disassembler<'a, R>, Error> {
        let mapping_start_offset = self.mapping_start_offset.unwrap_or(0);
        let mapping_end_offset = cmp::min(
            self.stream.stream_len()? + u64::from(mapping_start_offset),
            u64::from(u16::MAX),
        ) as u16;
        let decode_offsets = self.decode_offsets.take().unwrap_or_default();
        let has_invalid_decode_offsets = decode_offsets.iter().any(|decode_offset| {
            *decode_offset < mapping_start_offset || *decode_offset > mapping_end_offset
        });

        if has_invalid_decode_offsets {
            Err(DisassemblyError::InvalidDecodeOffset.into())
        } else {
            info!(
                "Disassembling stream mapped from {:04X} to {:04X}",
                mapping_start_offset, mapping_end_offset
            );

            let mut disassembler = Disassembler {
                stream: self.stream,
                mapping_start_offset,
                mapping_end_offset,
                visited_offsets: self.visited_offsets,
                decode_stack: decode_offsets,
            };

            if self.detect_interrupt_vectors {
                if disassembler.is_mapped(0xfffa) {
                    disassembler.jump_to(0xfffa)?;
                    let nmi_vector = disassembler.read_u16()?;
                    disassembler.push_decode_stack(nmi_vector);
                    info!("NMI vector detected at {:04X}", nmi_vector);
                } else {
                    info!("NMI vector not mapped");
                }

                if disassembler.is_mapped(0xfffc) {
                    disassembler.jump_to(0xfffc)?;
                    let reset_vector = disassembler.read_u16()?;
                    disassembler.push_decode_stack(reset_vector);
                    info!("Reset vector detected at {:04X}", reset_vector);
                } else {
                    info!("Reset vector not mapped");
                }

                if disassembler.is_mapped(0xfffe) {
                    disassembler.jump_to(0xfffe)?;
                    let irq_vector = disassembler.read_u16()?;
                    disassembler.push_decode_stack(irq_vector);
                    info!("IRQ vector detected at {:04X}", irq_vector);
                } else {
                    info!("IRQ vector not mapped");
                }
            }

            if let Some(offset) = disassembler.decode_stack.pop() {
                disassembler.jump_to(offset)?;
            } else {
                disassembler.jump_to(disassembler.mapping_start_offset)?;
            }

            Ok(disassembler)
        }
    }
}

pub struct Disassembler<'a, R: ReadBytesExt + Seek> {
    stream: &'a mut R,
    mapping_start_offset: u16,
    mapping_end_offset: u16,
    visited_offsets: BitSet<u16>,
    decode_stack: Vec<u16>,
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
    pub fn builder(stream: &'a mut R) -> DisassemblerBuilder<'a, R> {
        DisassemblerBuilder {
            stream,
            decode_offsets: None,
            mapping_start_offset: None,
            visited_offsets: BitSet::<u16>::default(),
            detect_interrupt_vectors: false,
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

                        let target_offset: Result<u16, Error> = {
                            let offset = self.cur_offset()? as isize + relative_offset as isize;

                            if offset < 0 || offset >= u16::MAX as isize {
                                Err(DisassemblyError::BranchOutOfBounds {
                                    branch_location: opcode_offset,
                                }
                                .into())
                            } else {
                                Ok(offset as u16)
                            }
                        };

                        let target_offset = target_offset?;
                        self.push_decode_stack(target_offset);
                        Ok(Operand::Relative(relative_offset))
                    }
                    Addressing::Accumulator => Ok(Operand::Accumulator),
                    Addressing::Absolute => {
                        let addr = self.read_u16()?;
                        match opcode {
                            Op::Jsr => self.push_decode_stack(addr),
                            Op::Jmp => {
                                self.push_decode_stack(addr);

                                // Seek back three bytes so we don't decode past the jump. The next
                                // call to read() will see that this offset has been decoded and pop
                                // the next unexplored offset.
                                self.stream.seek(SeekFrom::Current(-1))?;
                            }
                            _ => {}
                        }
                        Ok(Operand::Absolute(addr))
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
                            self.stream.seek(SeekFrom::Current(-1))?;
                            Ok(Operand::Implied)
                        }
                        _ => Ok(Operand::Implied),
                    },
                };

                Ok(Some((opcode_offset, Instruction::new(opcode, operand?))))
            } else {
                Err(DisassemblyError::IllegalOpcode {
                    opcode_location: opcode_offset,
                }
                .into())
            }
        } else {
            Ok(None)
        }
    }

    fn is_mapped(&self, offset: u16) -> bool {
        !(offset < self.mapping_start_offset || offset >= self.mapping_end_offset)
    }

    fn push_decode_stack(&mut self, offset: u16) {
        if !self.is_visited(offset) {
            info!("Pushed {:04X} onto the unexplored stack", offset);
            self.decode_stack.push(offset);
        }
    }

    fn next_opcode_offset(&mut self) -> Result<Option<u16>, Error> {
        let opcode_location = self.cur_offset()?;
        if !self.is_visited(opcode_location) {
            Ok(Some(opcode_location))
        } else {
            loop {
                if let Some(next_decode_location) = self.decode_stack.pop() {
                    if !self.is_visited(next_decode_location) {
                        if self.is_mapped(next_decode_location) {
                            self.jump_to(next_decode_location)?;
                            break Ok(Some(next_decode_location));
                        } else {
                            self.mark_visited(next_decode_location);
                            info!("{:04X} isn't mapped, skipping", next_decode_location);
                        }
                    } else {
                        info!("Already decoded {:04X}", opcode_location);
                    }
                } else {
                    break Ok(None);
                }
            }
        }
    }

    fn jump_to(&mut self, offset: u16) -> Result<(), Error> {
        info!("Decoder jumped to {:04X}", offset);
        let stream_offset = u64::from(offset - self.mapping_start_offset);
        self.stream.seek(SeekFrom::Start(stream_offset))?;
        Ok(())
    }

    fn read_u8(&mut self) -> Result<u8, Error> {
        let cur_pointer = self.cur_offset()?;
        self.mark_visited(cur_pointer);
        Ok(self.stream.read_u8()?)
    }

    fn read_i8(&mut self) -> Result<i8, Error> {
        let cur_pointer = self.cur_offset()?;
        self.mark_visited(cur_pointer);
        Ok(self.stream.read_i8()?)
    }

    fn read_u16(&mut self) -> Result<u16, Error> {
        let cur_pointer = self.cur_offset()?;
        self.mark_visited(cur_pointer);
        self.mark_visited(cur_pointer + 1);
        Ok(self.stream.read_u16::<LittleEndian>()?)
    }

    fn mark_visited(&mut self, offset: u16) {
        self.visited_offsets.insert(usize::from(offset));
    }

    fn is_visited(&self, offset: u16) -> bool {
        self.visited_offsets.contains(usize::from(offset))
    }

    fn cur_offset(&mut self) -> Result<u16, Error> {
        let cur_pointer = self.stream.stream_position()?;
        Ok(cur_pointer as u16 + self.mapping_start_offset)
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
    /// Disassemble the provided address space.
    ///
    /// # Arguments
    ///
    /// * `stream` - A seekable byte stream to disassemble
    /// * `address_space_start_offset` - The start offset that the byte stream maps to
    /// * `decode_offsets` - Offsets that are to be decoded
    pub fn from_stream<R: ReadBytesExt + Seek, I: IntoIterator<Item = u16>>(
        stream: &mut R,
        address_space_start_offset: u16,
        decode_offsets: I,
    ) -> Result<Self, Error> {
        let disassembler = Disassembler::builder(stream)
            .with_decode_offsets(decode_offsets)
            .with_mapping_start_offset(address_space_start_offset)
            .detect_interrupt_vectors(true)
            .build()?;

        let mut address_space = BTreeMap::new();
        Self::update_address_space(&mut address_space, disassembler);
        Ok(Disassembly { address_space })
    }

    /// Updates the disassembly starting at the specified offset. This is useful for example when
    /// the program counter points to an offset that was never disassembled or a particular offset
    /// was modified at runtime, as is the case with self-modifying code.
    pub fn update<R: ReadBytesExt + Seek>(
        &mut self,
        stream: &mut R,
        start_offset: u16,
    ) -> Result<(), Error> {
        let mut already_decoded =
            BitSet::<u16>::from_iter(self.address_space.keys().map(|offset| usize::from(*offset)));

        already_decoded.remove(usize::from(start_offset));

        let disassembler = Disassembler::builder(stream)
            .with_decode_offsets(Some(start_offset))
            .with_visited_offsets(already_decoded.iter().map(|offset| offset as u16))
            .build()?;

        Self::update_address_space(&mut self.address_space, disassembler);
        Ok(())
    }

    pub fn window(&self, offset: u16, size: u16) -> impl Iterator<Item = (&u16, &Instruction)> {
        let half_size = usize::from(size / 2);
        let mut window = Vec::with_capacity(half_size);
        window.extend(
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
                .take(half_size),
        );

        let rest_size = half_size - window.len() + half_size;

        window.into_iter().rev().chain(
            self.address_space
                .range(offset..)
                .filter_map(|(offset, a)| {
                    if let Address::Instruction(i) = a {
                        Some((offset, i))
                    } else {
                        None
                    }
                })
                .take(rest_size),
        )
    }

    /// Given an offset, which may be an instruction pointer or an address that is part of an
    /// instruction, return the instruction pointer and the decoded instruction.
    pub fn instruction_at(&self, offset: u16) -> Option<(u16, Instruction)> {
        self.address_space.get(&offset).map(|i| match i {
            Address::Instruction(instruction) => (offset, *instruction),
            Address::Operand(instruction_pointer) => {
                match self
                    .address_space
                    .get(instruction_pointer)
                    .expect("instruction pointer expected to point to decoded address")
                {
                    Address::Instruction(instruction) => (*instruction_pointer, *instruction),
                    _ => panic!("instruction pointer expected to point to instruction"),
                }
            }
        })
    }

    pub fn display_at(&self, offset: u16) -> Option<String> {
        self.instruction_at(offset)
            .map(|(_, instr)| format!("{:?} {}", instr.opcode, instr.operand.to_string()))
    }

    fn update_address_space<R: ReadBytesExt + Seek>(
        address_space: &mut BTreeMap<u16, Address>,
        disassembler: Disassembler<R>,
    ) {
        for (addr, instr) in disassembler {
            let operand_len = operand_length(instr);
            address_space.insert(addr, Address::Instruction(instr));
            for i in 0..operand_len {
                address_space.insert(addr + i + 1, Address::Operand(addr));
            }
        }
    }
}
