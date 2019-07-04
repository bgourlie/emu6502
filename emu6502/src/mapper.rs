use std::io::Read;

const ADDRESSABLE_MEMORY: usize = 0x10000;

pub trait Mapper {
    fn peek(&self, addr: u16) -> u8;
    fn peek_mut(&mut self, addr: u16) -> u8 {
        self.peek(addr)
    }
    fn poke(&mut self, addr: u16, value: u8);
}

pub struct BasicMapper {
    memory: Box<[u8; ADDRESSABLE_MEMORY]>,
}

impl BasicMapper {
    pub fn new<R: Read>(reader: &mut R, mapping_start: u16) -> Self {
        let mapping_start = usize::from(mapping_start);
        let mut memory = Box::new([0; ADDRESSABLE_MEMORY]);
        reader.read_exact(&mut memory[mapping_start..]).unwrap();
        BasicMapper { memory }
    }
}

impl Mapper for BasicMapper {
    fn peek(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        self.memory[addr]
    }

    fn poke(&mut self, addr: u16, data: u8) {
        let addr = addr as usize;
        self.memory[addr] = data;
    }
}
