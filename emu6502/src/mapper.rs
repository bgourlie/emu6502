use crate::Debugger;
use fnv::FnvHashSet;
use std::{
    io::{Cursor, Read},
    iter::FromIterator,
};

const ADDRESSABLE_MEMORY: usize = 0x10000;

pub trait Mapper: Sized {
    fn new<R: Read>(_reader: &mut R, _mapping_start: u16) -> Self {
        unimplemented!()
    }
    fn peek(&self, addr: u16) -> u8;
    fn peek_mut(&mut self, addr: u16) -> u8 {
        self.peek(addr)
    }
    fn poke(&mut self, addr: u16, value: u8);
}

pub struct BasicMapper {
    memory_change_set: FnvHashSet<u16>,
    memory: Box<[u8; ADDRESSABLE_MEMORY]>,
}

impl Mapper for BasicMapper {
    fn new<R: Read>(reader: &mut R, mapping_start: u16) -> Self {
        let mapping_start = usize::from(mapping_start);
        let mut memory = Box::new([0; ADDRESSABLE_MEMORY]);
        reader.read_exact(&mut memory[mapping_start..]).unwrap();
        BasicMapper {
            memory,
            memory_change_set: FnvHashSet::default(),
        }
    }

    fn peek(&self, addr: u16) -> u8 {
        self.memory[usize::from(addr)]
    }

    fn poke(&mut self, addr: u16, data: u8) {
        self.memory_change_set.insert(addr);
        self.memory[usize::from(addr)] = data;
    }
}

impl Debugger for BasicMapper {
    fn read_memory_changes(&mut self) -> Vec<u16> {
        Vec::from_iter(self.memory_change_set.drain())
    }

    fn address_space_stream(&self) -> Cursor<&[u8]> {
        Cursor::new(&self.memory[..])
    }
}
