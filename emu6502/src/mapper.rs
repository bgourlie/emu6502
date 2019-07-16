use {
    crate::Debugger,
    fnv::FnvHashSet,
    std::{cell::RefCell, collections::hash_set, io::Read},
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
    memory_change_set: RefCell<FnvHashSet<u16>>,
    memory: Box<[u8; ADDRESSABLE_MEMORY]>,
}

impl Mapper for BasicMapper {
    fn new<R: Read>(reader: &mut R, mapping_start: u16) -> Self {
        let mapping_start = usize::from(mapping_start);
        let mut memory = Box::new([0; ADDRESSABLE_MEMORY]);
        reader.read_exact(&mut memory[mapping_start..]).unwrap();
        BasicMapper {
            memory,
            memory_change_set: RefCell::new(FnvHashSet::default()),
        }
    }

    fn peek(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        self.memory[addr]
    }

    fn poke(&mut self, addr: u16, data: u8) {
        self.memory_change_set.borrow_mut().insert(addr);
        let addr = addr as usize;
        self.memory[addr] = data;
    }
}

impl Debugger for BasicMapper {
    fn read_memory_changes<F>(&self, f: F)
    where
        F: FnOnce(hash_set::Iter<u16>),
    {
        let mut memory_change_set = self.memory_change_set.borrow_mut();
        f(memory_change_set.iter());
        memory_change_set.clear()
    }
}
