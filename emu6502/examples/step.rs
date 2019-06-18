use emu6502::{Cpu, Mapper};

fn main() {
    let mut cpu = Box::new(Cpu::new(SimpleMapper {
        memory: [0; 0xffff],
    }));

    cpu.step();
}

struct SimpleMapper {
    memory: [u8; 0xffff],
}

impl Mapper for SimpleMapper {
    fn peek(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    fn poke(&mut self, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }
}
