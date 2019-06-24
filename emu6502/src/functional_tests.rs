use std::io::Seek;
use {
    super::{Cpu, Mapper},
    disasm6502::Disassembly,
    std::{
        fs::File,
        io::{Read, SeekFrom},
    },
};

const PC_START: u16 = 0x400;
const MAX_CYCLES: usize = 100000000;
const ADDRESSABLE_MEMORY: usize = 0x10000;

pub struct TestMapper {
    addr: [u8; ADDRESSABLE_MEMORY],
    elapsed_cycles: usize,
}

impl TestMapper {
    pub fn new() -> Self {
        TestMapper {
            addr: [0; ADDRESSABLE_MEMORY],
            elapsed_cycles: 0,
        }
    }

    pub fn store_many(&mut self, addr: u16, data: &[u8]) {
        for (i, byte) in data.iter().enumerate() {
            self.poke(addr + i as u16, *byte);
        }
    }
}

impl Mapper for TestMapper {
    fn peek(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        self.addr[addr]
    }

    fn poke(&mut self, addr: u16, data: u8) {
        let addr = addr as usize;
        self.addr[addr] = data;
    }
}

#[test]
fn opcodes() {
    let mut f = File::open("../test_roms/6502_functional_test.bin").unwrap();
    let disassembly = Disassembly::from_rom(&mut f, 0x400, 0x400).unwrap();
    f.seek(SeekFrom::Start(0));
    let mut rom = Vec::<u8>::new();
    f.read_to_end(&mut rom).unwrap();
    let mut mapper = TestMapper::new();
    mapper.store_many(PC_START, &rom);
    let mut cpu = Cpu::new(mapper);
    cpu.set_pc(PC_START);
    let mut last_pc = PC_START;

    loop {
        if let Some(instr) = disassembly.display_at(cpu.pc()) {
            println!("{:04X}: Executing {}", cpu.pc(), instr);
        } else {
            println!("{:04X}: Executing unknown instruction", cpu.pc());
        }
        cpu.step();
        // Prevent endless loop
        //        if cpu.interconnect.elapsed_cycles() > MAX_CYCLES {
        //            assert!(false, "Took too many cycles to complete");
        //        }

        if last_pc == cpu.pc() {
            if cpu.pc() == 0x3367 {
                // Success!
                break;
            } else {
                assert!(false, "Trap detected");
            }
        }

        last_pc = cpu.pc();
    }
}
