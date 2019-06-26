use {
    super::{Cpu, Mapper},
    disasm6502::Disassembly,
    std::{
        fs::File,
        io::{Cursor, Read},
    },
};

const PC_START: u16 = 0x400;
const ADDRESS_SPACE_MAPPING_START: u16 = 0xa;
const MAX_ITERATIONS: usize = 300000000;
const ADDRESSABLE_MEMORY: usize = 0x10000;

pub struct TestMapper {
    addr: [u8; ADDRESSABLE_MEMORY],
}

impl TestMapper {
    pub fn new() -> Self {
        TestMapper {
            addr: [0; ADDRESSABLE_MEMORY],
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
    let rom = {
        let mut f = File::open("../test_roms/6502_functional_test.bin").unwrap();
        let mut rom = Vec::<u8>::new();
        f.read_to_end(&mut rom).unwrap();
        rom
    };

    let mut mapper = TestMapper::new();
    mapper.store_many(ADDRESS_SPACE_MAPPING_START, &rom);

    let disassembly = {
        let mut cursor = Cursor::new(rom);
        Disassembly::from_rom(&mut cursor, 0xa, 0x400).unwrap()
    };

    let mut cpu = Cpu::new(mapper);
    cpu.set_pc(PC_START);
    let mut last_pc = PC_START;

    for i in 0.. {
        if let Some(instr) = disassembly.display_at(cpu.pc()) {
            println!("{:04X}: Executing {}", cpu.pc(), instr);
        } else {
            println!(
                "Unmapped address encountered: {:04X} (probably disasm issue)",
                cpu.pc()
            );
        }
        cpu.step();

        // Prevent endless loop
        if i > MAX_ITERATIONS {
            panic!("Took too many cycles to complete");
        }

        if last_pc == cpu.pc() {
            if cpu.pc() == 0x336d {
                // Success!
                break;
            } else {
                panic!("Trap detected");
            }
        }

        last_pc = cpu.pc();
    }
}
