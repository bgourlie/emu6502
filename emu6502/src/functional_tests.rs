use {
    super::{BasicMapper, Cpu, Mapper},
    disasm6502::Disassembly,
    std::fs::File,
};

const PC_START: u16 = 0x400;
const ADDRESS_SPACE_MAPPING_START: u16 = 0xa;
const MAX_ITERATIONS: usize = 300000000;

#[test]
fn opcodes() {
    let mut file = File::open("../test_roms/6502_functional_test.bin").unwrap();
    let mapper = BasicMapper::new(&mut file, ADDRESS_SPACE_MAPPING_START);
    let disassembly =
        Disassembly::from_stream(&mut file, ADDRESS_SPACE_MAPPING_START, Some(0x400)).unwrap();
    let mut cpu = Cpu::new(mapper);
    cpu.set_pc(PC_START);
    let mut last_pc = PC_START;

    for i in 0.. {
        if let Some(instr) = disassembly.display_at(cpu.pc()) {
            println!(
                "{:04X}: {: <15}PC    SP    A    X    Y   NVssDIZC",
                cpu.pc(),
                instr
            );
        } else {
            println!(
                "{:04X}: Unmapped address encountered: (probably disasm issue)",
                cpu.pc()
            );
        }
        cpu.step();
        println!(
            "                     {:04X}  {:02X}    {:02X}   {:02X}   {:02X}  {:08b}",
            cpu.pc(),
            cpu.sp(),
            cpu.acc(),
            cpu.x(),
            cpu.y(),
            cpu.status()
        );
        println!();
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
