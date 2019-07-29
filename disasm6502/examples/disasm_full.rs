use disasm6502::Disassembly;
use failure::Error;
use std::fs::File;

pub fn main() -> Result<(), Error> {
    pretty_env_logger::init();
    let mut f = File::open("./test_roms/6502_functional_test.bin").unwrap();
    let disassembly = Disassembly::from_stream(&mut f, 0xa, Some(0x400))?;
    for i in 0..0xffff {
        if let Some(label) = disassembly.label_at(i) {
            println!("{:04X}: {}", i, label)
        }
        if let Some((offset, instr)) = disassembly.instruction_at(i) {
            if offset == i {
                println!("{:04X}: {:?} {}", i, instr.opcode(), instr.operand().to_string());
            }
        }
    }

    Ok(())
}
