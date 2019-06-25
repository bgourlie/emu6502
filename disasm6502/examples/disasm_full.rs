use disasm6502::Disassembly;
use failure::Error;
use std::fs::File;

pub fn main() -> Result<(), Error> {
    pretty_env_logger::init();
    let mut f = File::open("./test_roms/6502_functional_test.bin").unwrap();
    let disassembly = Disassembly::from_rom(&mut f, 0x0, 0x400)?;
    for i in 0..0xffff {
        if let Some(instr) = disassembly.display_at(i) {
            println!("{:04X}: {}", i, instr);
        }
    }
    Ok(())
}
