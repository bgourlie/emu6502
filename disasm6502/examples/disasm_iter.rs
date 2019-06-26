use disasm6502::Disassembler;
use failure::Error;
use log::info;
use std::fs::File;

pub fn main() -> Result<(), Error> {
    pretty_env_logger::init();
    let mut f = File::open("./test_roms/6502_functional_test.bin").unwrap();
    let disassembler = Disassembler::new(&mut f, 0xa, 0x400)?;
    for (addr, instr) in disassembler {
        info!("{:04X}: {:?}", addr, instr);
    }
    Ok(())
}
