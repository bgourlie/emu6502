use disasm6502::Disassembler;
use failure::Error;
use log::info;
use std::fs::File;

pub fn main() -> Result<(), Error> {
    pretty_env_logger::init();
    let mut f = File::open("./test_roms/6502_functional_test.bin").unwrap();
    let mut d = Disassembler::new(&mut f, 0x400, 0x400)?;
    let mut instr_count = 0;
    while let Some((addr, instr)) = d.read()? {
        instr_count += 1;
        info!("{:06} {:04X}: {:?}", instr_count, addr, instr);
    }
    Ok(())
}
