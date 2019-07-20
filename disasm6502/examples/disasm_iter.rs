use disasm6502::Disassembler;
use failure::Error;
use log::info;
use std::fs::File;

pub fn main() -> Result<(), Error> {
    pretty_env_logger::init();
    let mut f = File::open("./test_roms/6502_functional_test.bin").unwrap();
    let disassembler = Disassembler::builder(&mut f)
        .with_decode_start_offset(0x400)
        .with_mapping_start_offset(0xa)
        .detect_interrupt_vectors(true)
        .build()?;
    for (addr, instr) in disassembler {
        info!("{:04X}: {:?}", addr, instr);
    }
    Ok(())
}
