use std::{fs::File, io::Read};

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let (_, tokens) = asm6502::lex(buffer.as_str()).unwrap();
    for token in tokens {
        println!("{:?}", token);
    }
    Ok(())
}
