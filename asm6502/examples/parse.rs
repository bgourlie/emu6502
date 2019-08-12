use asm6502::Span;
use std::fs::File;
use std::io::Read;

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let input = Span::new(&buffer);
    let (span, tokens) = asm6502::parse(input).unwrap();
    println!("{:?}", span);
    for token in tokens {
        println!("{:?}", token);
    }
    Ok(())
}
