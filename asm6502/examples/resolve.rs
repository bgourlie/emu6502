use asm6502::Resolver;

use std::{fs::File, io::Read};

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let (_, tokens) = asm6502::lex(buffer.as_str()).unwrap();
    println!("{} tokens", tokens.len());
    let (remaining, lines) = asm6502::parse(&tokens).unwrap();
    let mut resolver = Resolver::new(lines.len());
    for line in lines.into_iter() {
        if let Err(err) = resolver.resolve_line(line) {
            println!("ERROR\n{:?}", err);
        }
    }
    resolver.print_macros();
    println!("{:?}", remaining);
    Ok(())
}
