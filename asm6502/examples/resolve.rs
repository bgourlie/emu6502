use asm6502::{Resolver, Token};

use std::{fs::File, io::Read};

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let tokens: Vec<Token> = asm6502::Lexer::new(buffer.as_str())
        .map(|(token, _, _, _)| token)
        .collect();
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
