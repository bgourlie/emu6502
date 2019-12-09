use asm6502::Token;
use std::{fs::File, io::Read};

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let tokens: Vec<Token> = asm6502::Lexer::new(buffer.as_str())
        .map(|i| i.token())
        .collect();
    let (remaining, lines) = asm6502::parse(&tokens).unwrap();
    for (line_num, line) in lines.iter().enumerate() {
        println!("{}: {:?}", line_num + 1, line);
    }

    println!("{:?}", remaining);
    Ok(())
}
