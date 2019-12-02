use std::{fs::File, io::Read};

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let (_, tokens) = asm6502::lex(buffer.as_str()).unwrap();
    println!("{} tokens", tokens.len());
    let (remaining, lines) = asm6502::parse(&tokens).unwrap();
    for (line_num, line) in lines.iter().enumerate() {
        println!("{}: {:?}", line_num + 1, line);
    }

    println!("{:?}", remaining);
    Ok(())
}
