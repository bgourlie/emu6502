use std::{fs::File, io::Read};

fn main() -> Result<(), std::io::Error> {
    let mut file = File::open("./test_roms/6502_functional_test.a65")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    let lexer = asm6502::Lexer::lex(buffer.as_str());
    for token in lexer.tokens() {
        println!("{:?}", token);
    }
    Ok(())
}
