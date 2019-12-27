use asm6502::PositionedToken;
use insta::assert_debug_snapshot;
use std::{fs::File, io::Read};

#[test]
fn if_tokens() {
    assert_debug_snapshot!(Fixture::new("if_tokens").tokens());
}

#[test]
fn instruction_tokens() {
    assert_debug_snapshot!(Fixture::new("instruction_tokens").tokens());
}

#[test]
fn invalid_tokens() {
    assert_debug_snapshot!(Fixture::new("invalid_tokens").tokens());
}

struct Fixture {
    input: String,
}

impl Fixture {
    fn new(file: &'static str) -> Fixture {
        let mut file = File::open(format!("./tests/fixtures/lexer/{}.txt", file)).unwrap();
        let mut input = String::new();
        file.read_to_string(&mut input).unwrap();
        Fixture { input }
    }

    fn tokens(&self) -> Vec<PositionedToken> {
        asm6502::Lexer::new(self.input.as_str()).collect()
    }
}