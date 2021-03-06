use std::io::Cursor;

pub trait Debugger {
    fn read_memory_changes(&mut self) -> Vec<u16>;

    fn address_space_stream(&self) -> Cursor<&[u8]>;
}
