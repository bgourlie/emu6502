use std::{collections::hash_set, io::Cursor};

pub trait Debugger {
    fn read_memory_changes<F>(&self, f: F)
    where
        F: FnOnce(hash_set::Iter<u16>);

    fn address_space_stream(&self) -> Cursor<&[u8]>;
}
