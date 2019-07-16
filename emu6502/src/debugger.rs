use std::collections::hash_set;

pub trait Debugger {
    fn read_memory_changes<F>(&self, f: F)
    where
        F: FnOnce(hash_set::Iter<u16>);
}
