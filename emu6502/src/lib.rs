#![feature(const_generics)]

mod addressing_modes;
mod cpu;
mod opcodes;
mod util;

pub use crate::cpu::{Cpu, Mapper};
