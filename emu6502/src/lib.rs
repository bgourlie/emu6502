#![feature(const_generics)]

#[cfg(test)]
mod functional_tests;

mod addressing_modes;
mod cpu;
mod opcodes;
mod util;

pub use crate::cpu::{Cpu, Mapper};
