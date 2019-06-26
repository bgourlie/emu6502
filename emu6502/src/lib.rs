#![feature(const_generics)]

#[cfg(test)]
mod functional_tests;

mod addressing_modes;
mod cpu;
mod opcodes;

pub use crate::cpu::{Cpu, Mapper};
