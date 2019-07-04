#![feature(const_generics)]

#[cfg(test)]
mod functional_tests;

mod addressing_modes;
mod cpu;
mod mapper;
mod opcodes;

pub use crate::{cpu::Cpu, mapper::BasicMapper, mapper::Mapper};
