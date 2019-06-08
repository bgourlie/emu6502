#![cfg_attr(rustfmt, rustfmt_skip)]

use {
    crate::{addressing_modes::AddressingMode, opcodes::*, Cpu, Mapper},
};

struct TestMapper {
    memory: [u8; 0xffff],
}

impl Default for TestMapper {
    fn default() -> Self {
        TestMapper {
            memory: [0; 0xffff],
        }
    }
}

impl Mapper for TestMapper {
    fn peek(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    fn poke(&mut self, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }
}

struct Value<const VALUE: u8>;

impl<M: Mapper, const VALUE: u8> AddressingMode<M, u8> for Value<{VALUE}> {
    fn read(_cpu: &mut Cpu<M>) -> u8 {
        {VALUE}
    }
}

fn new_test_cpu() -> Cpu<TestMapper> {
    Cpu::new(TestMapper::default())
}

mod adc {
    use super::*;
    /// ## Sign and zero flag tests
    ///
    /// These tests check the presence of the sign and zero flag.
    #[test]
    fn flags_sign_and_zero_1() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<0>>(&mut cpu);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(0, cpu.acc());
    }

    #[test]
    fn flags_sign_and_zero_2() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<1>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(1, cpu.acc());
    }

    #[test]
    fn flags_sign_and_zero_3() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<255>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
        assert_eq!(255, cpu.acc());
    }

    /// ## Carry and overflow flag tests
    ///
    /// The following tests check all permutations of the
    /// 6th and 7th bits of both operands, asserting that
    /// the overflow and carry bit is set appropriately.
    ///
    /// A truth table for these tests can be found here:
    /// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    #[test]
    fn flags_carry_and_overflow_1() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<16>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_2() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<80>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_3() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<144>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_4() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<208>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_5() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<16>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_6() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<80>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_7() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<144>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_8() {
        let mut cpu = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<208>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }
}
