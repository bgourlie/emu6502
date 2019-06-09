use {
    crate::{addressing_modes::AddressingMode, opcodes::*, Cpu, Mapper},
    std::{
        cell::RefCell,
        rc::Rc
    }
};

type Memory = Rc<RefCell<[u8; 0xffff]>>;

struct TestMapper {
    memory: Memory,
}

impl TestMapper {
    fn new(memory: Memory) -> TestMapper {
        TestMapper { memory }
    }
}

impl Mapper for TestMapper {
    fn peek(&self, addr: u16) -> u8 {
        self.memory.borrow()[addr as usize]
    }
    fn poke(&mut self, addr: u16, value: u8) {
        self.memory.borrow_mut()[addr as usize] = value;
    }
}

struct Value<const VALUE: u8>;

impl<M: Mapper, const VALUE: u8> AddressingMode<M, u8, ()> for Value<{ VALUE }> {
    fn read(_cpu: &mut Cpu<M>) -> u8 {
        { VALUE }
    }
}

struct Address<const ADDR: u16>;

impl<M: Mapper, const ADDR: u16> AddressingMode<M, (u16, u8), (u16, u8)> for Address<{ ADDR }> {
    fn read(cpu: &mut Cpu<M>) -> (u16, u8) {
        ({ ADDR }, cpu.read({ ADDR }))
    }

    fn write(cpu: &mut Cpu<M>, data: (u16, u8)) {
        let (addr, data) = data;
        cpu.write(addr, data);
    }
}

fn new_test_cpu() -> (Cpu<TestMapper>, Memory) {
    let memory = Rc::new(RefCell::new([0; 0xffff]));
    let mapper = TestMapper::new(memory.clone());
    (Cpu::new(mapper), memory)
}

mod adc {
    use super::*;
    #[test]
    fn flags_sign_and_zero_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<0>>(&mut cpu);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(0, cpu.acc());
    }

    #[test]
    fn flags_sign_and_zero_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<1>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(1, cpu.acc());
    }

    #[test]
    fn flags_sign_and_zero_3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<255>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
        assert_eq!(255, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<16>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<80>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<144>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_4() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<208>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_5() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<16>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_6() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<80>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_7() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<144>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_8() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<208>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }
}

mod dex {
    use super::*;

    fn dec_base_1<F>(dec: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> u8,
    {
        let (mut cpu, mem) = new_test_cpu();
        let val = dec(&mut cpu, mem, 1);
        assert_eq!(0, val);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    fn dec_base_2<F>(dec: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> u8,
    {
        let (mut cpu, mem) = new_test_cpu();
        let val = dec(&mut cpu, mem, 2);
        assert_eq!(1, val);
        assert_eq!(false, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    fn dec_base_3<F>(dec: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> u8,
    {
        let (mut cpu, mem) = new_test_cpu();
        let val = dec(&mut cpu, mem, 254);
        assert_eq!(-3, val as i8);
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
    }

    #[test]
    fn dec_test1() {
        dec_base_1(|ref mut cpu, ref mem, val| {
            mem.borrow_mut()[0x6666] = val;
            Dec::execute::<Address<0x6666>>(cpu);
            mem.borrow()[0x6666]
        });
    }

    #[test]
    fn dec_test2() {
        dec_base_2(|ref mut cpu, ref mem, val| {
            mem.borrow_mut()[0x6666] = val;
            Dec::execute::<Address<0x6666>>(cpu);
            mem.borrow()[0x6666]
        });
    }

    #[test]
    fn dec_test3() {
        dec_base_3(|ref mut cpu, ref mem, val| {
            mem.borrow_mut()[0x6666] = val;
            Dec::execute::<Address<0x6666>>(cpu);
            mem.borrow()[0x6666]
        });
    }
}

mod sbc {
    use super::*;

    #[test]
    fn flags_sign_and_zero_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        cpu.set_carry(true);
        Sbc::execute::<Value<0>>(&mut cpu);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(0, cpu.acc());
    }

    #[test]
    fn flags_sign_and_zero_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(1);
        cpu.set_carry(true);
        Sbc::execute::<Value<1>>(&mut cpu);
        assert_eq!(0, cpu.acc());
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    #[test]
    fn flags_sign_and_zero_3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        cpu.set_carry(true);
        Sbc::execute::<Value<1>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
        assert_eq!(255, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        cpu.set_carry(true);
        Sbc::execute::<Value<240>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        cpu.set_carry(true);
        Sbc::execute::<Value<176>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        cpu.set_carry(true);
        Sbc::execute::<Value<112>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_4() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        cpu.set_carry(true);
        Sbc::execute::<Value<48>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_5() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<240>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(-32, cpu.acc() as i8);
    }

    #[test]
    fn flags_carry_and_overflow_6() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<176>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_7() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<112>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn flags_carry_and_overflow_8() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<48>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }
}
