use {
    crate::{
        addressing_modes::{AddressingMode, Implied},
        opcodes::*,
        Cpu, Mapper,
    },
    std::{cell::RefCell, rc::Rc},
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

struct RelativeAddress<const ADDR: i8>;

impl<M: Mapper, const ADDR: i8> AddressingMode<M, i8, ()> for RelativeAddress<{ ADDR }> {
    fn read(_cpu: &mut Cpu<M>) -> i8 {
        { ADDR }
    }
}

impl<M: Mapper, const ADDR: u16> AddressingMode<M, u8, ()> for Address<{ ADDR }> {
    fn read(cpu: &mut Cpu<M>) -> u8 {
        cpu.read({ ADDR })
    }
}

fn new_test_cpu() -> (Cpu<TestMapper>, Memory) {
    let memory = Rc::new(RefCell::new([0; 0xffff]));
    let mapper = TestMapper::new(memory.clone());
    (Cpu::new(mapper), memory)
}

mod branch {
    use super::*;
    fn test_branch_not_crossing_page_boundary_positive_offset<B, F>(setup: F)
    where
        B: Branch<TestMapper>,
        F: Fn(&mut Cpu<TestMapper>),
    {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_pc(43656);
        setup(&mut cpu);
        B::execute::<RelativeAddress<5>>(&mut cpu);
        assert_eq!(43661, cpu.pc());
    }

    fn test_branch_not_crossing_page_boundary_negative_offset<B, F>(setup: F)
        where
            B: Branch<TestMapper>,
            F: Fn(&mut Cpu<TestMapper>),
    {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_pc(43656);
        setup(&mut cpu);
        B::execute::<RelativeAddress<-1>>(&mut cpu);
        assert_eq!(43655, cpu.pc());
    }

    fn test_no_branch<B, F>(setup: F)
        where
            B: Branch<TestMapper>,
            F: Fn(&mut Cpu<TestMapper>),
    {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_pc(30);
        setup(&mut cpu);
        B::execute::<RelativeAddress<-20>>(&mut cpu);
        assert_eq!(30, cpu.pc());
    }

    #[test]
    fn bpl_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bpl, _>(|ref mut cpu| {
            cpu.set_sign(false);
        });
    }

    #[test]
    fn bpl_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bpl, _>(|ref mut cpu| {
            cpu.set_sign(false);
        });
    }

    #[test]
    fn bpl_no_branch() {
        test_no_branch::<Bpl, _>(|ref mut cpu| {
            cpu.set_sign(true);
        });
    }
    
    #[test]
    fn bcc_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bcc, _>(|ref mut cpu| {
            cpu.set_carry(false);
        });
    }

    #[test]
    fn bcc_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bcc, _>(|ref mut cpu| {
            cpu.set_carry(false);
        });
    }

    #[test]
    fn bcc_no_branch() {
        test_no_branch::<Bcc, _>(|ref mut cpu| {
            cpu.set_carry(true);
        });
    }

    #[test]
    fn bcs_not_crossing_page_boundary_positive_rel_addr() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bcs, _>(|ref mut cpu| {
            cpu.set_carry(true);
        });
    }

    #[test]
    fn bcs_not_crossing_page_boundary_negative_rel_addr() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bcs, _>(|ref mut cpu| {
            cpu.set_carry(true);
        });
    }

    #[test]
    fn bcs_no_branch() {
        test_no_branch::<Bcs, _>(|ref mut cpu| {
            cpu.set_carry(false);
        });
    }

    #[test]
    fn beq_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Beq, _>(|ref mut cpu| {
            cpu.set_zero(true);
        });
    }

    #[test]
    fn beq_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Beq, _>(|ref mut cpu| {
            cpu.set_zero(true);
        });
    }

    #[test]
    fn beq_no_branch() {
        test_no_branch::<Beq, _>(|ref mut cpu| {
            cpu.set_zero(false);
        });
    }

    #[test]
    fn bmi_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bmi, _>(|ref mut cpu| {
            cpu.set_sign(true);
        });
    }

    #[test]
    fn bmi_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bmi, _>(|ref mut cpu| {
            cpu.set_sign(true);
        });
    }

    #[test]
    fn bmi_no_branch() {
        test_no_branch::<Bmi, _>(|ref mut cpu| {
            cpu.set_sign(false);
        });
    }

    #[test]
    fn bne_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bne, _>(|ref mut cpu| {
            cpu.set_zero(false);
        });
    }

    #[test]
    fn bne_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bne, _>(|ref mut cpu| {
            cpu.set_zero(false);
        });
    }

    #[test]
    fn bne_no_branch() {
        test_no_branch::<Bne, _>(|ref mut cpu| {
            cpu.set_zero(true);
        });
    }

    #[test]
    fn bvc_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bvc, _>(|ref mut cpu| {
            cpu.set_overflow(false);
        });
    }

    #[test]
    fn bvc_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bvc, _>(|ref mut cpu| {
            cpu.set_overflow(false);
        });
    }

    #[test]
    fn bvc_no_branch() {
        test_no_branch::<Bvc, _>(|ref mut cpu| {
            cpu.set_overflow(true);
        });
    }

    #[test]
    fn bvs_not_crossing_page_boundary_positive_offset() {
        test_branch_not_crossing_page_boundary_positive_offset::<Bvs, _>(|ref mut cpu| {
            cpu.set_overflow(true);
        });
    }

    #[test]
    fn bvs_not_crossing_page_boundary_negative_offset() {
        test_branch_not_crossing_page_boundary_negative_offset::<Bvs, _>(|ref mut cpu| {
            cpu.set_overflow(true);
        });
    }

    #[test]
    fn bvs_no_branch() {
        test_no_branch::<Bvs, _>(|ref mut cpu| {
            cpu.set_overflow(false);
        });
    }
}

mod adc_sbc {
    use super::*;
    #[test]
    fn adc_flags_sign_and_zero_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<0>>(&mut cpu);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(0, cpu.acc());
    }

    #[test]
    fn adc_flags_sign_and_zero_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<1>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(1, cpu.acc());
    }

    #[test]
    fn adc_flags_sign_and_zero_3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Adc::execute::<Value<255>>(&mut cpu);
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
        assert_eq!(255, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<16>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<80>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<144>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_4() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(80);
        Adc::execute::<Value<208>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_5() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<16>>(&mut cpu);
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(224, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_6() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<80>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_7() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<144>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn adc_flags_carry_and_overflow_8() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        Adc::execute::<Value<208>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }

    #[test]
    fn sbc_flags_sign_and_zero_1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        cpu.set_carry(true);
        Sbc::execute::<Value<0>>(&mut cpu);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
        assert_eq!(0, cpu.acc());
    }

    #[test]
    fn sbc_flags_sign_and_zero_2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(1);
        cpu.set_carry(true);
        Sbc::execute::<Value<1>>(&mut cpu);
        assert_eq!(0, cpu.acc());
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    #[test]
    fn sbc_flags_sign_and_zero_3() {
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
    fn sbc_flags_carry_and_overflow_6() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<176>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(32, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_7() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<112>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(true, cpu.overflow());
        assert_eq!(96, cpu.acc());
    }

    #[test]
    fn sbc_flags_carry_and_overflow_8() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(208);
        cpu.set_carry(true);
        Sbc::execute::<Value<48>>(&mut cpu);
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.overflow());
        assert_eq!(160, cpu.acc());
    }
}

mod increment_decrement {
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

    fn inc_base_1<F>(inc: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> u8,
    {
        let (mut cpu, mem) = new_test_cpu();
        let val = inc(&mut cpu, mem, 1);
        assert_eq!(2, val);
        assert_eq!(false, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    fn inc_base_2<F>(inc: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> u8,
    {
        let (mut cpu, mem) = new_test_cpu();
        let val = inc(&mut cpu, mem, 255);
        assert_eq!(0, val);
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    fn inc_base_3<F>(inc: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> u8,
    {
        let (mut cpu, mem) = new_test_cpu();
        let val = inc(&mut cpu, mem, 254);
        assert_eq!(-1, val as i8);
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

    #[test]
    fn dex_test1() {
        dec_base_1(|ref mut cpu, ref _mem, val| {
            cpu.set_x(val);
            Dex::execute::<Implied>(cpu);
            cpu.x()
        });
    }

    #[test]
    fn dex_test2() {
        dec_base_2(|ref mut cpu, ref _mem, val| {
            cpu.set_x(val);
            Dex::execute::<Implied>(cpu);
            cpu.x()
        });
    }

    #[test]
    fn dex_test3() {
        dec_base_3(|ref mut cpu, ref _mem, val| {
            cpu.set_x(val);
            Dex::execute::<Implied>(cpu);
            cpu.x()
        });
    }

    #[test]
    fn dey_test1() {
        dec_base_1(|ref mut cpu, ref _mem, val| {
            cpu.set_y(val);
            Dey::execute::<Implied>(cpu);
            cpu.y()
        });
    }

    #[test]
    fn dey_test2() {
        dec_base_2(|ref mut cpu, ref _mem, val| {
            cpu.set_y(val);
            Dey::execute::<Implied>(cpu);
            cpu.y()
        });
    }

    #[test]
    fn dey_test3() {
        dec_base_3(|ref mut cpu, ref _mem, val| {
            cpu.set_y(val);
            Dey::execute::<Implied>(cpu);
            cpu.y()
        });
    }

    #[test]
    fn inc_test1() {
        inc_base_1(|ref mut cpu, ref mem, val| {
            mem.borrow_mut()[0x6666] = val;
            Inc::execute::<Address<0x6666>>(cpu);
            mem.borrow()[0x6666]
        });
    }

    #[test]
    fn inc_test2() {
        inc_base_2(|ref mut cpu, ref mem, val| {
            mem.borrow_mut()[0x6666] = val;
            Inc::execute::<Address<0x6666>>(cpu);
            mem.borrow()[0x6666]
        });
    }

    #[test]
    fn inc_test3() {
        inc_base_3(|ref mut cpu, ref mem, val| {
            mem.borrow_mut()[0x6666] = val;
            Inc::execute::<Address<0x6666>>(cpu);
            mem.borrow()[0x6666]
        });
    }

    #[test]
    fn iny_test1() {
        inc_base_1(|ref mut cpu, ref _mem, val| {
            cpu.set_y(val);
            Iny::execute::<Implied>(cpu);
            cpu.y()
        });
    }

    #[test]
    fn iny_test2() {
        inc_base_2(|ref mut cpu, ref _mem, val| {
            cpu.set_y(val);
            Iny::execute::<Implied>(cpu);
            cpu.y()
        });
    }

    #[test]
    fn iny_test3() {
        inc_base_3(|ref mut cpu, ref _mem, val| {
            cpu.set_y(val);
            Iny::execute::<Implied>(cpu);
            cpu.y()
        });
    }

    #[test]
    fn inx_test1() {
        inc_base_1(|ref mut cpu, ref _mem, val| {
            cpu.set_x(val);
            Inx::execute::<Implied>(cpu);
            cpu.x()
        });
    }

    #[test]
    fn inx_test2() {
        inc_base_2(|ref mut cpu, ref _mem, val| {
            cpu.set_x(val);
            Inx::execute::<Implied>(cpu);
            cpu.x()
        });
    }

    #[test]
    fn inx_test3() {
        inc_base_3(|ref mut cpu, ref _mem, val| {
            cpu.set_x(val);
            Inx::execute::<Implied>(cpu);
            cpu.x()
        });
    }
}

mod shifts {
    use super::*;

    fn shift_left_base_1<F>(do_shift: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> (u8, bool),
    {
        const VAL: u8 = 0b10000001;
        let (mut cpu, mem) = new_test_cpu();

        cpu.set_carry(true);
        let (result, rotate) = do_shift(&mut cpu, mem, VAL);

        if rotate {
            assert_eq!(0b00000011, result);
        } else {
            assert_eq!(0b00000010, result);
        }

        assert_eq!(false, cpu.sign());
        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.zero());
    }

    fn shift_left_base_2<F>(do_shift: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> (u8, bool),
    {
        const VAL: u8 = 0b01000000;
        let (mut cpu, mem) = new_test_cpu();

        let (result, _) = do_shift(&mut cpu, mem, VAL);

        assert_eq!(0b10000000, result);
        assert_eq!(true, cpu.sign());
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.zero());
    }

    fn shift_left_base_3<F>(do_shift: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> (u8, bool),
    {
        const VAL: u8 = 0b00000000;
        let (mut cpu, mem) = new_test_cpu();

        let (result, _) = do_shift(&mut cpu, mem, VAL);

        assert_eq!(0b00000000, result);
        assert_eq!(false, cpu.sign());
        assert_eq!(false, cpu.carry());
        assert_eq!(true, cpu.zero());
    }

    fn shift_right_base_1<F>(do_shift: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> (u8, bool),
    {
        const VAL: u8 = 0b10000001;

        let (mut cpu, mem) = new_test_cpu();

        cpu.set_carry(true);
        let (result, rotate) = do_shift(&mut cpu, mem, VAL);

        if rotate {
            assert_eq!(0b11000000, result);
            assert_eq!(true, cpu.sign());
        } else {
            assert_eq!(0b01000000, result);
            assert_eq!(false, cpu.sign());
        }

        assert_eq!(true, cpu.carry());
        assert_eq!(false, cpu.zero());
    }

    fn shift_right_base_2<F>(do_shift: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> (u8, bool),
    {
        const VAL: u8 = 0b01000000;

        let (mut cpu, mem) = new_test_cpu();

        let (result, _) = do_shift(&mut cpu, mem, VAL);

        assert_eq!(0b00100000, result);
        assert_eq!(false, cpu.sign());
        assert_eq!(false, cpu.carry());
        assert_eq!(false, cpu.zero());
    }

    fn shift_right_base_3<F>(do_shift: F)
    where
        F: Fn(&mut Cpu<TestMapper>, Memory, u8) -> (u8, bool),
    {
        const VAL: u8 = 0b00000000;

        let (mut cpu, mem) = new_test_cpu();

        let (result, _) = do_shift(&mut cpu, mem, VAL);

        assert_eq!(0b00000000, result);
        assert_eq!(false, cpu.sign());
        assert_eq!(false, cpu.carry());
        assert_eq!(true, cpu.zero());
    }

    fn rol(cpu: &mut Cpu<TestMapper>, mem: Memory, val: u8) -> (u8, bool) {
        mem.borrow_mut()[0x6666] = val;
        Rol::execute::<Address<0x6666>>(cpu);
        (mem.borrow()[0x6666], true)
    }

    #[test]
    fn rol_1() {
        shift_left_base_1(rol);
    }

    #[test]
    fn rol_2() {
        shift_left_base_2(rol);
    }

    #[test]
    fn rol_3() {
        shift_left_base_3(rol);
    }

    fn ror(cpu: &mut Cpu<TestMapper>, mem: Memory, val: u8) -> (u8, bool) {
        mem.borrow_mut()[0x6666] = val;
        Ror::execute::<Address<0x6666>>(cpu);
        (mem.borrow()[0x6666], true)
    }

    #[test]
    fn ror_1() {
        shift_right_base_1(ror);
    }

    #[test]
    fn ror_2() {
        shift_right_base_2(ror);
    }

    #[test]
    fn ror_3() {
        shift_right_base_3(ror);
    }
}

mod bitwise {
    use super::*;

    #[test]
    fn and1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        And::execute::<Value<255>>(&mut cpu);
        assert_eq!(0, cpu.acc());
        assert_eq!(true, cpu.zero());
        assert_eq!(false, cpu.sign());
    }

    #[test]
    fn and2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0b11110000_u8);
        And::execute::<Value<0b10101010>>(&mut cpu);
        assert_eq!(0b10100000, cpu.acc());
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
    }

    #[test]
    fn ora1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Ora::execute::<Value<0b11111111>>(&mut cpu);
        assert_eq!(0b11111111, cpu.acc());
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
    }

    #[test]
    fn ora2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0b11110000_u8);
        Ora::execute::<Value<0b10101010>>(&mut cpu);
        assert_eq!(0b11111010, cpu.acc());
        assert_eq!(false, cpu.zero());
        assert_eq!(true, cpu.sign());
    }

    #[test]
    fn bit_zero_flag_behavior1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Bit::execute::<Value<0>>(&mut cpu);
        assert_eq!(true, cpu.zero());
    }

    #[test]
    fn bit_zero_flag_behavior2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0b11110000);
        Bit::execute::<Value<0b00001111>>(&mut cpu);
        assert_eq!(true, cpu.zero());
    }

    #[test]
    fn bit_zero_flag_behavior3() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0b00111100);
        Bit::execute::<Value<0b00011000>>(&mut cpu);
        assert_eq!(false, cpu.zero());
    }

    #[test]
    fn bit_sign_flag_behavior1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Bit::execute::<Value<0b01111111>>(&mut cpu);
        assert_eq!(false, cpu.sign());
    }

    #[test]
    fn bit_sign_flag_behavior2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Bit::execute::<Value<0b10000000>>(&mut cpu);
        assert_eq!(true, cpu.sign());
    }

    #[test]
    fn bit_overflow_flag_behavior1() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Bit::execute::<Value<0b10111111>>(&mut cpu);
        assert_eq!(false, cpu.overflow());
    }

    #[test]
    fn bit_overflow_flag_behavior2() {
        let (mut cpu, _) = new_test_cpu();
        cpu.set_acc(0);
        Bit::execute::<Value<0b01000000>>(&mut cpu);
        assert_eq!(true, cpu.overflow());
    }
}
