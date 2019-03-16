use mmu::MMU;
use num::FromPrimitive;
use opcode::*;
use std::num::Wrapping;

pub struct CPU {
    pc: u16,
    sp: u16,
    a: u8,
    f: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    m: u8,
    t: u8,
    stop: bool,
    clock: Clock,
}

struct Clock {
    m: u32,
    t: u32,
}

enum Flag {
    Z,
    N,
    H,
    C,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            a: 0,
            f: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            pc: 0,
            sp: 0,
            m: 0,
            t: 0,
            stop: false,
            clock: Clock { m: 0, t: 0 },
        }
    }

    pub fn exec(&mut self, mmu: &mut MMU) -> Result<(), &str> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
        println!("opcode: {:?}", Opcode::from_u8(mmu.read_byte(pc)).unwrap());
        match Opcode::from_u8(mmu.read_byte(pc)) {
            Some(op) => match op {
                Opcode::NOP => {
                    self.m = 1;
                }
                Opcode::LDBCnn => {
                    self.c = mmu.read_byte(self.pc);
                    self.b = mmu.read_byte(self.pc.wrapping_add(1));
                    self.pc = self.pc.wrapping_add(2);
                    self.m = 3;
                }
                Opcode::LD_BC_A => {
                    let addr = self.bc();
                    mmu.write_byte(addr, self.a);
                    self.m = 2;
                }
                Opcode::INCBC => {
                    self.c = self.c.wrapping_add(1);
                    if self.c == 0 {
                        self.b = self.b.wrapping_add(1);
                    }
                    self.m = 1;
                }
                Opcode::INCB => {
                    println!("inc b");
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.b, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.b = self.b.wrapping_add(1);
                    if self.b == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECB => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.b, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.b = self.b.wrapping_sub(1);
                    if self.b == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDBn => {
                    self.b = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RLCA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (self.a & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    self.a = self.a.rotate_left(1);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LD_nn_SP => {
                    let addr = mmu.read_word(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    mmu.write_word(addr, self.sp);
                    self.m = 5;
                }
                Opcode::ADDHLBC => {
                    self.reset_flag(Flag::N);
                    let mut hl = self.hl();
                    let bc = self.bc();
                    if check_half_carry_16(hl, bc) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_16(hl, bc) {
                        self.set_flag(Flag::C);
                    }
                    hl = hl.wrapping_add(bc);
                    self.h = (hl >> 8) as u8;
                    self.l = (hl & 0x00ff) as u8;
                    self.m = 2; // jsGb has this as 3?
                }
                Opcode::LDA_BC_ => {
                    let addr = self.bc();
                    self.a = mmu.read_byte(addr);
                    self.m = 2;
                }
                Opcode::DECBC => {
                    self.c = self.c.wrapping_sub(1);
                    if self.c == 0xff {
                        self.b = self.b.wrapping_sub(1);
                    }
                    self.m = 2; //jsGB has 1?
                }
                Opcode::INCC => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.c, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.c = self.c.wrapping_add(1);
                    if self.c == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECC => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.c, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.c = self.c.wrapping_sub(1);
                    if self.c == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDCn => {
                    self.c = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RRCA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = self.a & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    self.a = self.a.rotate_right(1);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::STOP => {
                    if self.pc == 0 {
                        self.pc = self.pc.wrapping_add(1);
                        self.stop = true;
                        self.m = 1;
                    } else {
                        return Err("Invalid opcode - STOP should be 0x10");
                    }
                }
                Opcode::LDDEnn => {
                    self.e = mmu.read_byte(self.pc);
                    self.d = mmu.read_byte(self.pc.wrapping_add(1));
                    self.pc = self.pc.wrapping_add(2);
                    self.m = 3;
                }
                Opcode::LD_DE_A => {
                    mmu.write_byte(self.de(), self.a);
                    self.m = 2;
                }
                Opcode::INCDE => {
                    self.e = self.e.wrapping_add(1);
                    if self.e != 0 {
                        self.d = self.d.wrapping_add(1);
                    }
                    self.m = 2; //jsGB has 1?
                }
                Opcode::INCD => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.d, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.d = self.d.wrapping_add(1);
                    if self.d == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECD => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.d, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.d = self.d.wrapping_sub(1);
                    if self.d == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDDn => {
                    self.d = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RLA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (self.a & 0x80) >> 7;
                    self.a = (self.a << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::JRn => {
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    if (offset & 0x80) == 0x80 {
                        self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                    } else {
                        self.pc = self.pc.wrapping_add(offset as u16);
                    }
                    self.m = 2;
                }
                Opcode::ADDHLDE => {
                    self.reset_flag(Flag::N);
                    let mut hl = self.hl();
                    let de = self.de();
                    if check_half_carry_16(hl, de) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_16(hl, de) {
                        self.set_flag(Flag::C);
                    }
                    hl = hl.wrapping_add(de);
                    self.h = (hl >> 8) as u8;
                    self.l = (hl & 0x00ff) as u8;
                    self.m = 2; // jsGB has 3
                }
                Opcode::LDA_DE_ => {
                    self.a = mmu.read_byte(self.de());
                    self.m = 2;
                }
                Opcode::DECDE => {
                    self.d = self.d.wrapping_sub(1);
                    if self.d == 0xff {
                        self.e = self.e.wrapping_sub(1);
                    }
                    self.m = 2; //jsGB has 1?
                }
                Opcode::INCE => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.d, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.d = self.d.wrapping_add(1);
                    if self.d == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECE => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.e, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.e = self.e.wrapping_sub(1);
                    if self.e == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDEn => {
                    self.e = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RRA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = self.a & 0x1;
                    self.a = (self.a >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::JRNZn => {
                    if self.test_flag(Flag::Z) == 0 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                    }
                    self.m = 2;
                }
                Opcode::LDHLnn => {
                    self.h = mmu.read_byte(self.pc);
                    self.l = mmu.read_byte(self.pc.wrapping_add(1));
                    self.pc = self.pc.wrapping_add(2);
                    self.m = 3;
                }
                Opcode::LDI_HL_A => {
                    mmu.write_byte(self.hl(), self.a);
                    self.l = self.l.wrapping_add(1);
                    if self.l != 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::INCHL => {
                    self.l = self.l.wrapping_add(1);
                    if self.l != 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::INCH => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.h, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.h = self.h.wrapping_add(1);
                    if self.h == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECH => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.h, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.h = self.h.wrapping_sub(1);
                    if self.h == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDHn => {
                    self.h = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::DAA => {
                    let mut a = self.a;
                    if self.test_flag(Flag::H) == 1 || (self.a & 0xf) > 9 {
                        a += 0x6;
                    }
                    self.reset_flag(Flag::H);
                    if self.test_flag(Flag::C) == 1 || ((self.a & 0xf0) >> 4) > 9 {
                        a += 0x60;
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    self.a = a;
                    if a == 0 {
                        self.set_flag(Flag::Z);
                    }
                }
                Opcode::JRZn => {
                    if self.test_flag(Flag::Z) == 1 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                    }
                    self.m = 2;
                }
                Opcode::ADDHLHL => {
                    self.reset_flag(Flag::N);
                    let mut hl = self.hl();
                    if check_half_carry_16(hl, hl) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_16(hl, hl) {
                        self.set_flag(Flag::C);
                    }
                    hl = hl.wrapping_add(hl);
                    self.h = (hl >> 8) as u8;
                    self.l = (hl & 0x00ff) as u8;
                    self.m = 2; // jsGB has 3
                }
                Opcode::LDIA_HL_ => {
                    self.a = mmu.read_byte(self.hl());
                    self.l = self.l.wrapping_add(1);
                    if self.l != 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::DECHL => {
                    self.l = self.l.wrapping_sub(1);
                    if self.l == 0xff {
                        self.h = self.h.wrapping_sub(1);
                    }
                    self.m = 2; //jsGB has 1?
                }
                Opcode::INCL => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.l, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.l = self.l.wrapping_add(1);
                    if self.l == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECL => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.l, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.l = self.l.wrapping_sub(1);
                    if self.l == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDLn => {
                    self.l = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::CPL => {
                    self.set_flag(Flag::N);
                    self.set_flag(Flag::H);
                    self.a ^= 0xff;
                    self.m = 1;
                }
                Opcode::JRNCn => {
                    if self.test_flag(Flag::C) == 0 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                    }
                    self.m = 2;
                }
                Opcode::LDSPnn => {
                    self.sp = mmu.read_word(self.pc);
                    self.pc = self.pc.wrapping_add(2);
                    self.m = 3;
                }
                Opcode::LDD_HL_A => {
                    mmu.write_byte(self.hl(), self.a);
                    self.l = self.l.wrapping_sub(1);
                    if self.l == 0xff {
                        self.h = self.h.wrapping_sub(1);
                    }
                    self.m = 2;
                }
                Opcode::INCSP => {
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::INC_HL_ => {
                    self.reset_flag(Flag::N);
                    let addr = self.hl();
                    let mut val = mmu.read_byte(addr);
                    if check_half_carry_8(val, 1) {
                        self.set_flag(Flag::H);
                    }
                    val = val.wrapping_add(1);
                    if val == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(addr, val);
                    self.m = 3;
                }
                Opcode::DEC_HL_ => {
                    self.set_flag(Flag::N);
                    let addr = self.hl();
                    let mut val = mmu.read_byte(addr);
                    if !check_half_borrow_8(val, 1) {
                        self.set_flag(Flag::H);
                    }
                    val = val.wrapping_sub(1);
                    if val == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(addr, val);
                    self.m = 3;
                }
                Opcode::LD_HL_n => {
                    let val = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    mmu.write_byte(self.hl(), val);
                    self.m = 3;
                }
                Opcode::SCF => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.set_flag(Flag::C);
                    self.m = 1;
                }
                Opcode::JRCn => {
                    if self.test_flag(Flag::C) == 1 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                    }
                    self.m = 2;
                }
                Opcode::ADDHLSP => {
                    self.reset_flag(Flag::N);
                    let mut hl = self.hl();
                    if check_half_carry_16(hl, self.sp) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_16(hl, self.sp) {
                        self.set_flag(Flag::C);
                    }
                    hl = hl.wrapping_add(self.sp);
                    self.h = (hl >> 8) as u8;
                    self.l = (hl & 0x00ff) as u8;
                    self.m = 2; // jsGB has 3
                }
                Opcode::LDDA_HL_ => {
                    self.a = mmu.read_byte(self.hl());
                    self.l = self.l.wrapping_sub(1);
                    if self.l == 0xff {
                        self.h = self.h.wrapping_sub(1);
                    }
                    self.m = 2;
                }
                _ => return Err("Unsupported operation."),
            },
            None => return Err("Unsupported operation."),
        }

        self.clock.m += self.m as u32;
        Ok(())
    }

    fn bc(&self) -> u16 {
        ((self.b as u16) << 8) + self.c as u16
    }

    fn de(&self) -> u16 {
        ((self.d as u16) << 8) + self.e as u16
    }

    fn hl(&self) -> u16 {
        ((self.h as u16) << 8) + self.l as u16
    }

    fn test_flag(&self, flag: Flag) -> u8 {
        match flag {
            Flag::Z => (self.f & 0x80) >> 7,
            Flag::N => (self.f & 0x40) >> 6,
            Flag::H => (self.f & 0x20) >> 5,
            Flag::C => (self.f & 0x10) >> 4,
        }
    }

    fn set_flag(&mut self, flag: Flag) {
        match flag {
            Flag::Z => self.f |= 0x80,
            Flag::N => self.f |= 0x40,
            Flag::H => self.f |= 0x20,
            Flag::C => self.f |= 0x10,
        }
    }

    fn reset_flag(&mut self, flag: Flag) {
        match flag {
            Flag::Z => self.f &= 0x7f,
            Flag::N => self.f &= 0xbf,
            Flag::H => self.f &= 0xdf,
            Flag::C => self.f &= 0xef,
        }
    }
}

fn check_half_carry_8(a: u8, b: u8) -> bool {
    ((a & 0xf) + (b & 0xf)) & 0x10 == 0x10
}

fn check_half_carry_16(a: u16, b: u16) -> bool {
    (a & 0xfff) + (b & 0xfff) & 0x1000 == 0x1000
}

fn check_carry_8(a: u8, b: u8) -> bool {
    a.wrapping_add(b) < a
}

fn check_carry_16(a: u16, b: u16) -> bool {
    a.wrapping_add(b) < a
}

fn check_half_borrow_8(a: u8, b: u8) -> bool {
    (a & 0xf) < (b & 0xf)
}

fn check_half_borrow_16(a: u16, b: u16) -> bool {
    (a & 0xfff) < (b & 0xfff)
}

fn check_borrow_8(a: u8, b: u8) -> bool {
    a < b
}

fn check_borrow_16(a: u16, b: u16) -> bool {
    a < b
}

#[cfg(test)]
mod tests {
    use super::*;
    use cartridge::Cartridge;

    fn mmu_stub(b1: u8, b2: u8, b3: u8) -> MMU {
        let mut cart_data = [0; 0x148];
        cart_data[0x0] = b1;
        cart_data[0x1] = b2;
        cart_data[0x2] = b3;
        let cart = Cartridge::new(&cart_data);
        MMU::new(cart)
    }

    #[test]
    fn test_exec() {
        let mut cpu = CPU::new();

        cpu.exec(&mut mmu_stub(0, 0, 0)).expect("CPU exec failed");
        assert_eq!(cpu.pc, 1, "program counter increases");
        assert_eq!(cpu.clock.m, 1, "advances clock");

        cpu.exec(&mut mmu_stub(0, 0, 0)).expect("CPU exec failed");
        assert_eq!(cpu.pc, 2, "program counter increases");
        assert_eq!(cpu.clock.m, 2, "advances clock");
    }

    #[test]
    fn test_check_half_carry_8() {
        assert!(check_half_carry_8(0xf, 0x1));
        assert!(!check_half_carry_8(0xf, 0x0));
    }

    #[test]
    fn test_check_half_carry_16() {
        assert!(check_half_carry_16(0xfff, 0x1));
        assert!(!check_half_carry_16(0xfff, 0x0));
    }

    #[test]
    fn test_check_carry_8() {
        assert!(check_carry_8(0xff, 0x1));
        assert!(!check_carry_8(0xfe, 0x1))
    }

    #[test]
    fn test_check_carry_16() {
        assert!(check_carry_16(0xffff, 0x1));
        assert!(!check_carry_16(0xfffe, 0x1));
    }

    #[test]
    fn test_check_half_borrow_8() {
        assert!(check_half_borrow_8(0x10, 0xf));
        assert!(!check_half_borrow_8(0xff, 0x1));
    }

    #[test]
    fn test_check_borrow_8() {
        assert!(check_borrow_8(0x1, 0x2));
        assert!(!check_borrow_8(0xff, 0x1));
    }

    #[test]
    fn test_check_half_borrow_16() {
        assert!(check_half_borrow_16(0x100, 0xf00));
        assert!(!check_half_borrow_16(0xfff, 0x1));
    }

    #[test]
    fn test_check_borrow_16() {
        assert!(check_borrow_16(0x1000, 0xf000));
        assert!(!check_borrow_16(0xf000, 0x1000));
    }

    #[test]
    fn test_incb() {
        let mut cpu = CPU::new();
        cpu.b = 0xff;
        cpu.set_flag(Flag::N);
        cpu.exec(&mut mmu_stub(Opcode::INCB as u8, 0, 0))
            .expect("CPU exec failed");
        assert_eq!(cpu.b, 0);
        assert_eq!(cpu.test_flag(Flag::Z), 1);
        assert_eq!(cpu.test_flag(Flag::N), 0);
        assert_eq!(cpu.test_flag(Flag::H), 1);
    }

    #[test]
    fn test_rlca() {
        let mut cpu = CPU::new();
        cpu.a = 0b1000_0000;
        cpu.set_flag(Flag::N);
        cpu.set_flag(Flag::H);
        cpu.exec(&mut mmu_stub(Opcode::RLCA as u8, 0, 0))
            .expect("CPU exec failed");
        assert_eq!(cpu.a, 0b0000_0001);
        assert_eq!(cpu.test_flag(Flag::N), 0);
        assert_eq!(cpu.test_flag(Flag::H), 0);
        assert_eq!(cpu.test_flag(Flag::C), 1);
    }

    #[test]
    fn test_lda_bc_() {
        let mut cpu = CPU::new();
        let mut mmu = mmu_stub(Opcode::LDA_BC_ as u8, 0, 0);
        mmu.write_byte(0xc001, 0xab);
        cpu.b = 0xc0;
        cpu.c = 0x01;

        cpu.exec(&mut mmu).expect("CPU exec failed");
        assert_eq!(cpu.a, 0xab);
    }

    #[test]
    fn test_rra() {
        let mut cpu = CPU::new();
        cpu.a = 0b1000_0000;
        cpu.set_flag(Flag::N);
        cpu.set_flag(Flag::H);
        cpu.set_flag(Flag::C);
        cpu.exec(&mut mmu_stub(Opcode::RRA as u8, 0, 0))
            .expect("CPU exec failed");
        assert_eq!(cpu.a, 0b1100_0000);
        assert_eq!(cpu.test_flag(Flag::C), 0);
        assert_eq!(cpu.test_flag(Flag::N), 0);
        assert_eq!(cpu.test_flag(Flag::H), 0);
    }

    #[test]
    fn test_jrn() {
        let mut cpu = CPU::new();
        cpu.exec(&mut mmu_stub(Opcode::JRn as u8, 0xff, 0))
            .expect("CPU exec failed");
        assert_eq!(cpu.pc, 1);

        cpu.pc = 0;
        cpu.exec(&mut mmu_stub(Opcode::JRn as u8, 0x1, 0))
            .expect("CPU exec failed");
        assert_eq!(cpu.pc, 3);
    }
}
