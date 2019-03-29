use emulator::IntFlag;
use mmu::MMU;
use num::FromPrimitive;
use opcode::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone, Serialize)]
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
    pub ime: bool,
    pub stop: bool,
    pub halt: bool,
    clock: Clock,
}

#[wasm_bindgen]
#[derive(Clone, Serialize)]
struct Clock {
    m: u32,
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
            halt: false,
            ime: true,
            clock: Clock { m: 0 },
        }
    }

    pub fn post_bios(&mut self) {
        self.pc = 0x100;
        self.sp = 0xfffe;
        self.h = 0x01;
        self.l = 0x4d;
        self.c = 0x13;
        self.e = 0xd8;
        self.a = 0x01;
    }

    pub fn m(&self) -> u8 {
        self.m
    }

    pub fn clock_m(&self) -> u32 {
        self.clock.m
    }

    pub fn halt(&mut self) -> bool {
        if self.halt {
            self.clock.m += 1;
        }
        self.halt
    }

    pub fn stop(&self) -> bool {
        self.stop
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

    pub fn handle_interrupt(&mut self, iflag: IntFlag, mmu: &mut MMU) {
        self.sp = self.sp.wrapping_sub(2);
        mmu.write_word(self.sp, self.pc);
        match iflag {
            IntFlag::Vblank => self.pc = 0x40,
            IntFlag::LCDC => self.pc = 0x40,
            IntFlag::TimerOverflow => self.pc = 0x50,
            IntFlag::SerialIO => self.pc = 0x58,
            IntFlag::JoyPad => self.pc = 0x60,
        }
        self.clock.m += 4;
    }

    pub fn exec(&mut self, mmu: &mut MMU) -> Result<(), String> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
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
                        return Err("Invalid opcode - STOP should be 0x10".to_owned());
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
                    self.m = 3;
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
                    self.m = 2;
                    if self.test_flag(Flag::Z) == 0 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                        self.m += 1;
                    }
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
                    self.m = 2;
                    if self.test_flag(Flag::Z) == 1 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                        self.m += 1;
                    }
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
                    self.m = 2;
                    if self.test_flag(Flag::C) == 0 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                        self.m += 1;
                    }
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
                    self.m = 2;
                    if self.test_flag(Flag::C) == 1 {
                        let offset = mmu.read_byte(self.pc);
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                        self.m += 1;
                    }
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
                Opcode::DECSP => {
                    self.sp = self.sp.wrapping_sub(1);
                    self.m = 2;
                }
                Opcode::INCA => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.a = self.l.wrapping_add(1);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::DECA => {
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, 1) {
                        self.set_flag(Flag::H);
                    }
                    self.a = self.a.wrapping_sub(1);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::LDAn => {
                    self.a = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::CCF => {
                    if self.test_flag(Flag::C) == 1 {
                        self.reset_flag(Flag::C);
                    } else {
                        self.set_flag(Flag::C)
                    }
                    self.m = 1;
                }
                Opcode::LDBB => {
                    self.m = 1;
                }
                Opcode::LDBC => {
                    self.b = self.c;
                    self.m = 1;
                }
                Opcode::LDBD => {
                    self.b = self.d;
                    self.m = 1;
                }
                Opcode::LDBE => {
                    self.b = self.e;
                    self.m = 1;
                }
                Opcode::LDBH => {
                    self.b = self.h;
                    self.m = 1;
                }
                Opcode::LDBL => {
                    self.b = self.l;
                    self.m = 1;
                }
                Opcode::LDB_HL_ => {
                    self.b = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDBA => {
                    self.b = self.a;
                    self.m = 1;
                }

                Opcode::LDCB => {
                    self.c = self.b;
                    self.m = 1;
                }
                Opcode::LDCC => {
                    self.m = 1;
                }
                Opcode::LDCD => {
                    self.c = self.d;
                    self.m = 1;
                }
                Opcode::LDCE => {
                    self.c = self.e;
                    self.m = 1;
                }
                Opcode::LDCH => {
                    self.c = self.h;
                    self.m = 1;
                }
                Opcode::LDCL => {
                    self.c = self.l;
                    self.m = 1;
                }
                Opcode::LDC_HL_ => {
                    self.c = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDCA => {
                    self.c = self.a;
                    self.m = 1;
                }

                Opcode::LDDB => {
                    self.d = self.b;
                    self.m = 1;
                }
                Opcode::LDDC => {
                    self.d = self.c;
                    self.m = 1;
                }
                Opcode::LDDD => {
                    self.m = 1;
                }
                Opcode::LDDE => {
                    self.d = self.e;
                    self.m = 1;
                }
                Opcode::LDDH => {
                    self.d = self.h;
                    self.m = 1;
                }
                Opcode::LDDL => {
                    self.d = self.l;
                    self.m = 1;
                }
                Opcode::LDD_HL_ => {
                    self.d = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDDA => {
                    self.d = self.a;
                    self.m = 1;
                }
                Opcode::LDEB => {
                    self.e = self.b;
                    self.m = 1;
                }
                Opcode::LDEC => {
                    self.e = self.c;
                    self.m = 1;
                }
                Opcode::LDED => {
                    self.e = self.d;
                    self.m = 1;
                }
                Opcode::LDEE => {
                    self.m = 1;
                }
                Opcode::LDEH => {
                    self.e = self.h;
                    self.m = 1;
                }
                Opcode::LDEL => {
                    self.e = self.l;
                    self.m = 1;
                }
                Opcode::LDE_HL_ => {
                    self.e = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDEA => {
                    self.e = self.a;
                    self.m = 1;
                }
                Opcode::LDHB => {
                    self.h = self.b;
                    self.m = 1;
                }
                Opcode::LDHC => {
                    self.h = self.c;
                    self.m = 1;
                }
                Opcode::LDHD => {
                    self.h = self.d;
                    self.m = 1;
                }
                Opcode::LDHE => {
                    self.h = self.e;
                    self.m = 1;
                }
                Opcode::LDHH => {
                    self.m = 1;
                }
                Opcode::LDHL => {
                    self.h = self.l;
                    self.m = 1;
                }
                Opcode::LDH_HL_ => {
                    self.h = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDHA => {
                    self.h = self.a;
                    self.m = 1;
                }
                Opcode::LDLB => {
                    self.l = self.b;
                    self.m = 1;
                }
                Opcode::LDLC => {
                    self.l = self.c;
                    self.m = 1;
                }
                Opcode::LDLD => {
                    self.l = self.d;
                    self.m = 1;
                }
                Opcode::LDLE => {
                    self.l = self.e;
                    self.m = 1;
                }
                Opcode::LDLH => {
                    self.l = self.h;
                    self.m = 1;
                }
                Opcode::LDLL => {
                    self.m = 1;
                }
                Opcode::LDL_HL_ => {
                    self.l = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDLA => {
                    self.l = self.a;
                    self.m = 1;
                }
                Opcode::LD_HL_B => {
                    mmu.write_byte(self.hl(), self.b);
                    self.m = 2;
                }
                Opcode::LD_HL_C => {
                    mmu.write_byte(self.hl(), self.c);
                    self.m = 2;
                }
                Opcode::LD_HL_D => {
                    mmu.write_byte(self.hl(), self.d);
                    self.m = 2;
                }
                Opcode::LD_HL_E => {
                    mmu.write_byte(self.hl(), self.e);
                    self.m = 2;
                }
                Opcode::LD_HL_H => {
                    mmu.write_byte(self.hl(), self.h);
                    self.m = 2;
                }
                Opcode::LD_HL_L => {
                    mmu.write_byte(self.hl(), self.l);
                    self.m = 2;
                }
                Opcode::HALT => {
                    self.halt = true;
                    self.m = 1;
                }
                Opcode::LD_HL_A => {
                    mmu.write_byte(self.hl(), self.a);
                    self.m = 2;
                }
                Opcode::LDAB => {
                    self.a = self.b;
                    self.m = 1;
                }
                Opcode::LDAC => {
                    self.a = self.c;
                    self.m = 1;
                }
                Opcode::LDAD => {
                    self.a = self.d;
                    self.m = 1;
                }
                Opcode::LDAE => {
                    self.a = self.e;
                    self.m = 1;
                }
                Opcode::LDAH => {
                    self.a = self.h;
                    self.m = 1;
                }
                Opcode::LDAL => {
                    self.a = self.l;
                    self.m = 1;
                }
                Opcode::LDA_HL_ => {
                    self.a = mmu.read_byte(self.hl());
                    self.m = 2;
                }
                Opcode::LDAA => {
                    self.m = 1;
                }
                Opcode::ADDAB => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.b) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.b) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.b);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADDAC => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.c) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.c) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.c);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADDAD => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.d) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.d) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.d);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADDAE => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.e) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.e) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.e);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADDAH => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.h) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.h) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.h);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADDAL => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.l) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.l) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.l);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADDA_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::ADDAA => {
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, self.a) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, self.a) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(self.a);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCAB => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.b.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCAC => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.c.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCAD => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.d.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCAE => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.e.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCAH => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.h.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCAL => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.l.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ADCA_HL_ => {
                    let carry = self.test_flag(Flag::C);
                    let mut value = mmu.read_byte(self.hl());
                    value = value.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::ADCAA => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.a.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBAB => {
                    let value = self.b;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBAC => {
                    let value = self.c;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBAD => {
                    let value = self.d;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBAE => {
                    let value = self.e;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBAH => {
                    let value = self.h;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBAL => {
                    let value = self.l;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SUBA_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::SUBAA => {
                    let value = self.a;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCAB => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.b.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCAC => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.c.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCAD => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.d.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCAE => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.e.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCAH => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.h.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCAL => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.l.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::SBCA_HL_ => {
                    let carry = self.test_flag(Flag::C);
                    let mut value = mmu.read_byte(self.hl());
                    value = value.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::SBCAA => {
                    let carry = self.test_flag(Flag::C);
                    let value = self.a.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ANDB => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.b;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ANDC => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.c;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ANDD => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.d;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ANDE => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.e;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ANDH => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.h;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ANDL => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.l;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::AND_HL_ => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= mmu.read_byte(self.hl());
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::ANDA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::C);
                    self.set_flag(Flag::H);
                    self.a &= self.a;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XORB => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.b;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XORC => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.c;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XORD => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.d;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XORE => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.e;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XORH => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.h;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XORL => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.l;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::XOR_HL_ => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= mmu.read_byte(self.hl());
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::XORA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a ^= self.a;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ORB => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.a;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ORC => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.c;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ORD => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.d;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ORE => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.e;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ORH => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.h;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::ORL => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.l;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::OR_HL_ => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= mmu.read_byte(self.hl());
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::ORA => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    self.a |= self.a;
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CPB => {
                    let value = self.b;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CPC => {
                    let value = self.c;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CPD => {
                    let value = self.d;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CPE => {
                    let value = self.e;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CPH => {
                    let value = self.h;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CP_L => {
                    let value = self.l;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::CP_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::CPA => {
                    let value = self.a;
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 1;
                }
                Opcode::RETNZ => {
                    self.m = 2; //jsGB has 3 (1 + 2 if true)?
                    if self.test_flag(Flag::Z) == 0 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                        self.m += 3;
                    }
                }
                Opcode::POPBC => {
                    self.c = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.b = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 3;
                }
                Opcode::JPNZnn => {
                    self.m = 3;
                    if self.test_flag(Flag::Z) == 0 {
                        self.pc = mmu.read_word(self.pc);
                        self.m += 1;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::JPnn => {
                    self.pc = mmu.read_word(self.pc);
                    self.m = 4;
                }
                Opcode::CALLNZnn => {
                    self.m = 3;
                    if self.test_flag(Flag::Z) == 0 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                        self.m += 3;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::PUSHBC => {
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.b);
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.c);
                    self.m = 4;
                }
                Opcode::ADDAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::RST0 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0;
                    self.m = 4;
                }
                Opcode::RETZ => {
                    self.m = 2;
                    if self.test_flag(Flag::Z) == 1 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                        self.m += 3;
                    }
                }
                Opcode::RET => {
                    self.pc = mmu.read_word(self.sp);
                    self.sp = self.sp.wrapping_add(2);
                    self.m = 4;
                }
                Opcode::JPZnn => {
                    if self.test_flag(Flag::Z) == 1 {
                        self.pc = mmu.read_word(self.pc);
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                    self.m = 3;
                }
                Opcode::ExtOps => return self.exec_ext(mmu),
                Opcode::CALLZnn => {
                    self.m = 3;
                    if self.test_flag(Flag::Z) == 1 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                        self.m += 3;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::CALLnn => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc.wrapping_add(2));
                    self.pc = mmu.read_word(self.pc);
                    self.m = 6;
                }
                Opcode::ADCAn => {
                    let carry = self.test_flag(Flag::C);
                    let mut value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    value = value.wrapping_add(carry);
                    self.reset_flag(Flag::N);
                    if check_half_carry_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_carry_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_add(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::RST8 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x8;
                    self.m = 4;
                }
                Opcode::RETNC => {
                    self.m = 2;
                    if self.test_flag(Flag::C) == 0 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                        self.m += 3;
                    }
                }
                Opcode::POPDE => {
                    self.e = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.d = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 3;
                }
                Opcode::JPNCnn => {
                    self.m = 3;
                    if self.test_flag(Flag::C) == 0 {
                        self.pc = mmu.read_word(self.pc);
                        self.m += 1;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::CALLNCnn => {
                    self.m = 3;
                    if self.test_flag(Flag::C) == 0 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                        self.m += 3;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::PUSHDE => {
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.d);
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.e);
                    self.m = 4;
                }
                Opcode::SUBAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::RST10 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x10;
                    self.m = 4;
                }
                Opcode::RETC => {
                    self.m = 2;
                    if self.test_flag(Flag::C) == 1 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                        self.m += 3;
                    }
                }
                Opcode::RETI => {
                    self.pc = mmu.read_word(self.sp);
                    self.sp = self.sp.wrapping_add(2);
                    self.ime = true;
                    self.m = 4;
                }
                Opcode::JPCnn => {
                    self.m = 3;
                    if self.test_flag(Flag::C) == 1 {
                        self.pc = mmu.read_word(self.pc);
                        self.m += 1;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::CALLCnn => {
                    self.m = 3;
                    if self.test_flag(Flag::C) == 1 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                        self.m += 3;
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                }
                Opcode::SBCAn => {
                    let carry = self.test_flag(Flag::C);
                    let mut value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    value = value.wrapping_add(carry);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    self.a = self.a.wrapping_sub(value);
                    if self.a == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::RST18 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x18;
                    self.m = 4;
                }
                Opcode::LDH_n_A => {
                    let n = mmu.read_byte(self.pc);
                    self.pc.wrapping_add(1);
                    let addr = 0xff00 + (n as u16);
                    mmu.write_byte(addr, self.a);
                    self.m = 3;
                }
                Opcode::POPHL => {
                    self.l = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.h = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 3;
                }
                Opcode::LD_C_A => {
                    let addr = 0xff00 + (self.c as u16);
                    mmu.write_byte(addr, self.a);
                    self.m = 2;
                }
                Opcode::PUSHHL => {
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.h);
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.l);
                    self.m = 4;
                }
                Opcode::ANDn => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.a &= n;
                    self.m = 2;
                }
                Opcode::RST20 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x20;
                    self.m = 4;
                }
                Opcode::ADDSPn => {
                    self.reset_flag(Flag::Z);
                    self.reset_flag(Flag::N);
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    if (offset & 0x80) == 0x80 {
                        self.sp = self.sp.wrapping_sub((!offset + 1) as u16);
                    } else {
                        self.sp = self.sp.wrapping_add(offset as u16);
                    }
                    self.m = 4;
                }
                Opcode::JP_HL_ => {
                    self.pc = self.hl();
                    self.m = 1;
                }
                Opcode::LD_nn_A => {
                    let addr = mmu.read_word(self.pc);
                    self.pc = self.pc.wrapping_add(2);
                    mmu.write_byte(addr, self.a);
                    self.m = 4;
                }
                Opcode::XORn => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.a ^= n;
                    self.m = 2;
                }
                Opcode::RST28 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x28;
                    self.m = 4;
                }
                Opcode::LDHA_n_ => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    let addr = 0xff00 + (n as u16);
                    self.a = mmu.read_byte(addr);
                    self.m = 3;
                }
                Opcode::POPAF => {
                    self.f = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.a = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 3;
                }
                Opcode::DI => {
                    self.ime = false;
                    self.m = 1;
                }
                Opcode::PUSHAF => {
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.a);
                    self.sp = self.sp.wrapping_sub(1);
                    mmu.write_byte(self.sp, self.f);
                    self.m = 4;
                }
                Opcode::ORn => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.a |= n;
                    self.m = 2;
                }
                Opcode::RST30 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x30;
                    self.m = 4;
                }
                Opcode::LDHLSPn => {
                    self.reset_flag(Flag::Z);
                    self.reset_flag(Flag::N);
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    let mut spn: u16;
                    if (offset & 0x80) == 0x80 {
                        spn = self.sp.wrapping_sub((!offset + 1) as u16);
                    } else {
                        spn = self.sp.wrapping_add(offset as u16);
                    }
                    self.h = (spn >> 8) as u8;
                    self.l = (spn & 0xf) as u8;
                    self.m = 3;
                }
                Opcode::LDSPHL => {
                    self.sp = self.hl();
                    self.m = 2;
                }
                Opcode::LDA_nn_ => {
                    let addr = mmu.read_word(self.pc);
                    self.pc = self.pc.wrapping_add(2);
                    self.a = mmu.read_byte(addr);
                    self.m = 4;
                }
                Opcode::EI => {
                    self.ime = true;
                    self.m = 1;
                }
                Opcode::CPn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.set_flag(Flag::N);
                    if !check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if !check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::RST38 => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x38;
                    self.m = 4;
                }
            },
            None => return Err("Unsupported operation.".to_owned()),
        }

        self.clock.m += self.m as u32;
        Ok(())
    }

    fn exec_ext(&mut self, mmu: &mut MMU) -> Result<(), String> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
        match ExtOpcode::from_u8(mmu.read_byte(pc)) {
            Some(op) => match op {
                ExtOpcode::RLCB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::RLCC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::RLCD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::RLCE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::RLCH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::RLCL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::RLC_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::RLCA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_left(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::RRCB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::RRCC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::RRCD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::RRCE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::RRCH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::RRCL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::RRC_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::RRCA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value.rotate_right(1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::RLB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::RLC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::RLD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::RLE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::RLH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::RLL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::RL_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::RLA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    value = (value << 1) | self.test_flag(Flag::C);
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::RRB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::RRC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::RRD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::RRE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::RRH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::RRL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::RR_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::RRA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    value = (value >> 1) | (self.test_flag(Flag::C) << 7);
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::SLAB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::SLAC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::SLAD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::SLAE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::SLAH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::SLAL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::SLA_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::SLAA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b7 = (value & 0x80) >> 7;
                    if b7 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value << 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::SRAB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::SRAC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::SRAD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::SRAE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::SRAH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::SRAL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::SRA_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 4;
                }
                ExtOpcode::SRAA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let msb = value & 0x80;
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = msb + (value >> 1);
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::SWAPB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::SWAPC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::SWAPD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::SWAPE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::SWAPH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::SWAPL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::SWAP_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::SWAPA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    self.reset_flag(Flag::C);
                    let upper = value >> 4;
                    let lower = value & 0xf;
                    value = (lower << 4) + upper;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::SRLB => {
                    let mut value = self.b;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.b = value;
                    self.m = 2;
                }
                ExtOpcode::SRLC => {
                    let mut value = self.c;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.c = value;
                    self.m = 2;
                }
                ExtOpcode::SRLD => {
                    let mut value = self.d;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.d = value;
                    self.m = 2;
                }
                ExtOpcode::SRLE => {
                    let mut value = self.e;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.e = value;
                    self.m = 2;
                }
                ExtOpcode::SRLH => {
                    let mut value = self.h;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.h = value;
                    self.m = 2;
                }
                ExtOpcode::SRLL => {
                    let mut value = self.l;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.l = value;
                    self.m = 2;
                }
                ExtOpcode::SRL_HL_ => {
                    let mut value = mmu.read_byte(self.hl());
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    mmu.write_byte(self.hl(), value);
                    self.m = 4;
                }
                ExtOpcode::SRLA => {
                    let mut value = self.a;
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
                    let b0 = value & 0x1;
                    if b0 == 1 {
                        self.set_flag(Flag::C);
                    } else {
                        self.reset_flag(Flag::C);
                    }
                    value = value >> 1;
                    if value == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.a = value;
                    self.m = 2;
                }
                ExtOpcode::BIT0B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT0C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT0D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT0E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT0H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT0L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT0_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT0A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(0, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT1_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT1A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(1, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT2_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT2A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(2, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT3_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT3A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(3, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT4_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT4A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(4, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT5_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT5A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(5, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT6_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT6A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(6, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7B => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.b) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7C => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.c) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7D => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.d) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7E => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.e) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7H => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.h) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7L => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.l) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::BIT7_HL_ => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, mmu.read_byte(self.hl())) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 4;
                }
                ExtOpcode::BIT7A => {
                    self.reset_flag(Flag::N);
                    self.set_flag(Flag::H);
                    if test_bit(7, self.a) == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                ExtOpcode::RES0B => {
                    self.b &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES0C => {
                    self.c &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES0D => {
                    self.d &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES0E => {
                    self.e &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES0H => {
                    self.h &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES0L => {
                    self.l &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xfe);
                    self.m = 4;
                }
                ExtOpcode::RES0A => {
                    self.a &= 0xfe;
                    self.m = 2;
                }
                ExtOpcode::RES1B => {
                    self.b &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES1C => {
                    self.c &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES1D => {
                    self.d &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES1E => {
                    self.e &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES1H => {
                    self.h &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES1L => {
                    self.l &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xfd);
                    self.m = 4;
                }
                ExtOpcode::RES1A => {
                    self.a &= 0xfd;
                    self.m = 2;
                }
                ExtOpcode::RES2B => {
                    self.b &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES2C => {
                    self.c &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES2D => {
                    self.d &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES2E => {
                    self.e &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES2H => {
                    self.h &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES2L => {
                    self.l &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xfb);
                    self.m = 4;
                }
                ExtOpcode::RES2A => {
                    self.a &= 0xfb;
                    self.m = 2;
                }
                ExtOpcode::RES3B => {
                    self.b &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES3C => {
                    self.c &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES3D => {
                    self.d &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES3E => {
                    self.e &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES3H => {
                    self.h &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES3L => {
                    self.l &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xf7);
                    self.m = 4;
                }
                ExtOpcode::RES3A => {
                    self.a &= 0xf7;
                    self.m = 2;
                }
                ExtOpcode::RES4B => {
                    self.b &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES4C => {
                    self.c &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES4D => {
                    self.d &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES4E => {
                    self.e &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES4H => {
                    self.h &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES4L => {
                    self.l &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xef);
                    self.m = 4;
                }
                ExtOpcode::RES4A => {
                    self.a &= 0xef;
                    self.m = 2;
                }
                ExtOpcode::RES5B => {
                    self.b &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES5C => {
                    self.c &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES5D => {
                    self.d &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES5E => {
                    self.e &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES5H => {
                    self.h &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES5L => {
                    self.l &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xdf);
                    self.m = 4;
                }
                ExtOpcode::RES5A => {
                    self.a &= 0xdf;
                    self.m = 2;
                }
                ExtOpcode::RES6B => {
                    self.b &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES6C => {
                    self.c &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES6D => {
                    self.d &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES6E => {
                    self.e &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES6H => {
                    self.h &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES6L => {
                    self.l &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0xbf);
                    self.m = 4;
                }
                ExtOpcode::RES6A => {
                    self.a &= 0xbf;
                    self.m = 2;
                }
                ExtOpcode::RES7B => {
                    self.b &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::RES7C => {
                    self.c &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::RES7D => {
                    self.d &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::RES7E => {
                    self.e &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::RES7H => {
                    self.h &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::RES7L => {
                    self.l &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::RES7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value & 0x7f);
                    self.m = 4;
                }
                ExtOpcode::RES7A => {
                    self.a &= 0x7f;
                    self.m = 2;
                }
                ExtOpcode::SET0B => {
                    self.b |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET0C => {
                    self.c |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET0D => {
                    self.d |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET0E => {
                    self.e |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET0H => {
                    self.h |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET0L => {
                    self.l |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x01);
                    self.m = 4;
                }
                ExtOpcode::SET0A => {
                    self.a |= 0x01;
                    self.m = 2;
                }
                ExtOpcode::SET1B => {
                    self.b |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET1C => {
                    self.c |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET1D => {
                    self.d |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET1E => {
                    self.e |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET1H => {
                    self.h |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET1L => {
                    self.l |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x02);
                    self.m = 4;
                }
                ExtOpcode::SET1A => {
                    self.a |= 0x02;
                    self.m = 2;
                }
                ExtOpcode::SET2B => {
                    self.b |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET2C => {
                    self.c |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET2D => {
                    self.d |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET2E => {
                    self.e |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET2H => {
                    self.h |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET2L => {
                    self.l |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x04);
                    self.m = 4;
                }
                ExtOpcode::SET2A => {
                    self.a |= 0x04;
                    self.m = 2;
                }
                ExtOpcode::SET3B => {
                    self.b |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET3C => {
                    self.c |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET3D => {
                    self.d |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET3E => {
                    self.e |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET3H => {
                    self.h |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET3L => {
                    self.l |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x08);
                    self.m = 4;
                }
                ExtOpcode::SET3A => {
                    self.a |= 0x08;
                    self.m = 2;
                }
                ExtOpcode::SET4B => {
                    self.b |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET4C => {
                    self.c |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET4D => {
                    self.d |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET4E => {
                    self.e |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET4H => {
                    self.h |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET4L => {
                    self.l |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x10);
                    self.m = 4;
                }
                ExtOpcode::SET4A => {
                    self.a |= 0x10;
                    self.m = 2;
                }
                ExtOpcode::SET5B => {
                    self.b |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET5C => {
                    self.c |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET5D => {
                    self.d |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET5E => {
                    self.e |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET5H => {
                    self.h |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET5L => {
                    self.l |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x20);
                    self.m = 4;
                }
                ExtOpcode::SET5A => {
                    self.a |= 0x20;
                    self.m = 2;
                }
                ExtOpcode::SET6B => {
                    self.b |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET6C => {
                    self.c |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET6D => {
                    self.d |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET6E => {
                    self.e |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET6H => {
                    self.h |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET6L => {
                    self.l |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x40);
                    self.m = 4;
                }
                ExtOpcode::SET6A => {
                    self.a |= 0x40;
                    self.m = 2;
                }
                ExtOpcode::SET7B => {
                    self.b |= 0x80;
                    self.m = 2;
                }
                ExtOpcode::SET7C => {
                    self.c |= 0x80;
                    self.m = 2;
                }
                ExtOpcode::SET7D => {
                    self.d |= 0x80;
                    self.m = 2;
                }
                ExtOpcode::SET7E => {
                    self.e |= 0x80;
                    self.m = 2;
                }
                ExtOpcode::SET7H => {
                    self.h |= 0x80;
                    self.m = 2;
                }
                ExtOpcode::SET7L => {
                    self.l |= 0x80;
                    self.m = 2;
                }
                ExtOpcode::SET7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), value | 0x80);
                    self.m = 4;
                }
                ExtOpcode::SET7A => {
                    self.a |= 0x80;
                    self.m = 2;
                }
            },
            None => return Err("Unsupported operation.".to_owned()),
        }

        self.clock.m += self.m as u32;
        Ok(())
    }
}

fn test_bit(b: u8, n: u8) -> u8 {
    (n & (1 << b)) >> b
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

fn check_borrow_8(a: u8, b: u8) -> bool {
    a < b
}

#[cfg(test)]
mod tests {
    use super::*;
    use cartridge::Cartridge;
    use gpu::GPU;
    use std::rc::Rc;
    use timer::Timer;

    fn mmu_stub(b1: u8, b2: u8, b3: u8) -> MMU {
        let mut cart_data = [0; 0x148];
        cart_data[0x0] = b1;
        cart_data[0x1] = b2;
        cart_data[0x2] = b3;
        let cart = Cartridge::new(&cart_data);
        let timer = Timer::new();
        let gpu = GPU::new();
        MMU::new(cart, Rc::new(timer), Rc::new(gpu))
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

    #[test]
    fn test_bit_ops() {
        let mut cpu = CPU::new();

        cpu.c = 0b1101_0100;
        cpu.exec(&mut mmu_stub(
            Opcode::ExtOps as u8,
            ExtOpcode::BIT3C as u8,
            0,
        ))
        .expect("CPU exec failed");
        assert_eq!(cpu.test_flag(Flag::Z), 1);

        cpu.pc = 0;
        cpu.reset_flag(Flag::Z);
        cpu.d = 0b0010_0100;
        cpu.exec(&mut mmu_stub(
            Opcode::ExtOps as u8,
            ExtOpcode::BIT2D as u8,
            0,
        ))
        .expect("CPU exec failed");
        assert_eq!(cpu.test_flag(Flag::Z), 0);

        cpu.pc = 0;
        cpu.a = 0b0010_0100;
        cpu.exec(&mut mmu_stub(
            Opcode::ExtOps as u8,
            ExtOpcode::SET7A as u8,
            0,
        ))
        .expect("CPU exec failed");
        assert_eq!(cpu.a, 0b1010_0100);

        cpu.pc = 0;
        cpu.l = 0b0011_0110;
        cpu.exec(&mut mmu_stub(
            Opcode::ExtOps as u8,
            ExtOpcode::RES4L as u8,
            0,
        ))
        .expect("CPU exec failed");
        assert_eq!(cpu.l, 0b0010_0110);
    }
}
