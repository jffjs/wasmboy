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
    halt: bool,
    interrupts_enabled: bool,
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
            halt: false,
            interrupts_enabled: false,
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
                    if self.test_flag(Flag::Z) == 0 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                    }
                    self.m = 2; //jsGB has 3 (1 + 2 if true)?
                }
                Opcode::POPBC => {
                    self.c = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.b = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 3;
                }
                Opcode::JPNZnn => {
                    if self.test_flag(Flag::Z) == 0 {
                        self.pc = mmu.read_word(self.pc);
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                    self.m = 3;
                }
                Opcode::JPnn => {
                    self.pc = mmu.read_word(self.pc);
                    self.m = 3;
                }
                Opcode::CALLNZnn => {
                    if self.test_flag(Flag::Z) == 0 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                    self.m = 3;
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
                    self.m = 8;
                }
                Opcode::RETZ => {
                    if self.test_flag(Flag::Z) == 1 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                    }
                    self.m = 2;
                }
                Opcode::RET => {
                    self.pc = mmu.read_word(self.sp);
                    self.sp = self.sp.wrapping_add(2);
                    self.m = 2;
                }
                Opcode::JPZnn => {
                    if self.test_flag(Flag::Z) == 1 {
                        self.pc = mmu.read_word(self.pc);
                    }
                    self.m = 3;
                }
                Opcode::ExtOps => return self.exec_ext(mmu),
                Opcode::CALLZnn => {
                    if self.test_flag(Flag::Z) == 1 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                    self.m = 3;
                }
                Opcode::CALLnn => {
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc.wrapping_add(2));
                    self.pc = mmu.read_word(self.pc);
                    self.m = 3;
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
                    self.m = 8;
                }
                Opcode::RETNC => {
                    if self.test_flag(Flag::C) == 0 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                    }
                    self.m = 2;
                }
                Opcode::POPDE => {
                    self.e = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.d = mmu.read_byte(self.sp);
                    self.sp = self.sp.wrapping_add(1);
                    self.m = 3;
                }
                Opcode::JPNCnn => {
                    if self.test_flag(Flag::C) == 0 {
                        self.pc = mmu.read_word(self.pc);
                    }
                    self.m = 3;
                }
                Opcode::CALLNCnn => {
                    if self.test_flag(Flag::C) == 0 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                    self.m = 3;
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
                    self.m = 8;
                }
                Opcode::RETC => {
                    if self.test_flag(Flag::C) == 1 {
                        self.pc = mmu.read_word(self.sp);
                        self.sp = self.sp.wrapping_add(2);
                    }
                    self.m = 2;
                }
                Opcode::RETI => {
                    self.pc = mmu.read_word(self.sp);
                    self.sp = self.sp.wrapping_add(2);
                    self.interrupts_enabled = true;
                }
                Opcode::JPCnn => {
                    if self.test_flag(Flag::C) == 1 {
                        self.pc = mmu.read_word(self.pc);
                    }
                    self.m = 3;
                }
                Opcode::CALLCnn => {
                    if self.test_flag(Flag::C) == 1 {
                        self.sp = self.sp.wrapping_sub(2);
                        mmu.write_word(self.sp, self.pc.wrapping_add(2));
                        self.pc = mmu.read_word(self.pc);
                    } else {
                        self.pc = self.pc.wrapping_add(2);
                    }
                    self.m = 3;
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
                    self.m = 8;
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
                    self.m = 8;
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
                    self.m = 8;
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
                    self.interrupts_enabled = false;
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
                    self.m = 8;
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
                    self.interrupts_enabled = true;
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
                    self.m = 8;
                }
                _ => return Err("Unsupported operation."),
            },
            None => return Err("Unsupported operation."),
        }

        self.clock.m += self.m as u32;
        Ok(())
    }

    fn exec_ext(&mut self, mmu: &mut MMU) -> Result<(), &str> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
        println!("opcode: {:?}", Opcode::from_u8(mmu.read_byte(pc)).unwrap());
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
