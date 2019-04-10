use emulator::IntFlag;
use mmu::MMU;
use num::FromPrimitive;
use opcode::*;
use utils::*;
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

enum Condition {
    Z,
    NZ,
    C,
    NC,
}

#[derive(Clone, Copy)]
enum Reg {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
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

    pub fn handle_interrupt(&mut self, iflag: IntFlag, mmu: &mut MMU) {
        mmu.rsv(self.clone());
        self.sp = self.sp.wrapping_sub(2);
        mmu.write_word(self.sp, self.pc);
        match iflag {
            IntFlag::Vblank => self.pc = 0x40,
            IntFlag::LCDC => self.pc = 0x48,
            IntFlag::TimerOverflow => self.pc = 0x50,
            IntFlag::SerialIO => self.pc = 0x58,
            IntFlag::JoyPad => self.pc = 0x60,
        }
        self.clock.m += 4;
    }

    pub fn exec(&mut self, mmu: &mut MMU) -> Result<(), String> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
        let opcode = Opcode::from_u8(mmu.read_byte(pc));
        match opcode {
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
                    self.inc(Reg::B);
                }
                Opcode::DECB => {
                    self.dec(Reg::B);
                }
                Opcode::LDBn => {
                    self.b = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RLCA => {
                    self.rlc(Reg::A);
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
                    self.inc(Reg::C);
                }
                Opcode::DECC => {
                    self.dec(Reg::C);
                }
                Opcode::LDCn => {
                    self.c = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RRCA => {
                    self.rrc(Reg::A);
                    self.m = 1;
                }
                Opcode::STOP => {
                    if mmu.read_byte(self.pc) == 0 {
                        self.pc = self.pc.wrapping_add(1);
                        self.stop = true;
                        self.m = 1;
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
                    if self.e == 0 {
                        self.d = self.d.wrapping_add(1);
                    }
                    self.m = 2; //jsGB has 1?
                }
                Opcode::INCD => {
                    self.inc(Reg::D);
                }
                Opcode::DECD => {
                    self.dec(Reg::D);
                }
                Opcode::LDDn => {
                    self.d = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RLA => {
                    self.rl(Reg::A);
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
                    self.e = self.e.wrapping_sub(1);
                    if self.e == 0xff {
                        self.d = self.d.wrapping_sub(1);
                    }
                    self.m = 2; //jsGB has 1?
                }
                Opcode::INCE => {
                    self.inc(Reg::E);
                }
                Opcode::DECE => {
                    self.dec(Reg::E);
                }
                Opcode::LDEn => {
                    self.e = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::RRA => {
                    self.rr(Reg::A);
                    self.m = 1;
                }
                Opcode::JRNZn => {
                    self.m = 2;
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    if self.test_flag(Flag::Z) == 0 {
                        if (offset & 0x80) == 0x80 {
                            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
                        } else {
                            self.pc = self.pc.wrapping_add(offset as u16);
                        }
                        self.m += 1;
                    }
                }
                Opcode::LDHLnn => {
                    self.l = mmu.read_byte(self.pc);
                    self.h = mmu.read_byte(self.pc.wrapping_add(1));
                    self.pc = self.pc.wrapping_add(2);
                    self.m = 3;
                }
                Opcode::LDI_HL_A => {
                    mmu.write_byte(self.hl(), self.a);
                    self.l = self.l.wrapping_add(1);
                    if self.l == 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::INCHL => {
                    self.l = self.l.wrapping_add(1);
                    if self.l == 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::INCH => {
                    self.inc(Reg::H);
                }
                Opcode::DECH => {
                    self.dec(Reg::H);
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
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    if self.test_flag(Flag::Z) == 1 {
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
                    if self.l == 0 {
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
                    self.inc(Reg::L);
                }
                Opcode::DECL => {
                    self.dec(Reg::L);
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
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    if self.test_flag(Flag::C) == 0 {
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
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.inc_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 3;
                }
                Opcode::DEC_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.dec_val(value);
                    mmu.write_byte(addr, value);
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
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    if self.test_flag(Flag::C) == 1 {
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
                    self.inc(Reg::A);
                }
                Opcode::DECA => {
                    self.dec(Reg::A);
                }
                Opcode::LDAn => {
                    self.a = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.m = 2;
                }
                Opcode::CCF => {
                    self.reset_flag(Flag::N);
                    self.reset_flag(Flag::H);
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
                    let v = self.b;
                    self.add(v);
                }
                Opcode::ADDAC => {
                    let v = self.c;
                    self.add(v);
                }
                Opcode::ADDAD => {
                    let v = self.d;
                    self.add(v);
                }
                Opcode::ADDAE => {
                    let v = self.e;
                    self.add(v);
                }
                Opcode::ADDAH => {
                    let v = self.h;
                    self.add(v);
                }
                Opcode::ADDAL => {
                    let v = self.l;
                    self.add(v);
                }
                Opcode::ADDA_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.add(v);
                    self.m += 1;
                }
                Opcode::ADDAA => {
                    let v = self.a;
                    self.add(v);
                }
                Opcode::ADCAB => {
                    let v = self.b;
                    self.adc(v);
                }
                Opcode::ADCAC => {
                    let v = self.c;
                    self.adc(v);
                }
                Opcode::ADCAD => {
                    let v = self.d;
                    self.adc(v);
                }
                Opcode::ADCAE => {
                    let v = self.e;
                    self.adc(v);
                }
                Opcode::ADCAH => {
                    let v = self.h;
                    self.adc(v);
                }
                Opcode::ADCAL => {
                    let v = self.l;
                    self.adc(v);
                }
                Opcode::ADCA_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.adc(v);
                    self.m += 1;
                }
                Opcode::ADCAA => {
                    let v = self.a;
                    self.adc(v);
                }
                Opcode::SUBAB => {
                    let v = self.b;
                    self.sub(v);
                }
                Opcode::SUBAC => {
                    let v = self.c;
                    self.sub(v);
                }
                Opcode::SUBAD => {
                    let v = self.d;
                    self.sub(v);
                }
                Opcode::SUBAE => {
                    let v = self.e;
                    self.sub(v);
                }
                Opcode::SUBAH => {
                    let v = self.h;
                    self.sub(v);
                }
                Opcode::SUBAL => {
                    let v = self.l;
                    self.sub(v);
                }
                Opcode::SUBA_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.sub(v);
                    self.m += 1;
                }
                Opcode::SUBAA => {
                    let v = self.a;
                    self.sub(v);
                }
                Opcode::SBCAB => {
                    let v = self.b;
                    self.sbc(v);
                }
                Opcode::SBCAC => {
                    let v = self.c;
                    self.sbc(v);
                }
                Opcode::SBCAD => {
                    let v = self.d;
                    self.sbc(v);
                }
                Opcode::SBCAE => {
                    let v = self.e;
                    self.sbc(v);
                }
                Opcode::SBCAH => {
                    let v = self.h;
                    self.sbc(v);
                }
                Opcode::SBCAL => {
                    let v = self.l;
                    self.sbc(v);
                }
                Opcode::SBCA_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.sbc(v);
                    self.m += 1;
                }
                Opcode::SBCAA => {
                    let v = self.a;
                    self.sbc(v);
                }
                Opcode::ANDB => {
                    let v = self.b;
                    self.and(v);
                }
                Opcode::ANDC => {
                    let v = self.c;
                    self.and(v);
                }
                Opcode::ANDD => {
                    let v = self.d;
                    self.and(v);
                }
                Opcode::ANDE => {
                    let v = self.e;
                    self.and(v);
                }
                Opcode::ANDH => {
                    let v = self.h;
                    self.and(v);
                }
                Opcode::ANDL => {
                    let v = self.l;
                    self.and(v);
                }
                Opcode::AND_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.and(v);
                    self.m += 1;
                }
                Opcode::ANDA => {
                    let v = self.a;
                    self.and(v);
                }
                Opcode::XORB => {
                    let v = self.b;
                    self.xor(v);
                }
                Opcode::XORC => {
                    let v = self.c;
                    self.xor(v);
                }
                Opcode::XORD => {
                    let v = self.d;
                    self.xor(v);
                }
                Opcode::XORE => {
                    let v = self.e;
                    self.xor(v);
                }
                Opcode::XORH => {
                    let v = self.h;
                    self.xor(v);
                }
                Opcode::XORL => {
                    let v = self.l;
                    self.xor(v);
                }
                Opcode::XOR_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.xor(v);
                    self.m += 1;
                }
                Opcode::XORA => {
                    let v = self.a;
                    self.xor(v);
                }
                Opcode::ORB => {
                    let v = self.b;
                    self.or(v);
                }
                Opcode::ORC => {
                    let v = self.c;
                    self.or(v);
                }
                Opcode::ORD => {
                    let v = self.d;
                    self.or(v);
                }
                Opcode::ORE => {
                    let v = self.e;
                    self.or(v);
                }
                Opcode::ORH => {
                    let v = self.h;
                    self.or(v);
                }
                Opcode::ORL => {
                    let v = self.l;
                    self.or(v);
                }
                Opcode::OR_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.or(v);
                    self.m += 1;
                }
                Opcode::ORA => {
                    let v = self.a;
                    self.or(v);
                }
                Opcode::CPB => {
                    let v = self.b;
                    self.cmp(v);
                }
                Opcode::CPC => {
                    let v = self.c;
                    self.cmp(v);
                }
                Opcode::CPD => {
                    let v = self.d;
                    self.cmp(v);
                }
                Opcode::CPE => {
                    let v = self.e;
                    self.cmp(v);
                }
                Opcode::CPH => {
                    let v = self.h;
                    self.cmp(v);
                }
                Opcode::CP_L => {
                    let v = self.l;
                    self.cmp(v);
                }
                Opcode::CP_HL_ => {
                    let v = mmu.read_byte(self.hl());
                    self.cmp(v);
                    self.m += 1;
                }
                Opcode::CPA => {
                    let v = self.a;
                    self.cmp(v);
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
                    self.call(Some(Condition::NZ), mmu);
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
                    self.add(value);
                    self.m += 1;
                }
                Opcode::RST0 => {
                    mmu.rsv(self.clone());
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
                    self.call(Some(Condition::Z), mmu);
                }
                Opcode::CALLnn => {
                    self.call(None, mmu);
                }
                Opcode::ADCAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.adc(value);
                    self.m += 1;
                }
                Opcode::RST8 => {
                    mmu.rsv(self.clone());
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
                    self.call(Some(Condition::NC), mmu);
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
                    self.sub(value);
                    self.m += 1;
                }
                Opcode::RST10 => {
                    mmu.rsv(self.clone());
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
                    // self.restore(mmu.rrs());
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
                    self.call(Some(Condition::C), mmu);
                }
                Opcode::SBCAn => {
                    let mut value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.sbc(value);
                    self.m += 1;
                }
                Opcode::RST18 => {
                    mmu.rsv(self.clone());
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x18;
                    self.m = 4;
                }
                Opcode::LDH_n_A => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
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
                    self.and(n);
                    self.m += 1;
                }
                Opcode::RST20 => {
                    mmu.rsv(self.clone());
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
                    self.xor(n);
                    self.m += 1;
                }
                Opcode::RST28 => {
                    mmu.rsv(self.clone());
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
                    self.or(n);
                    self.m += 1;
                }
                Opcode::RST30 => {
                    mmu.rsv(self.clone());
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
                    self.l = (spn & 0xff) as u8;
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
                    if check_half_borrow_8(self.a, value) {
                        self.set_flag(Flag::H);
                    }
                    if check_borrow_8(self.a, value) {
                        self.set_flag(Flag::C);
                    }
                    let result = self.a.wrapping_sub(value);
                    if result == 0 {
                        self.set_flag(Flag::Z);
                    }
                    self.m = 2;
                }
                Opcode::RST38 => {
                    mmu.rsv(self.clone());
                    self.sp = self.sp.wrapping_sub(2);
                    mmu.write_word(self.sp, self.pc);
                    self.pc = 0x38;
                    self.m = 4;
                }
            },
            None => {
                // println!("Unsupported operation: {}", mmu.read_byte(pc));
            }
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
                    self.rlc(Reg::B);
                }
                ExtOpcode::RLCC => {
                    self.rlc(Reg::C);
                }
                ExtOpcode::RLCD => {
                    self.rlc(Reg::D);
                }
                ExtOpcode::RLCE => {
                    self.rlc(Reg::E);
                }
                ExtOpcode::RLCH => {
                    self.rlc(Reg::H);
                }
                ExtOpcode::RLCL => {
                    self.rlc(Reg::H);
                }
                ExtOpcode::RLC_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rlc_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RLCA => {
                    self.rlc(Reg::A);
                }
                ExtOpcode::RRCB => {
                    self.rrc(Reg::B);
                }
                ExtOpcode::RRCC => {
                    self.rrc(Reg::C);
                }
                ExtOpcode::RRCD => {
                    self.rrc(Reg::D);
                }
                ExtOpcode::RRCE => {
                    self.rrc(Reg::E);
                }
                ExtOpcode::RRCH => {
                    self.rrc(Reg::H);
                }
                ExtOpcode::RRCL => {
                    self.rrc(Reg::L);
                }
                ExtOpcode::RRC_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rrc_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RRCA => {
                    self.rrc(Reg::A);
                }
                ExtOpcode::RLB => {
                    self.rl(Reg::B);
                }
                ExtOpcode::RLC => {
                    self.rl(Reg::C);
                }
                ExtOpcode::RLD => {
                    self.rl(Reg::D);
                }
                ExtOpcode::RLE => {
                    self.rl(Reg::E);
                }
                ExtOpcode::RLH => {
                    self.rl(Reg::H);
                }
                ExtOpcode::RLL => {
                    self.rl(Reg::L);
                }
                ExtOpcode::RL_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rl_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RLA => {
                    self.rl(Reg::A);
                }
                ExtOpcode::RRB => {
                    self.rr(Reg::B);
                }
                ExtOpcode::RRC => {
                    self.rr(Reg::C);
                }
                ExtOpcode::RRD => {
                    self.rr(Reg::D);
                }
                ExtOpcode::RRE => {
                    self.rr(Reg::E);
                }
                ExtOpcode::RRH => {
                    self.rr(Reg::H);
                }
                ExtOpcode::RRL => {
                    self.rr(Reg::L);
                }
                ExtOpcode::RR_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rr_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RRA => {
                    self.rr(Reg::A);
                }
                ExtOpcode::SLAB => {
                    self.sla(Reg::B);
                }
                ExtOpcode::SLAC => {
                    self.sla(Reg::C);
                }
                ExtOpcode::SLAD => {
                    self.sla(Reg::D);
                }
                ExtOpcode::SLAE => {
                    self.sla(Reg::E);
                }
                ExtOpcode::SLAH => {
                    self.sla(Reg::H);
                }
                ExtOpcode::SLAL => {
                    self.sla(Reg::L);
                }
                ExtOpcode::SLA_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.sla_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SLAA => {
                    self.sla(Reg::A);
                }
                ExtOpcode::SRAB => {
                    self.sra(Reg::B);
                }
                ExtOpcode::SRAC => {
                    self.sra(Reg::C);
                }
                ExtOpcode::SRAD => {
                    self.sra(Reg::D);
                }
                ExtOpcode::SRAE => {
                    self.sra(Reg::E);
                }
                ExtOpcode::SRAH => {
                    self.sra(Reg::H);
                }
                ExtOpcode::SRAL => {
                    self.sra(Reg::L);
                }
                ExtOpcode::SRA_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.sra_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SRAA => {
                    self.sra(Reg::A);
                }
                ExtOpcode::SWAPB => {
                    self.swap(Reg::B);
                }
                ExtOpcode::SWAPC => {
                    self.swap(Reg::C);
                }
                ExtOpcode::SWAPD => {
                    self.swap(Reg::D);
                }
                ExtOpcode::SWAPE => {
                    self.swap(Reg::E);
                }
                ExtOpcode::SWAPH => {
                    self.swap(Reg::H);
                }
                ExtOpcode::SWAPL => {
                    self.swap(Reg::L);
                }
                ExtOpcode::SWAP_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.swap_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SWAPA => {
                    self.swap(Reg::A);
                }
                ExtOpcode::SRLB => {
                    self.srl(Reg::B);
                }
                ExtOpcode::SRLC => {
                    self.srl(Reg::C);
                }
                ExtOpcode::SRLD => {
                    self.srl(Reg::D);
                }
                ExtOpcode::SRLE => {
                    self.srl(Reg::E);
                }
                ExtOpcode::SRLH => {
                    self.srl(Reg::H);
                }
                ExtOpcode::SRLL => {
                    self.srl(Reg::L);
                }
                ExtOpcode::SRL_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.srl_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SRLA => {
                    self.srl(Reg::A);
                }
                ExtOpcode::BIT0B => {
                    self.bit(Reg::B, 0);
                }
                ExtOpcode::BIT0C => {
                    self.bit(Reg::C, 0);
                }
                ExtOpcode::BIT0D => {
                    self.bit(Reg::D, 0);
                }
                ExtOpcode::BIT0E => {
                    self.bit(Reg::E, 0);
                }
                ExtOpcode::BIT0H => {
                    self.bit(Reg::H, 0);
                }
                ExtOpcode::BIT0L => {
                    self.bit(Reg::L, 0);
                }
                ExtOpcode::BIT0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 0);
                    self.m = 3;
                }
                ExtOpcode::BIT0A => {
                    self.bit(Reg::A, 0);
                }
                ExtOpcode::BIT1B => {
                    self.bit(Reg::B, 1);
                }
                ExtOpcode::BIT1C => {
                    self.bit(Reg::C, 1);
                }
                ExtOpcode::BIT1D => {
                    self.bit(Reg::D, 1);
                }
                ExtOpcode::BIT1E => {
                    self.bit(Reg::E, 1);
                }
                ExtOpcode::BIT1H => {
                    self.bit(Reg::H, 1);
                }
                ExtOpcode::BIT1L => {
                    self.bit(Reg::L, 1);
                }
                ExtOpcode::BIT1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 1);
                    self.m = 3;
                }
                ExtOpcode::BIT1A => {
                    self.bit(Reg::A, 1);
                }
                ExtOpcode::BIT2B => {
                    self.bit(Reg::B, 2);
                }
                ExtOpcode::BIT2C => {
                    self.bit(Reg::C, 2);
                }
                ExtOpcode::BIT2D => {
                    self.bit(Reg::D, 2);
                }
                ExtOpcode::BIT2E => {
                    self.bit(Reg::E, 2);
                }
                ExtOpcode::BIT2H => {
                    self.bit(Reg::H, 2);
                }
                ExtOpcode::BIT2L => {
                    self.bit(Reg::L, 2);
                }
                ExtOpcode::BIT2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 2);
                    self.m = 3;
                }
                ExtOpcode::BIT2A => {
                    self.bit(Reg::A, 2);
                }
                ExtOpcode::BIT3B => {
                    self.bit(Reg::B, 3);
                }
                ExtOpcode::BIT3C => {
                    self.bit(Reg::C, 3);
                }
                ExtOpcode::BIT3D => {
                    self.bit(Reg::D, 3);
                }
                ExtOpcode::BIT3E => {
                    self.bit(Reg::E, 3);
                }
                ExtOpcode::BIT3H => {
                    self.bit(Reg::H, 3);
                }
                ExtOpcode::BIT3L => {
                    self.bit(Reg::L, 3);
                }
                ExtOpcode::BIT3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 3);
                    self.m = 3;
                }
                ExtOpcode::BIT3A => {
                    self.bit(Reg::A, 3);
                }
                ExtOpcode::BIT4B => {
                    self.bit(Reg::B, 4);
                }
                ExtOpcode::BIT4C => {
                    self.bit(Reg::C, 4);
                }
                ExtOpcode::BIT4D => {
                    self.bit(Reg::D, 4);
                }
                ExtOpcode::BIT4E => {
                    self.bit(Reg::E, 4);
                }
                ExtOpcode::BIT4H => {
                    self.bit(Reg::H, 4);
                }
                ExtOpcode::BIT4L => {
                    self.bit(Reg::L, 4);
                }
                ExtOpcode::BIT4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 4);
                    self.m = 3;
                }
                ExtOpcode::BIT4A => {
                    self.bit(Reg::A, 4);
                }
                ExtOpcode::BIT5B => {
                    self.bit(Reg::B, 5);
                }
                ExtOpcode::BIT5C => {
                    self.bit(Reg::C, 5);
                }
                ExtOpcode::BIT5D => {
                    self.bit(Reg::D, 5);
                }
                ExtOpcode::BIT5E => {
                    self.bit(Reg::E, 5);
                }
                ExtOpcode::BIT5H => {
                    self.bit(Reg::H, 5);
                }
                ExtOpcode::BIT5L => {
                    self.bit(Reg::L, 5);
                }
                ExtOpcode::BIT5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 5);
                    self.m = 3;
                }
                ExtOpcode::BIT5A => {
                    self.bit(Reg::A, 5);
                }
                ExtOpcode::BIT6B => {
                    self.bit(Reg::B, 6);
                }
                ExtOpcode::BIT6C => {
                    self.bit(Reg::C, 6);
                }
                ExtOpcode::BIT6D => {
                    self.bit(Reg::D, 6);
                }
                ExtOpcode::BIT6E => {
                    self.bit(Reg::E, 6);
                }
                ExtOpcode::BIT6H => {
                    self.bit(Reg::H, 6);
                }
                ExtOpcode::BIT6L => {
                    self.bit(Reg::L, 6);
                }
                ExtOpcode::BIT6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 6);
                    self.m = 3;
                }
                ExtOpcode::BIT6A => {
                    self.bit(Reg::A, 6);
                }
                ExtOpcode::BIT7B => {
                    self.bit(Reg::B, 7);
                }
                ExtOpcode::BIT7C => {
                    self.bit(Reg::C, 7);
                }
                ExtOpcode::BIT7D => {
                    self.bit(Reg::D, 7);
                }
                ExtOpcode::BIT7E => {
                    self.bit(Reg::E, 7);
                }
                ExtOpcode::BIT7H => {
                    self.bit(Reg::H, 7);
                }
                ExtOpcode::BIT7L => {
                    self.bit(Reg::L, 7);
                }
                ExtOpcode::BIT7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 7);
                    self.m = 3;
                }
                ExtOpcode::BIT7A => {
                    self.bit(Reg::A, 7);
                }
                ExtOpcode::RES0B => {
                    self.res(Reg::B, 0);
                }
                ExtOpcode::RES0C => {
                    self.res(Reg::C, 0);
                }
                ExtOpcode::RES0D => {
                    self.res(Reg::D, 0);
                }
                ExtOpcode::RES0E => {
                    self.res(Reg::E, 0);
                }
                ExtOpcode::RES0H => {
                    self.res(Reg::H, 0);
                }
                ExtOpcode::RES0L => {
                    self.res(Reg::L, 0);
                }
                ExtOpcode::RES0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(0, value));
                    self.m = 4;
                }
                ExtOpcode::RES0A => {
                    self.res(Reg::A, 0);
                }
                ExtOpcode::RES1B => {
                    self.res(Reg::B, 1);
                }
                ExtOpcode::RES1C => {
                    self.res(Reg::C, 1);
                }
                ExtOpcode::RES1D => {
                    self.res(Reg::D, 1);
                }
                ExtOpcode::RES1E => {
                    self.res(Reg::E, 1);
                }
                ExtOpcode::RES1H => {
                    self.res(Reg::H, 1);
                }
                ExtOpcode::RES1L => {
                    self.res(Reg::L, 1);
                }
                ExtOpcode::RES1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(1, value));
                    self.m = 4;
                }
                ExtOpcode::RES1A => {
                    self.res(Reg::A, 1);
                }
                ExtOpcode::RES2B => {
                    self.res(Reg::B, 2);
                }
                ExtOpcode::RES2C => {
                    self.res(Reg::C, 2);
                }
                ExtOpcode::RES2D => {
                    self.res(Reg::D, 2);
                }
                ExtOpcode::RES2E => {
                    self.res(Reg::E, 2);
                }
                ExtOpcode::RES2H => {
                    self.res(Reg::H, 2);
                }
                ExtOpcode::RES2L => {
                    self.res(Reg::L, 2);
                }
                ExtOpcode::RES2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(2, value));
                    self.m = 4;
                }
                ExtOpcode::RES2A => {
                    self.res(Reg::A, 2);
                }
                ExtOpcode::RES3B => {
                    self.res(Reg::B, 3);
                }
                ExtOpcode::RES3C => {
                    self.res(Reg::C, 3);
                }
                ExtOpcode::RES3D => {
                    self.res(Reg::D, 3);
                }
                ExtOpcode::RES3E => {
                    self.res(Reg::E, 3);
                }
                ExtOpcode::RES3H => {
                    self.res(Reg::H, 3);
                }
                ExtOpcode::RES3L => {
                    self.res(Reg::L, 3);
                }
                ExtOpcode::RES3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(3, value));
                    self.m = 4;
                }
                ExtOpcode::RES3A => {
                    self.res(Reg::A, 3);
                }
                ExtOpcode::RES4B => {
                    self.res(Reg::B, 4);
                }
                ExtOpcode::RES4C => {
                    self.res(Reg::C, 4);
                }
                ExtOpcode::RES4D => {
                    self.res(Reg::D, 4);
                }
                ExtOpcode::RES4E => {
                    self.res(Reg::E, 4);
                }
                ExtOpcode::RES4H => {
                    self.res(Reg::H, 4);
                }
                ExtOpcode::RES4L => {
                    self.res(Reg::L, 4);
                }
                ExtOpcode::RES4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(4, value));
                    self.m = 4;
                }
                ExtOpcode::RES4A => {
                    self.res(Reg::A, 4);
                }
                ExtOpcode::RES5B => {
                    self.res(Reg::B, 5);
                }
                ExtOpcode::RES5C => {
                    self.res(Reg::C, 5);
                }
                ExtOpcode::RES5D => {
                    self.res(Reg::D, 5);
                }
                ExtOpcode::RES5E => {
                    self.res(Reg::E, 5);
                }
                ExtOpcode::RES5H => {
                    self.res(Reg::H, 5);
                }
                ExtOpcode::RES5L => {
                    self.res(Reg::L, 5);
                }
                ExtOpcode::RES5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(5, value));
                    self.m = 4;
                }
                ExtOpcode::RES5A => {
                    self.res(Reg::A, 5);
                }
                ExtOpcode::RES6B => {
                    self.res(Reg::B, 6);
                }
                ExtOpcode::RES6C => {
                    self.res(Reg::C, 6);
                }
                ExtOpcode::RES6D => {
                    self.res(Reg::D, 6);
                }
                ExtOpcode::RES6E => {
                    self.res(Reg::E, 6);
                }
                ExtOpcode::RES6H => {
                    self.res(Reg::H, 6);
                }
                ExtOpcode::RES6L => {
                    self.res(Reg::L, 6);
                }
                ExtOpcode::RES6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(6, value));
                    self.m = 4;
                }
                ExtOpcode::RES6A => {
                    self.res(Reg::A, 6);
                }
                ExtOpcode::RES7B => {
                    self.res(Reg::B, 7);
                }
                ExtOpcode::RES7C => {
                    self.res(Reg::C, 7);
                }
                ExtOpcode::RES7D => {
                    self.res(Reg::D, 7);
                }
                ExtOpcode::RES7E => {
                    self.res(Reg::E, 7);
                }
                ExtOpcode::RES7H => {
                    self.res(Reg::H, 7);
                }
                ExtOpcode::RES7L => {
                    self.res(Reg::L, 7);
                }
                ExtOpcode::RES7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(7, value));
                    self.m = 4;
                }
                ExtOpcode::RES7A => {
                    self.res(Reg::A, 7);
                }
                ExtOpcode::SET0B => {
                    self.set(Reg::B, 0);
                }
                ExtOpcode::SET0C => {
                    self.set(Reg::C, 0);
                }
                ExtOpcode::SET0D => {
                    self.set(Reg::D, 0);
                }
                ExtOpcode::SET0E => {
                    self.set(Reg::E, 0);
                }
                ExtOpcode::SET0H => {
                    self.set(Reg::H, 0);
                }
                ExtOpcode::SET0L => {
                    self.set(Reg::L, 0);
                }
                ExtOpcode::SET0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(0, value));
                    self.m = 4;
                }
                ExtOpcode::SET0A => {
                    self.set(Reg::A, 0);
                }
                ExtOpcode::SET1B => {
                    self.set(Reg::B, 1);
                }
                ExtOpcode::SET1C => {
                    self.set(Reg::C, 1);
                }
                ExtOpcode::SET1D => {
                    self.set(Reg::D, 1);
                }
                ExtOpcode::SET1E => {
                    self.set(Reg::E, 1);
                }
                ExtOpcode::SET1H => {
                    self.set(Reg::H, 1);
                }
                ExtOpcode::SET1L => {
                    self.set(Reg::L, 1);
                }
                ExtOpcode::SET1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(1, value));
                    self.m = 4;
                }
                ExtOpcode::SET1A => {
                    self.set(Reg::A, 1);
                }
                ExtOpcode::SET2B => {
                    self.set(Reg::B, 2);
                }
                ExtOpcode::SET2C => {
                    self.set(Reg::C, 2);
                }
                ExtOpcode::SET2D => {
                    self.set(Reg::D, 2);
                }
                ExtOpcode::SET2E => {
                    self.set(Reg::E, 2);
                }
                ExtOpcode::SET2H => {
                    self.set(Reg::H, 2);
                }
                ExtOpcode::SET2L => {
                    self.set(Reg::L, 2);
                }
                ExtOpcode::SET2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(2, value));
                    self.m = 4;
                }
                ExtOpcode::SET2A => {
                    self.set(Reg::A, 2);
                }
                ExtOpcode::SET3B => {
                    self.set(Reg::B, 3);
                }
                ExtOpcode::SET3C => {
                    self.set(Reg::C, 3);
                }
                ExtOpcode::SET3D => {
                    self.set(Reg::D, 3);
                }
                ExtOpcode::SET3E => {
                    self.set(Reg::E, 3);
                }
                ExtOpcode::SET3H => {
                    self.set(Reg::H, 3);
                }
                ExtOpcode::SET3L => {
                    self.set(Reg::L, 3);
                }
                ExtOpcode::SET3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(3, value));
                    self.m = 4;
                }
                ExtOpcode::SET3A => {
                    self.set(Reg::A, 3);
                }
                ExtOpcode::SET4B => {
                    self.set(Reg::B, 4);
                }
                ExtOpcode::SET4C => {
                    self.set(Reg::C, 4);
                }
                ExtOpcode::SET4D => {
                    self.set(Reg::D, 4);
                }
                ExtOpcode::SET4E => {
                    self.set(Reg::E, 4);
                }
                ExtOpcode::SET4H => {
                    self.set(Reg::H, 4);
                }
                ExtOpcode::SET4L => {
                    self.set(Reg::L, 4);
                }
                ExtOpcode::SET4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(4, value));
                    self.m = 4;
                }
                ExtOpcode::SET4A => {
                    self.set(Reg::A, 4);
                }
                ExtOpcode::SET5B => {
                    self.set(Reg::B, 5);
                }
                ExtOpcode::SET5C => {
                    self.set(Reg::C, 5);
                }
                ExtOpcode::SET5D => {
                    self.set(Reg::D, 5);
                }
                ExtOpcode::SET5E => {
                    self.set(Reg::E, 5);
                }
                ExtOpcode::SET5H => {
                    self.set(Reg::H, 5);
                }
                ExtOpcode::SET5L => {
                    self.set(Reg::L, 5);
                }
                ExtOpcode::SET5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(5, value));
                    self.m = 4;
                }
                ExtOpcode::SET5A => {
                    self.set(Reg::A, 5);
                }
                ExtOpcode::SET6B => {
                    self.set(Reg::B, 6);
                }
                ExtOpcode::SET6C => {
                    self.set(Reg::C, 6);
                }
                ExtOpcode::SET6D => {
                    self.set(Reg::D, 6);
                }
                ExtOpcode::SET6E => {
                    self.set(Reg::E, 6);
                }
                ExtOpcode::SET6H => {
                    self.set(Reg::H, 6);
                }
                ExtOpcode::SET6L => {
                    self.set(Reg::L, 6);
                }
                ExtOpcode::SET6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(6, value));
                    self.m = 4;
                }
                ExtOpcode::SET6A => {
                    self.set(Reg::A, 6);
                }
                ExtOpcode::SET7B => {
                    self.set(Reg::B, 7);
                }
                ExtOpcode::SET7C => {
                    self.set(Reg::C, 7);
                }
                ExtOpcode::SET7D => {
                    self.set(Reg::D, 7);
                }
                ExtOpcode::SET7E => {
                    self.set(Reg::E, 7);
                }
                ExtOpcode::SET7H => {
                    self.set(Reg::H, 7);
                }
                ExtOpcode::SET7L => {
                    self.set(Reg::L, 7);
                }
                ExtOpcode::SET7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(7, value));
                    self.m = 4;
                }
                ExtOpcode::SET7A => {
                    self.set(Reg::A, 7);
                }
            },
            None => {
                println!("Unsupported ext operation: {:?}", mmu.read_byte(pc));
            }
        }

        self.clock.m.wrapping_add(self.m as u32);
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

    fn restore(&mut self, copy: CPU) {
        self.a = copy.a;
        self.f = copy.f;
        self.b = copy.b;
        self.c = copy.c;
        self.d = copy.d;
        self.e = copy.e;
        self.h = copy.h;
        self.l = copy.l;
    }

    fn get_reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::A => self.a,
            Reg::B => self.b,
            Reg::C => self.c,
            Reg::D => self.d,
            Reg::E => self.e,
            Reg::H => self.h,
            Reg::L => self.l,
        }
    }

    fn set_reg(&mut self, reg: Reg, value: u8) {
        match reg {
            Reg::A => self.a = value,
            Reg::B => self.b = value,
            Reg::C => self.c = value,
            Reg::D => self.d = value,
            Reg::E => self.e = value,
            Reg::H => self.h = value,
            Reg::L => self.l = value,
        }
    }

    fn add(&mut self, value: u8) {
        self.f = 0;
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

    fn adc(&mut self, value: u8) {
        let carry = self.test_flag(Flag::C);
        self.f = 0;
        let value = value.wrapping_add(carry);
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

    fn sub(&mut self, value: u8) {
        self.f = 0;
        self.set_flag(Flag::N);
        if check_half_borrow_8(self.a, value) {
            self.set_flag(Flag::H);
        }
        if check_borrow_8(self.a, value) {
            self.set_flag(Flag::C);
        }
        self.a = self.a.wrapping_sub(value);
        if self.a == 0 {
            self.set_flag(Flag::Z);
        }
        self.m = 1;
    }

    fn sbc(&mut self, value: u8) {
        let carry = self.test_flag(Flag::C);
        self.f = 0;
        let value = value.wrapping_add(carry);
        self.set_flag(Flag::N);
        if check_half_borrow_8(self.a, value) {
            self.set_flag(Flag::H);
        }
        if check_borrow_8(self.a, value) {
            self.set_flag(Flag::C);
        }
        self.a = self.a.wrapping_sub(value);
        if self.a == 0 {
            self.set_flag(Flag::Z);
        }
        self.m = 1;
    }

    fn and(&mut self, value: u8) {
        self.f = 0;
        self.set_flag(Flag::H);
        self.a &= value;
        if self.a == 0 {
            self.set_flag(Flag::Z);
        }
        self.m = 1;
    }

    fn xor(&mut self, value: u8) {
        self.f = 0;
        self.a ^= value;
        if self.a == 0 {
            self.set_flag(Flag::Z);
        }
        self.m = 1;
    }

    fn or(&mut self, value: u8) {
        self.f = 0;
        self.a |= value;
        if self.a == 0 {
            self.set_flag(Flag::Z);
        }
        self.m = 1;
    }

    fn cmp(&mut self, value: u8) {
        self.f = 0;
        self.set_flag(Flag::N);
        if check_half_borrow_8(self.a, value) {
            self.set_flag(Flag::H);
        }
        if check_borrow_8(self.a, value) {
            self.set_flag(Flag::C);
        }
        if self.a == value {
            self.set_flag(Flag::Z);
        }
        self.m = 1;
    }

    fn inc_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        if check_half_carry_8(value, 1) {
            self.set_flag(Flag::H);
        }
        let value = value.wrapping_add(1);
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn inc(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.inc_val(value);
        self.set_reg(reg, value);
        self.m = 1;
    }

    fn dec_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        self.set_flag(Flag::N);
        if check_half_borrow_8(value, 1) {
            self.set_flag(Flag::H);
        }
        let value = value.wrapping_sub(1);
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn dec(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.dec_val(value);
        self.set_reg(reg, value);
        self.m = 1;
    }

    fn rlc_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        let b7 = (value & 0x80) >> 7;
        if b7 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        let value = value.rotate_left(1);
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn rlc(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.rlc_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn rrc_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        let b0 = value & 0x1;
        if b0 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        let value = value.rotate_right(1);
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn rrc(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.rrc_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn rl_val(&mut self, value: u8) -> u8 {
        let carry = self.test_flag(Flag::C);
        self.f = 0;
        let b7 = (value & 0x80) >> 7;
        let value = (value << 1) | carry;
        if b7 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn rl(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.rl_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn rr_val(&mut self, value: u8) -> u8 {
        let carry = self.test_flag(Flag::C);
        self.f = 0;
        let b0 = value & 0x1;
        let value = (value >> 1) | (carry << 7);
        if b0 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn rr(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.rr_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn sla_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        let b7 = (value & 0x80) >> 7;
        if b7 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        let value = value << 1;
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn sla(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.sla_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn sra_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        let msb = value & 0x80;
        let b0 = value & 0x1;
        if b0 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        let value = msb + (value >> 1);
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn sra(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.sra_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn swap_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        let upper = value >> 4;
        let lower = value & 0xf;
        let value = (lower << 4) + upper;
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn swap(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.swap_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn srl_val(&mut self, value: u8) -> u8 {
        self.f = 0;
        let b0 = value & 0x1;
        if b0 == 1 {
            self.set_flag(Flag::C);
        } else {
            self.reset_flag(Flag::C);
        }
        let value = value >> 1;
        if value == 0 {
            self.set_flag(Flag::Z);
        }
        value
    }

    fn srl(&mut self, reg: Reg) {
        let mut value = self.get_reg(reg);
        value = self.srl_val(value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn bit_val(&mut self, value: u8, bit: u8) {
        self.f = 0;
        self.set_flag(Flag::H);
        if test_bit(bit, value) == 0 {
            self.set_flag(Flag::Z);
        }
    }

    fn bit(&mut self, reg: Reg, bit: u8) {
        let value = self.get_reg(reg);
        self.bit_val(value, bit);
        self.m = 2;
    }

    fn res(&mut self, reg: Reg, bit: u8) {
        let mut value = self.get_reg(reg);
        value = reset_bit(bit, value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn set(&mut self, reg: Reg, bit: u8) {
        let mut value = self.get_reg(reg);
        value = set_bit(bit, value);
        self.set_reg(reg, value);
        self.m = 2;
    }

    fn call(&mut self, cond: Option<Condition>, mmu: &mut MMU) {
        self.m = 3;
        if let Some(c) = cond {
            let should_call = match c {
                Condition::Z => self.test_flag(Flag::Z) == 1,
                Condition::NZ => self.test_flag(Flag::Z) == 0,
                Condition::C => self.test_flag(Flag::C) == 1,
                Condition::NC => self.test_flag(Flag::C) == 0,
            };
            if should_call {
                self.do_call(mmu);
            } else {
                self.pc = self.pc.wrapping_add(2);
            }
        } else {
            self.do_call(mmu);
        }
    }

    fn do_call(&mut self, mmu: &mut MMU) {
        self.sp = self.sp.wrapping_sub(2);
        mmu.write_word(self.sp, self.pc.wrapping_add(2));
        self.pc = mmu.read_word(self.pc);
        self.m += 3;
    }
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

    fn cpu_exec(cpu: &mut CPU, op: u8, b1: u8, b2: u8) {
        cpu.pc = 0;
        cpu.exec(&mut mmu_stub(op, b1, b2)).unwrap();
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
    fn test_bc() {
        let mut cpu = CPU::new();
        cpu.b = 0xf2;
        cpu.c = 0xa3;
        assert_eq!(cpu.bc(), 0xf2a3);
    }

    #[test]
    fn test_de() {
        let mut cpu = CPU::new();
        cpu.d = 0xf2;
        cpu.e = 0xa3;
        assert_eq!(cpu.de(), 0xf2a3);
    }

    #[test]
    fn test_hl() {
        let mut cpu = CPU::new();
        cpu.h = 0xf2;
        cpu.l = 0xa3;
        assert_eq!(cpu.hl(), 0xf2a3);
    }

    fn assert_flags_eq(cpu: &CPU, z: u8, n: u8, h: u8, c: u8) {
        assert_eq!(cpu.test_flag(Flag::Z), z);
        assert_eq!(cpu.test_flag(Flag::N), n);
        assert_eq!(cpu.test_flag(Flag::H), h);
        assert_eq!(cpu.test_flag(Flag::C), c);
    }

    fn registers() -> Vec<Reg> {
        vec![Reg::A, Reg::B, Reg::C, Reg::D, Reg::E, Reg::H, Reg::L]
    }

    #[test]
    fn test_add() {
        let mut cpu = CPU::new();
        cpu.a = 0;
        cpu.add(0x1f);
        assert_eq!(cpu.a, 0x1f);
        assert_flags_eq(&cpu, 0, 0, 0, 0);

        cpu.add(0xe1);
        assert_eq!(cpu.a, 0);
        assert_flags_eq(&cpu, 1, 0, 1, 1);
    }

    #[test]
    fn test_adc() {
        let mut cpu = CPU::new();
        cpu.a = 0;
        cpu.set_flag(Flag::C);

        cpu.adc(0x1e);
        assert_eq!(cpu.a, 0x1f);
        assert_flags_eq(&cpu, 0, 0, 0, 0);

        cpu.set_flag(Flag::C);
        cpu.adc(0xe0);
        assert_eq!(cpu.a, 0);
        assert_flags_eq(&cpu, 1, 0, 1, 1);
    }

    #[test]
    fn test_sub() {
        let mut cpu = CPU::new();
        cpu.a = 0xff;
        cpu.sub(0x1f);
        assert_eq!(cpu.a, 0xe0);
        assert_flags_eq(&cpu, 0, 1, 0, 0);

        cpu.sub(0xe0);
        assert_eq!(cpu.a, 0);
        assert_flags_eq(&cpu, 1, 1, 0, 0);

        cpu.a = 0x1f;
        cpu.sub(0xff);
        assert_eq!(cpu.a, 0x20);
        assert_flags_eq(&cpu, 0, 1, 0, 1);

        cpu.a = 0x10;
        cpu.sub(0xf);
        assert_eq!(cpu.a, 1);
        assert_flags_eq(&cpu, 0, 1, 1, 0);
    }

    #[test]
    fn test_sbc() {
        let mut cpu = CPU::new();
        cpu.a = 0xff;
        cpu.set_flag(Flag::C);
        cpu.sbc(0x1e);
        assert_eq!(cpu.a, 0xe0);
        assert_flags_eq(&cpu, 0, 1, 0, 0);

        cpu.set_flag(Flag::C);
        cpu.sbc(0xdf);
        assert_eq!(cpu.a, 0);
        assert_flags_eq(&cpu, 1, 1, 0, 0);

        cpu.a = 0x1f;
        cpu.set_flag(Flag::C);
        cpu.sbc(0xfe);
        assert_eq!(cpu.a, 0x20);
        assert_flags_eq(&cpu, 0, 1, 0, 1);

        cpu.a = 0x10;
        cpu.set_flag(Flag::C);
        cpu.sbc(0xe);
        assert_eq!(cpu.a, 1);
        assert_flags_eq(&cpu, 0, 1, 1, 0);
    }

    #[test]
    fn test_and() {
        let mut cpu = CPU::new();
        cpu.a = 0xff;

        cpu.and(0);
        assert_eq!(cpu.a, 0);
        assert_flags_eq(&cpu, 1, 0, 1, 0);

        cpu.a = 0xff;
        cpu.and(0xf0);
        assert_eq!(cpu.a, 0xf0);
        assert_flags_eq(&cpu, 0, 0, 1, 0);
    }

    #[test]
    fn test_or() {
        let mut cpu = CPU::new();
        cpu.a = 0xff;

        cpu.or(0);
        assert_eq!(cpu.a, 0xff);
        assert_flags_eq(&cpu, 0, 0, 0, 0);

        cpu.a = 0xf0;
        cpu.or(0xf);
        assert_eq!(cpu.a, 0xff);
        assert_flags_eq(&cpu, 0, 0, 0, 0);

        cpu.a = 0x0;
        cpu.or(0x0);
        assert_eq!(cpu.a, 0x0);
        assert_flags_eq(&cpu, 1, 0, 0, 0);
    }

    #[test]
    fn test_xor() {
        let mut cpu = CPU::new();
        cpu.a = 0xff;

        cpu.xor(0xf0);
        assert_eq!(cpu.a, 0xf);
        assert_flags_eq(&cpu, 0, 0, 0, 0);

        cpu.a = 0xff;
        cpu.xor(0xff);
        assert_eq!(cpu.a, 0);
        assert_flags_eq(&cpu, 1, 0, 0, 0);
    }

    #[test]
    fn test_cmp() {
        let mut cpu = CPU::new();
        cpu.a = 0xff;

        cpu.cmp(0xe0);
        assert_eq!(cpu.a, 0xff);
        assert_flags_eq(&cpu, 0, 1, 0, 0);

        cpu.cmp(0xff);
        assert_eq!(cpu.a, 0xff);
        assert_flags_eq(&cpu, 1, 1, 0, 0);

        cpu.a = 0x10;
        cpu.cmp(0xff);
        assert_eq!(cpu.a, 0x10);
        assert_flags_eq(&cpu, 0, 1, 1, 1);
    }

    #[test]
    fn test_inc() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();
            cpu.set_reg(r, 0xf);
            cpu.inc(r);
            assert_eq!(cpu.get_reg(r), 0x10);
            assert_flags_eq(&cpu, 0, 0, 1, 0);

            cpu.set_reg(r, 0xff);
            cpu.inc(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 1, 0);
        }
    }

    #[test]
    fn test_dec() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();
            cpu.set_reg(r, 0x10);
            cpu.dec(r);
            assert_eq!(cpu.get_reg(r), 0xf);
            assert_flags_eq(&cpu, 0, 1, 1, 0);

            cpu.set_reg(r, 1);
            cpu.dec(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 1, 0, 0);
        }
    }

    #[test]
    fn test_rlc() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b1000_1000);
            cpu.rlc(r);
            assert_eq!(cpu.get_reg(r), 0b0001_0001);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0);
            cpu.rlc(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);
        }
    }

    #[test]
    fn test_rrc() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b0001_0001);
            cpu.rrc(r);
            assert_eq!(cpu.get_reg(r), 0b1000_1000);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0);
            cpu.rrc(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);
        }
    }

    #[test]
    fn test_rl() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b1000_1000);
            cpu.rl(r);
            assert_eq!(cpu.get_reg(r), 0b0001_0000);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0b0000_1000);
            cpu.set_flag(Flag::C);
            cpu.rl(r);
            assert_eq!(cpu.get_reg(r), 0b0001_0001);
            assert_flags_eq(&cpu, 0, 0, 0, 0);

            cpu.set_reg(r, 0);
            cpu.reset_flag(Flag::C);
            cpu.rl(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);

            cpu.set_reg(r, 0);
            cpu.set_flag(Flag::C);
            cpu.rl(r);
            assert_eq!(cpu.get_reg(r), 1);
            assert_flags_eq(&cpu, 0, 0, 0, 0)
        }
    }

    #[test]
    fn test_rr() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b1000_1001);
            cpu.rr(r);
            assert_eq!(cpu.get_reg(r), 0b0100_0100);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0b0010_0000);
            cpu.set_flag(Flag::C);
            cpu.rr(r);
            assert_eq!(cpu.get_reg(r), 0b1001_0000);
            assert_flags_eq(&cpu, 0, 0, 0, 0);

            cpu.set_reg(r, 0);
            cpu.reset_flag(Flag::C);
            cpu.rr(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);

            cpu.set_reg(r, 0x80);
            cpu.set_flag(Flag::C);
            cpu.rr(r);
            assert_eq!(cpu.get_reg(r), 0xc0);
            assert_flags_eq(&cpu, 0, 0, 0, 0)
        }
    }

    #[test]
    fn test_sla() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b1000_1001);
            cpu.sla(r);
            assert_eq!(cpu.get_reg(r), 0b0001_0010);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0);
            cpu.sla(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);
        }
    }

    #[test]
    fn test_sra() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b1000_1001);
            cpu.sra(r);
            assert_eq!(cpu.get_reg(r), 0b1100_0100);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0);
            cpu.sra(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);
        }
    }

    #[test]
    fn test_swap() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0x12);
            cpu.swap(r);
            assert_eq!(cpu.get_reg(r), 0x21);
            assert_flags_eq(&cpu, 0, 0, 0, 0);

            cpu.set_reg(r, 0xf);
            cpu.swap(r);
            assert_eq!(cpu.get_reg(r), 0xf0);
            assert_flags_eq(&cpu, 0, 0, 0, 0);

            cpu.set_reg(r, 0);
            cpu.swap(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);
        }
    }

    #[test]
    fn test_srl() {
        for r in registers().into_iter() {
            let mut cpu = CPU::new();

            cpu.set_reg(r, 0b1000_1001);
            cpu.srl(r);
            assert_eq!(cpu.get_reg(r), 0b0100_0100);
            assert_flags_eq(&cpu, 0, 0, 0, 1);

            cpu.set_reg(r, 0);
            cpu.srl(r);
            assert_eq!(cpu.get_reg(r), 0);
            assert_flags_eq(&cpu, 1, 0, 0, 0);
        }
    }

    #[test]
    fn test_bit() {
        for r in registers().into_iter() {
            for b in 0..8 {
                let mut cpu = CPU::new();

                cpu.set_reg(r, 0);
                cpu.bit(r, b);
                assert_flags_eq(&cpu, 1, 0, 1, 0);

                cpu.set_reg(r, 1 << b);
                cpu.bit(r, b);
                assert_flags_eq(&cpu, 0, 0, 1, 0);
            }
        }
    }

    #[test]
    fn test_res() {
        for r in registers().into_iter() {
            for b in 0..8 {
                let mut cpu = CPU::new();

                cpu.set_reg(r, 0xff);
                cpu.res(r, b);
                assert_eq!(cpu.get_reg(r), 0xff ^ (1 << b));

                cpu.set_reg(r, 0);
                cpu.res(r, b);
                assert_eq!(cpu.get_reg(r), 0);
            }
        }
    }

    #[test]
    fn test_set() {
        for r in registers().into_iter() {
            for b in 0..8 {
                let mut cpu = CPU::new();

                cpu.set_reg(r, 0);
                cpu.set(r, b);
                assert_eq!(cpu.get_reg(r), (1 << b));

                cpu.set_reg(r, 0xff);
                cpu.set(r, b);
                assert_eq!(cpu.get_reg(r), 0xff);
            }
        }
    }

    #[test]
    fn test_8bit_loads() {
        let mut cpu = CPU::new();
        let test_data = [
            (0x06, 1),
            (0x0e, 2),
            (0x16, 3),
            (0x1e, 4),
            (0x26, 5),
            (0x2e, 6),
        ];
        for (op, val) in &test_data {
            cpu_exec(&mut cpu, *op, *val, 0);
        }
        assert_eq!(cpu.c, 2);
        assert_eq!(cpu.b, 1);
        assert_eq!(cpu.c, 2);
        assert_eq!(cpu.d, 3);
        assert_eq!(cpu.e, 4);
        assert_eq!(cpu.h, 5);
        assert_eq!(cpu.l, 6);

        let test_data = [
            (0x7f, 0),
            (0x78, 1),
            (0x79, 2),
            (0x7a, 3),
            (0x7b, 4),
            (0x7c, 5),
            (0x7d, 6),
        ];
        for (op, val) in &test_data {
            cpu_exec(&mut cpu, *op, *val, 0);
            assert_eq!(cpu.a, *val);
        }
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
