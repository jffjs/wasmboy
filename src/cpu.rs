use crate::{emulator::IntFlag, io_device::IoDevice, mmu::MMU, opcode::*, utils::*};
use num::FromPrimitive;
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

#[derive(Clone, Copy)]
enum Reg16 {
    AF,
    BC,
    DE,
    HL,
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

    pub fn halted(&mut self) -> bool {
        if self.halt {
            self.clock.m += 1;
        }
        self.halt
    }

    pub fn stop(&self) -> bool {
        self.stop
    }

    pub fn handle_interrupt(&mut self, iflag: IntFlag, mmu: &mut MMU) {
        match iflag {
            IntFlag::Vblank => self.rst(0x40, mmu),
            IntFlag::LCDC => self.rst(0x48, mmu),
            IntFlag::TimerOverflow => self.rst(0x50, mmu),
            IntFlag::SerialIO => self.rst(0x58, mmu),
            IntFlag::JoyPad => self.rst(0x60, mmu),
        }
        self.clock.m += self.m as u32;
    }

    pub fn exec(&mut self, mmu: &mut IoDevice) -> Result<(), String> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
        let opcode = Opcode::from_u8(mmu.read_byte(pc));
        match opcode {
            Some(op) => match op {
                Opcode::NOP => self.m = 1,
                Opcode::LDBCnn => self.ldrrnn(Reg::B, Reg::C, mmu),
                Opcode::LD_BC_A => self.ld_rr_r(Reg16::BC, Reg::A, mmu),
                Opcode::INCBC => self.inc16(Reg16::BC),
                Opcode::INCB => self.inc(Reg::B),
                Opcode::DECB => self.dec(Reg::B),
                Opcode::LDBn => self.ldrn(Reg::B, mmu),
                Opcode::RLCA => {
                    self.rlc(Reg::A);
                    self.m = 1;
                }
                Opcode::LD_nn_SP => {
                    let addr = mmu.read_word(self.pc);
                    self.pc = self.pc.wrapping_add(2);
                    mmu.write_word(addr, self.sp);
                    self.m = 5;
                }
                Opcode::ADDHLBC => self.add16(Reg16::HL, Reg16::BC),
                Opcode::LDA_BC_ => self.ldr_rr_(Reg::A, Reg16::BC, mmu),
                Opcode::DECBC => self.dec16(Reg16::BC),
                Opcode::INCC => self.inc(Reg::C),
                Opcode::DECC => self.dec(Reg::C),
                Opcode::LDCn => self.ldrn(Reg::C, mmu),
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
                Opcode::LDDEnn => self.ldrrnn(Reg::D, Reg::E, mmu),
                Opcode::LD_DE_A => self.ld_rr_r(Reg16::DE, Reg::A, mmu),
                Opcode::INCDE => self.inc16(Reg16::DE),
                Opcode::INCD => self.inc(Reg::D),
                Opcode::DECD => self.dec(Reg::D),
                Opcode::LDDn => self.ldrn(Reg::D, mmu),
                Opcode::RLA => {
                    self.rl(Reg::A);
                    self.m = 1;
                }
                Opcode::JRn => self.jump_relative(None, mmu),
                Opcode::ADDHLDE => self.add16(Reg16::HL, Reg16::DE),
                Opcode::LDA_DE_ => self.ldr_rr_(Reg::A, Reg16::DE, mmu),
                Opcode::DECDE => self.dec16(Reg16::DE),
                Opcode::INCE => self.inc(Reg::E),
                Opcode::DECE => self.dec(Reg::E),
                Opcode::LDEn => self.ldrn(Reg::E, mmu),
                Opcode::RRA => {
                    self.rr(Reg::A);
                    self.m = 1;
                }
                Opcode::JRNZn => self.jump_relative(Some(Condition::NZ), mmu),
                Opcode::LDHLnn => self.ldrrnn(Reg::D, Reg::E, mmu),
                Opcode::LDI_HL_A => {
                    mmu.write_byte(self.hl(), self.a);
                    self.l = self.l.wrapping_add(1);
                    if self.l == 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::INCHL => self.inc16(Reg16::HL),
                Opcode::INCH => self.inc(Reg::H),
                Opcode::DECH => self.dec(Reg::H),
                Opcode::LDHn => self.ldrn(Reg::H, mmu),
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
                Opcode::JRZn => self.jump_relative(Some(Condition::Z), mmu),
                Opcode::ADDHLHL => self.add16(Reg16::HL, Reg16::HL),
                Opcode::LDIA_HL_ => {
                    self.a = mmu.read_byte(self.hl());
                    self.l = self.l.wrapping_add(1);
                    if self.l == 0 {
                        self.h = self.h.wrapping_add(1);
                    }
                    self.m = 2;
                }
                Opcode::DECHL => self.dec16(Reg16::HL),
                Opcode::INCL => self.inc(Reg::L),
                Opcode::DECL => self.dec(Reg::L),
                Opcode::LDLn => self.ldrn(Reg::L, mmu),
                Opcode::CPL => {
                    self.set_flag(Flag::N);
                    self.set_flag(Flag::H);
                    self.a ^= 0xff;
                    self.m = 1;
                }
                Opcode::JRNCn => self.jump_relative(Some(Condition::NC), mmu),
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
                Opcode::JRCn => self.jump_relative(Some(Condition::C), mmu),
                Opcode::ADDHLSP => {
                    self.add16_val(Reg16::HL, self.sp);
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
                Opcode::INCA => self.inc(Reg::A),
                Opcode::DECA => self.dec(Reg::A),
                Opcode::LDAn => self.ldrn(Reg::A, mmu),
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
                Opcode::LDBB => self.ldrr(Reg::B, Reg::B),
                Opcode::LDBC => self.ldrr(Reg::B, Reg::C),
                Opcode::LDBD => self.ldrr(Reg::B, Reg::D),
                Opcode::LDBE => self.ldrr(Reg::B, Reg::E),
                Opcode::LDBH => self.ldrr(Reg::B, Reg::H),
                Opcode::LDBL => self.ldrr(Reg::B, Reg::L),
                Opcode::LDB_HL_ => self.ldr_rr_(Reg::B, Reg16::HL, mmu),
                Opcode::LDBA => self.ldrr(Reg::B, Reg::A),
                Opcode::LDCB => self.ldrr(Reg::C, Reg::B),
                Opcode::LDCC => self.ldrr(Reg::C, Reg::C),
                Opcode::LDCD => self.ldrr(Reg::C, Reg::D),
                Opcode::LDCE => self.ldrr(Reg::C, Reg::E),
                Opcode::LDCH => self.ldrr(Reg::C, Reg::H),
                Opcode::LDCL => self.ldrr(Reg::C, Reg::L),
                Opcode::LDC_HL_ => self.ldr_rr_(Reg::C, Reg16::HL, mmu),
                Opcode::LDCA => self.ldrr(Reg::C, Reg::A),
                Opcode::LDDB => self.ldrr(Reg::D, Reg::B),
                Opcode::LDDC => self.ldrr(Reg::D, Reg::C),
                Opcode::LDDD => self.ldrr(Reg::D, Reg::D),
                Opcode::LDDE => self.ldrr(Reg::D, Reg::E),
                Opcode::LDDH => self.ldrr(Reg::D, Reg::H),
                Opcode::LDDL => self.ldrr(Reg::D, Reg::L),
                Opcode::LDD_HL_ => self.ldr_rr_(Reg::D, Reg16::HL, mmu),
                Opcode::LDDA => self.ldrr(Reg::D, Reg::A),
                Opcode::LDEB => self.ldrr(Reg::E, Reg::B),
                Opcode::LDEC => self.ldrr(Reg::E, Reg::C),
                Opcode::LDED => self.ldrr(Reg::E, Reg::D),
                Opcode::LDEE => self.ldrr(Reg::E, Reg::E),
                Opcode::LDEH => self.ldrr(Reg::E, Reg::H),
                Opcode::LDEL => self.ldrr(Reg::E, Reg::L),
                Opcode::LDE_HL_ => self.ldr_rr_(Reg::E, Reg16::HL, mmu),
                Opcode::LDEA => self.ldrr(Reg::E, Reg::A),
                Opcode::LDHB => self.ldrr(Reg::H, Reg::B),
                Opcode::LDHC => self.ldrr(Reg::H, Reg::C),
                Opcode::LDHD => self.ldrr(Reg::H, Reg::D),
                Opcode::LDHE => self.ldrr(Reg::H, Reg::E),
                Opcode::LDHH => self.ldrr(Reg::H, Reg::H),
                Opcode::LDHL => self.ldrr(Reg::H, Reg::L),
                Opcode::LDH_HL_ => self.ldr_rr_(Reg::H, Reg16::HL, mmu),
                Opcode::LDHA => self.ldrr(Reg::H, Reg::A),
                Opcode::LDLB => self.ldrr(Reg::L, Reg::L),
                Opcode::LDLC => self.ldrr(Reg::L, Reg::L),
                Opcode::LDLD => self.ldrr(Reg::L, Reg::L),
                Opcode::LDLE => self.ldrr(Reg::L, Reg::L),
                Opcode::LDLH => self.ldrr(Reg::L, Reg::L),
                Opcode::LDLL => self.ldrr(Reg::L, Reg::L),
                Opcode::LDL_HL_ => self.ldr_rr_(Reg::L, Reg16::HL, mmu),
                Opcode::LDLA => self.ldrr(Reg::L, Reg::A),
                Opcode::LD_HL_B => self.ld_rr_r(Reg16::HL, Reg::B, mmu),
                Opcode::LD_HL_C => self.ld_rr_r(Reg16::HL, Reg::C, mmu),
                Opcode::LD_HL_D => self.ld_rr_r(Reg16::HL, Reg::D, mmu),
                Opcode::LD_HL_E => self.ld_rr_r(Reg16::HL, Reg::E, mmu),
                Opcode::LD_HL_H => self.ld_rr_r(Reg16::HL, Reg::H, mmu),
                Opcode::LD_HL_L => self.ld_rr_r(Reg16::HL, Reg::L, mmu),
                Opcode::HALT => {
                    self.halt = true;
                    self.m = 1;
                }
                Opcode::LD_HL_A => self.ld_rr_r(Reg16::HL, Reg::A, mmu),
                Opcode::LDAB => self.ldrr(Reg::A, Reg::B),
                Opcode::LDAC => self.ldrr(Reg::A, Reg::C),
                Opcode::LDAD => self.ldrr(Reg::A, Reg::D),
                Opcode::LDAE => self.ldrr(Reg::A, Reg::E),
                Opcode::LDAH => self.ldrr(Reg::A, Reg::H),
                Opcode::LDAL => self.ldrr(Reg::A, Reg::L),
                Opcode::LDA_HL_ => self.ldr_rr_(Reg::A, Reg16::HL, mmu),
                Opcode::LDAA => self.ldrr(Reg::A, Reg::A),
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
                    self.ret(Some(Condition::NZ), mmu);
                }
                Opcode::POPBC => self.pop(Reg16::BC, mmu),
                Opcode::JPNZnn => {
                    self.jump(Some(Condition::NZ), mmu);
                }
                Opcode::JPnn => {
                    self.jump(None, mmu);
                }
                Opcode::CALLNZnn => {
                    self.call(Some(Condition::NZ), mmu);
                }
                Opcode::PUSHBC => self.push(Reg16::BC, mmu),
                Opcode::ADDAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.add(value);
                    self.m += 1;
                }
                Opcode::RST0 => self.rst(0, mmu),
                Opcode::RETZ => self.ret(Some(Condition::Z), mmu),
                Opcode::RET => self.ret(None, mmu),
                Opcode::JPZnn => self.jump(Some(Condition::Z), mmu),
                Opcode::ExtOps => return self.exec_ext(mmu),
                Opcode::CALLZnn => self.call(Some(Condition::Z), mmu),
                Opcode::CALLnn => self.call(None, mmu),
                Opcode::ADCAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.adc(value);
                    self.m += 1;
                }
                Opcode::RST8 => self.rst(0x8, mmu),
                Opcode::RETNC => self.ret(Some(Condition::NC), mmu),
                Opcode::POPDE => self.pop(Reg16::DE, mmu),
                Opcode::JPNCnn => self.jump(Some(Condition::NC), mmu),
                Opcode::CALLNCnn => self.call(Some(Condition::NC), mmu),
                Opcode::PUSHDE => self.push(Reg16::DE, mmu),
                Opcode::SUBAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.sub(value);
                    self.m += 1;
                }
                Opcode::RST10 => self.rst(0x10, mmu),
                Opcode::RETC => self.ret(Some(Condition::C), mmu),
                Opcode::RETI => {
                    self.do_ret(mmu);
                    self.ime = true;
                    // self.restore(mmu.rrs());
                    self.m = 3;
                }
                Opcode::JPCnn => self.jump(Some(Condition::C), mmu),
                Opcode::CALLCnn => self.call(Some(Condition::C), mmu),
                Opcode::SBCAn => {
                    let value = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.sbc(value);
                    self.m += 1;
                }
                Opcode::RST18 => self.rst(0x18, mmu),
                Opcode::LDH_n_A => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    let addr = 0xff00 + (n as u16);
                    mmu.write_byte(addr, self.a);
                    self.m = 3;
                }
                Opcode::POPHL => self.pop(Reg16::HL, mmu),
                Opcode::LDH_C_A => {
                    let addr = 0xff00 + (self.c as u16);
                    mmu.write_byte(addr, self.a);
                    self.m = 2;
                }
                Opcode::PUSHHL => self.push(Reg16::HL, mmu),
                Opcode::ANDn => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.and(n);
                    self.m += 1;
                }
                Opcode::RST20 => self.rst(0x20, mmu),
                Opcode::ADDSPn => {
                    self.f = 0;
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    let sp = self.sp;
                    if (offset & 0x80) == 0x80 {
                        self.sp = sp.wrapping_sub((!offset + 1) as u16);
                    } else {
                        self.sp = sp.wrapping_add(offset as u16);
                    }
                    if (sp ^ (offset as u16) ^ self.sp) & 0x10 == 0x10 {
                        self.set_flag(Flag::H);
                    }
                    if (sp ^ (offset as u16) ^ self.sp) & 0x100 == 0x100 {
                        self.set_flag(Flag::C);
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
                Opcode::RST28 => self.rst(0x28, mmu),
                Opcode::LDHA_n_ => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    let addr = 0xff00 + (n as u16);
                    self.a = mmu.read_byte(addr);
                    self.m = 3;
                }
                Opcode::POPAF => self.pop(Reg16::AF, mmu),
                Opcode::LDHA_C_ => {
                    self.a = mmu.read_byte(0xff00 + (self.c as u16));
                    self.m = 2;
                }
                Opcode::DI => {
                    self.ime = false;
                    self.m = 1;
                }
                Opcode::PUSHAF => self.push(Reg16::AF, mmu),
                Opcode::ORn => {
                    let n = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    self.or(n);
                    self.m += 1;
                }
                Opcode::RST30 => self.rst(0x30, mmu),
                Opcode::LDHLSPn => {
                    self.f = 0;
                    let offset = mmu.read_byte(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    let sp = self.sp;
                    let spn: u16;
                    if (offset & 0x80) == 0x80 {
                        spn = sp.wrapping_sub((!offset + 1) as u16);
                    } else {
                        spn = sp.wrapping_add(offset as u16);
                    }
                    if (sp ^ (offset as u16) ^ spn) & 0x10 == 0x10 {
                        self.set_flag(Flag::H);
                    }
                    if (sp ^ (offset as u16) ^ spn) & 0x100 == 0x100 {
                        self.set_flag(Flag::C);
                    }
                    self.set_reg16(Reg16::HL, spn);
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
                Opcode::RST38 => self.rst(0x38, mmu),
            },
            None => {
                panic!("Unsupported operation: {}", mmu.read_byte(pc));
            }
        }

        self.clock.m += self.m as u32;
        Ok(())
    }

    fn exec_ext(&mut self, mmu: &mut IoDevice) -> Result<(), String> {
        let pc = self.pc;
        self.pc = self.pc.wrapping_add(1);
        match ExtOpcode::from_u8(mmu.read_byte(pc)) {
            Some(op) => match op {
                ExtOpcode::RLCB => self.rlc(Reg::B),
                ExtOpcode::RLCC => self.rlc(Reg::C),
                ExtOpcode::RLCD => self.rlc(Reg::D),
                ExtOpcode::RLCE => self.rlc(Reg::E),
                ExtOpcode::RLCH => self.rlc(Reg::H),
                ExtOpcode::RLCL => self.rlc(Reg::H),
                ExtOpcode::RLC_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rlc_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RLCA => self.rlc(Reg::A),
                ExtOpcode::RRCB => self.rrc(Reg::B),
                ExtOpcode::RRCC => self.rrc(Reg::C),
                ExtOpcode::RRCD => self.rrc(Reg::D),
                ExtOpcode::RRCE => self.rrc(Reg::E),
                ExtOpcode::RRCH => self.rrc(Reg::H),
                ExtOpcode::RRCL => self.rrc(Reg::L),
                ExtOpcode::RRC_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rrc_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RRCA => self.rrc(Reg::A),
                ExtOpcode::RLB => self.rl(Reg::B),
                ExtOpcode::RLC => self.rl(Reg::C),
                ExtOpcode::RLD => self.rl(Reg::D),
                ExtOpcode::RLE => self.rl(Reg::E),
                ExtOpcode::RLH => self.rl(Reg::H),
                ExtOpcode::RLL => self.rl(Reg::L),
                ExtOpcode::RL_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rl_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RLA => self.rl(Reg::A),
                ExtOpcode::RRB => self.rr(Reg::B),
                ExtOpcode::RRC => self.rr(Reg::C),
                ExtOpcode::RRD => self.rr(Reg::D),
                ExtOpcode::RRE => self.rr(Reg::E),
                ExtOpcode::RRH => self.rr(Reg::H),
                ExtOpcode::RRL => self.rr(Reg::L),
                ExtOpcode::RR_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.rr_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::RRA => self.rr(Reg::A),
                ExtOpcode::SLAB => self.sla(Reg::B),
                ExtOpcode::SLAC => self.sla(Reg::C),
                ExtOpcode::SLAD => self.sla(Reg::D),
                ExtOpcode::SLAE => self.sla(Reg::E),
                ExtOpcode::SLAH => self.sla(Reg::H),
                ExtOpcode::SLAL => self.sla(Reg::L),
                ExtOpcode::SLA_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.sla_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SLAA => self.sla(Reg::A),
                ExtOpcode::SRAB => self.sra(Reg::B),
                ExtOpcode::SRAC => self.sra(Reg::C),
                ExtOpcode::SRAD => self.sra(Reg::D),
                ExtOpcode::SRAE => self.sra(Reg::E),
                ExtOpcode::SRAH => self.sra(Reg::H),
                ExtOpcode::SRAL => self.sra(Reg::L),
                ExtOpcode::SRA_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.sra_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SRAA => self.sra(Reg::A),
                ExtOpcode::SWAPB => self.swap(Reg::B),
                ExtOpcode::SWAPC => self.swap(Reg::C),
                ExtOpcode::SWAPD => self.swap(Reg::D),
                ExtOpcode::SWAPE => self.swap(Reg::E),
                ExtOpcode::SWAPH => self.swap(Reg::H),
                ExtOpcode::SWAPL => self.swap(Reg::L),
                ExtOpcode::SWAP_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.swap_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SWAPA => self.swap(Reg::A),
                ExtOpcode::SRLB => self.srl(Reg::B),
                ExtOpcode::SRLC => self.srl(Reg::C),
                ExtOpcode::SRLD => self.srl(Reg::D),
                ExtOpcode::SRLE => self.srl(Reg::E),
                ExtOpcode::SRLH => self.srl(Reg::H),
                ExtOpcode::SRLL => self.srl(Reg::L),
                ExtOpcode::SRL_HL_ => {
                    let addr = self.hl();
                    let mut value = mmu.read_byte(addr);
                    value = self.srl_val(value);
                    mmu.write_byte(addr, value);
                    self.m = 4;
                }
                ExtOpcode::SRLA => self.srl(Reg::A),
                ExtOpcode::BIT0B => self.bit(Reg::B, 0),
                ExtOpcode::BIT0C => self.bit(Reg::C, 0),
                ExtOpcode::BIT0D => self.bit(Reg::D, 0),
                ExtOpcode::BIT0E => self.bit(Reg::E, 0),
                ExtOpcode::BIT0H => self.bit(Reg::H, 0),
                ExtOpcode::BIT0L => self.bit(Reg::L, 0),
                ExtOpcode::BIT0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 0);
                    self.m = 3;
                }
                ExtOpcode::BIT0A => self.bit(Reg::A, 0),
                ExtOpcode::BIT1B => self.bit(Reg::B, 1),
                ExtOpcode::BIT1C => self.bit(Reg::C, 1),
                ExtOpcode::BIT1D => self.bit(Reg::D, 1),
                ExtOpcode::BIT1E => self.bit(Reg::E, 1),
                ExtOpcode::BIT1H => self.bit(Reg::H, 1),
                ExtOpcode::BIT1L => self.bit(Reg::L, 1),
                ExtOpcode::BIT1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 1);
                    self.m = 3;
                }
                ExtOpcode::BIT1A => self.bit(Reg::A, 1),
                ExtOpcode::BIT2B => self.bit(Reg::B, 2),
                ExtOpcode::BIT2C => self.bit(Reg::C, 2),
                ExtOpcode::BIT2D => self.bit(Reg::D, 2),
                ExtOpcode::BIT2E => self.bit(Reg::E, 2),
                ExtOpcode::BIT2H => self.bit(Reg::H, 2),
                ExtOpcode::BIT2L => self.bit(Reg::L, 2),
                ExtOpcode::BIT2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 2);
                    self.m = 3;
                }
                ExtOpcode::BIT2A => self.bit(Reg::A, 2),
                ExtOpcode::BIT3B => self.bit(Reg::B, 3),
                ExtOpcode::BIT3C => self.bit(Reg::C, 3),
                ExtOpcode::BIT3D => self.bit(Reg::D, 3),
                ExtOpcode::BIT3E => self.bit(Reg::E, 3),
                ExtOpcode::BIT3H => self.bit(Reg::H, 3),
                ExtOpcode::BIT3L => self.bit(Reg::L, 3),
                ExtOpcode::BIT3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 3);
                    self.m = 3;
                }
                ExtOpcode::BIT3A => self.bit(Reg::A, 3),
                ExtOpcode::BIT4B => self.bit(Reg::B, 4),
                ExtOpcode::BIT4C => self.bit(Reg::C, 4),
                ExtOpcode::BIT4D => self.bit(Reg::D, 4),
                ExtOpcode::BIT4E => self.bit(Reg::E, 4),
                ExtOpcode::BIT4H => self.bit(Reg::H, 4),
                ExtOpcode::BIT4L => self.bit(Reg::L, 4),
                ExtOpcode::BIT4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 4);
                    self.m = 3;
                }
                ExtOpcode::BIT4A => self.bit(Reg::A, 4),
                ExtOpcode::BIT5B => self.bit(Reg::B, 5),
                ExtOpcode::BIT5C => self.bit(Reg::C, 5),
                ExtOpcode::BIT5D => self.bit(Reg::D, 5),
                ExtOpcode::BIT5E => self.bit(Reg::E, 5),
                ExtOpcode::BIT5H => self.bit(Reg::H, 5),
                ExtOpcode::BIT5L => self.bit(Reg::L, 5),
                ExtOpcode::BIT5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 5);
                    self.m = 3;
                }
                ExtOpcode::BIT5A => self.bit(Reg::A, 5),
                ExtOpcode::BIT6B => self.bit(Reg::B, 6),
                ExtOpcode::BIT6C => self.bit(Reg::C, 6),
                ExtOpcode::BIT6D => self.bit(Reg::D, 6),
                ExtOpcode::BIT6E => self.bit(Reg::E, 6),
                ExtOpcode::BIT6H => self.bit(Reg::H, 6),
                ExtOpcode::BIT6L => self.bit(Reg::L, 6),
                ExtOpcode::BIT6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 6);
                    self.m = 3;
                }
                ExtOpcode::BIT6A => self.bit(Reg::A, 6),
                ExtOpcode::BIT7B => self.bit(Reg::B, 7),
                ExtOpcode::BIT7C => self.bit(Reg::C, 7),
                ExtOpcode::BIT7D => self.bit(Reg::D, 7),
                ExtOpcode::BIT7E => self.bit(Reg::E, 7),
                ExtOpcode::BIT7H => self.bit(Reg::H, 7),
                ExtOpcode::BIT7L => self.bit(Reg::L, 7),
                ExtOpcode::BIT7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    self.bit_val(value, 7);
                    self.m = 3;
                }
                ExtOpcode::BIT7A => self.bit(Reg::A, 7),
                ExtOpcode::RES0B => self.res(Reg::B, 0),
                ExtOpcode::RES0C => self.res(Reg::C, 0),
                ExtOpcode::RES0D => self.res(Reg::D, 0),
                ExtOpcode::RES0E => self.res(Reg::E, 0),
                ExtOpcode::RES0H => self.res(Reg::H, 0),
                ExtOpcode::RES0L => self.res(Reg::L, 0),
                ExtOpcode::RES0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(0, value));
                    self.m = 4;
                }
                ExtOpcode::RES0A => self.res(Reg::A, 0),
                ExtOpcode::RES1B => self.res(Reg::B, 1),
                ExtOpcode::RES1C => self.res(Reg::C, 1),
                ExtOpcode::RES1D => self.res(Reg::D, 1),
                ExtOpcode::RES1E => self.res(Reg::E, 1),
                ExtOpcode::RES1H => self.res(Reg::H, 1),
                ExtOpcode::RES1L => self.res(Reg::L, 1),
                ExtOpcode::RES1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(1, value));
                    self.m = 4;
                }
                ExtOpcode::RES1A => self.res(Reg::A, 1),
                ExtOpcode::RES2B => self.res(Reg::B, 2),
                ExtOpcode::RES2C => self.res(Reg::C, 2),
                ExtOpcode::RES2D => self.res(Reg::D, 2),
                ExtOpcode::RES2E => self.res(Reg::E, 2),
                ExtOpcode::RES2H => self.res(Reg::H, 2),
                ExtOpcode::RES2L => self.res(Reg::L, 2),
                ExtOpcode::RES2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(2, value));
                    self.m = 4;
                }
                ExtOpcode::RES2A => self.res(Reg::A, 2),
                ExtOpcode::RES3B => self.res(Reg::B, 3),
                ExtOpcode::RES3C => self.res(Reg::C, 3),
                ExtOpcode::RES3D => self.res(Reg::D, 3),
                ExtOpcode::RES3E => self.res(Reg::E, 3),
                ExtOpcode::RES3H => self.res(Reg::H, 3),
                ExtOpcode::RES3L => self.res(Reg::L, 3),
                ExtOpcode::RES3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(3, value));
                    self.m = 4;
                }
                ExtOpcode::RES3A => self.res(Reg::A, 3),
                ExtOpcode::RES4B => self.res(Reg::B, 4),
                ExtOpcode::RES4C => self.res(Reg::C, 4),
                ExtOpcode::RES4D => self.res(Reg::D, 4),
                ExtOpcode::RES4E => self.res(Reg::E, 4),
                ExtOpcode::RES4H => self.res(Reg::H, 4),
                ExtOpcode::RES4L => self.res(Reg::L, 4),
                ExtOpcode::RES4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(4, value));
                    self.m = 4;
                }
                ExtOpcode::RES4A => self.res(Reg::A, 4),
                ExtOpcode::RES5B => self.res(Reg::B, 5),
                ExtOpcode::RES5C => self.res(Reg::C, 5),
                ExtOpcode::RES5D => self.res(Reg::D, 5),
                ExtOpcode::RES5E => self.res(Reg::E, 5),
                ExtOpcode::RES5H => self.res(Reg::H, 5),
                ExtOpcode::RES5L => self.res(Reg::L, 5),
                ExtOpcode::RES5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(5, value));
                    self.m = 4;
                }
                ExtOpcode::RES5A => self.res(Reg::A, 5),
                ExtOpcode::RES6B => self.res(Reg::B, 6),
                ExtOpcode::RES6C => self.res(Reg::C, 6),
                ExtOpcode::RES6D => self.res(Reg::D, 6),
                ExtOpcode::RES6E => self.res(Reg::E, 6),
                ExtOpcode::RES6H => self.res(Reg::H, 6),
                ExtOpcode::RES6L => self.res(Reg::L, 6),
                ExtOpcode::RES6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(6, value));
                    self.m = 4;
                }
                ExtOpcode::RES6A => self.res(Reg::A, 6),
                ExtOpcode::RES7B => self.res(Reg::B, 7),
                ExtOpcode::RES7C => self.res(Reg::C, 7),
                ExtOpcode::RES7D => self.res(Reg::D, 7),
                ExtOpcode::RES7E => self.res(Reg::E, 7),
                ExtOpcode::RES7H => self.res(Reg::H, 7),
                ExtOpcode::RES7L => self.res(Reg::L, 7),
                ExtOpcode::RES7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), reset_bit(7, value));
                    self.m = 4;
                }
                ExtOpcode::RES7A => self.res(Reg::A, 7),
                ExtOpcode::SET0B => self.set(Reg::B, 0),
                ExtOpcode::SET0C => self.set(Reg::C, 0),
                ExtOpcode::SET0D => self.set(Reg::D, 0),
                ExtOpcode::SET0E => self.set(Reg::E, 0),
                ExtOpcode::SET0H => self.set(Reg::H, 0),
                ExtOpcode::SET0L => self.set(Reg::L, 0),
                ExtOpcode::SET0_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(0, value));
                    self.m = 4;
                }
                ExtOpcode::SET0A => self.set(Reg::A, 0),
                ExtOpcode::SET1B => self.set(Reg::B, 1),
                ExtOpcode::SET1C => self.set(Reg::C, 1),
                ExtOpcode::SET1D => self.set(Reg::D, 1),
                ExtOpcode::SET1E => self.set(Reg::E, 1),
                ExtOpcode::SET1H => self.set(Reg::H, 1),
                ExtOpcode::SET1L => self.set(Reg::L, 1),
                ExtOpcode::SET1_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(1, value));
                    self.m = 4;
                }
                ExtOpcode::SET1A => self.set(Reg::A, 1),
                ExtOpcode::SET2B => self.set(Reg::B, 2),
                ExtOpcode::SET2C => self.set(Reg::C, 2),
                ExtOpcode::SET2D => self.set(Reg::D, 2),
                ExtOpcode::SET2E => self.set(Reg::E, 2),
                ExtOpcode::SET2H => self.set(Reg::H, 2),
                ExtOpcode::SET2L => self.set(Reg::L, 2),
                ExtOpcode::SET2_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(2, value));
                    self.m = 4;
                }
                ExtOpcode::SET2A => self.set(Reg::A, 2),
                ExtOpcode::SET3B => self.set(Reg::B, 3),
                ExtOpcode::SET3C => self.set(Reg::C, 3),
                ExtOpcode::SET3D => self.set(Reg::D, 3),
                ExtOpcode::SET3E => self.set(Reg::E, 3),
                ExtOpcode::SET3H => self.set(Reg::H, 3),
                ExtOpcode::SET3L => self.set(Reg::L, 3),
                ExtOpcode::SET3_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(3, value));
                    self.m = 4;
                }
                ExtOpcode::SET3A => self.set(Reg::A, 3),
                ExtOpcode::SET4B => self.set(Reg::B, 4),
                ExtOpcode::SET4C => self.set(Reg::C, 4),
                ExtOpcode::SET4D => self.set(Reg::D, 4),
                ExtOpcode::SET4E => self.set(Reg::E, 4),
                ExtOpcode::SET4H => self.set(Reg::H, 4),
                ExtOpcode::SET4L => self.set(Reg::L, 4),
                ExtOpcode::SET4_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(4, value));
                    self.m = 4;
                }
                ExtOpcode::SET4A => self.set(Reg::A, 4),
                ExtOpcode::SET5B => self.set(Reg::B, 5),
                ExtOpcode::SET5C => self.set(Reg::C, 5),
                ExtOpcode::SET5D => self.set(Reg::D, 5),
                ExtOpcode::SET5E => self.set(Reg::E, 5),
                ExtOpcode::SET5H => self.set(Reg::H, 5),
                ExtOpcode::SET5L => self.set(Reg::L, 5),
                ExtOpcode::SET5_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(5, value));
                    self.m = 4;
                }
                ExtOpcode::SET5A => self.set(Reg::A, 5),
                ExtOpcode::SET6B => self.set(Reg::B, 6),
                ExtOpcode::SET6C => self.set(Reg::C, 6),
                ExtOpcode::SET6D => self.set(Reg::D, 6),
                ExtOpcode::SET6E => self.set(Reg::E, 6),
                ExtOpcode::SET6H => self.set(Reg::H, 6),
                ExtOpcode::SET6L => self.set(Reg::L, 6),
                ExtOpcode::SET6_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(6, value));
                    self.m = 4;
                }
                ExtOpcode::SET6A => self.set(Reg::A, 6),
                ExtOpcode::SET7B => self.set(Reg::B, 7),
                ExtOpcode::SET7C => self.set(Reg::C, 7),
                ExtOpcode::SET7D => self.set(Reg::D, 7),
                ExtOpcode::SET7E => self.set(Reg::E, 7),
                ExtOpcode::SET7H => self.set(Reg::H, 7),
                ExtOpcode::SET7L => self.set(Reg::L, 7),
                ExtOpcode::SET7_HL_ => {
                    let value = mmu.read_byte(self.hl());
                    mmu.write_byte(self.hl(), set_bit(7, value));
                    self.m = 4;
                }
                ExtOpcode::SET7A => self.set(Reg::A, 7),
            },
            None => {
                println!("Unsupported ext operation: {:?}", mmu.read_byte(pc));
            }
        }

        self.clock.m.wrapping_add(self.m as u32);
        Ok(())
    }

    fn af(&self) -> u16 {
        ((self.a as u16) << 8) + self.f as u16
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

    fn get_reg16(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => self.af(),
            Reg16::BC => self.bc(),
            Reg16::DE => self.de(),
            Reg16::HL => self.hl(),
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

    fn set_reg16(&mut self, reg: Reg16, value: u16) {
        let hi = hi_byte(value);
        let lo = lo_byte(value);
        match reg {
            Reg16::AF => {
                self.a = hi;
                self.f = lo;
            }
            Reg16::BC => {
                self.b = hi;
                self.c = lo;
            }
            Reg16::DE => {
                self.d = hi;
                self.e = lo;
            }
            Reg16::HL => {
                self.h = hi;
                self.l = lo;
            }
        }
    }

    fn ldrn(&mut self, reg: Reg, mmu: &IoDevice) {
        self.set_reg(reg, mmu.read_byte(self.pc));
        self.pc = self.pc.wrapping_add(1);
        self.m = 2;
    }

    fn ldrrnn(&mut self, hi: Reg, lo: Reg, mmu: &IoDevice) {
        self.set_reg(lo, mmu.read_byte(self.pc));
        self.set_reg(hi, mmu.read_byte(self.pc.wrapping_add(1)));
        self.pc = self.pc.wrapping_add(2);
        self.m = 3;
    }

    fn ldrr(&mut self, to: Reg, from: Reg) {
        let value = self.get_reg(from);
        self.set_reg(to, value);
        self.m = 1;
    }

    fn ldr_rr_(&mut self, to: Reg, addr: Reg16, mmu: &IoDevice) {
        let value = mmu.read_byte(self.get_reg16(addr));
        self.set_reg(to, value);
        self.m = 2;
    }

    fn ld_rr_r(&mut self, addr: Reg16, value: Reg, mmu: &mut IoDevice) {
        let addr = self.get_reg16(addr);
        let value = self.get_reg(value);
        mmu.write_byte(addr, value);
        self.m = 2;
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

    fn add16_val(&mut self, reg: Reg16, value: u16) {
        self.f = 0;
        let r_val = self.get_reg16(reg);
        if check_half_carry_16(r_val, value) {
            self.set_flag(Flag::H);
        }
        if check_carry_16(r_val, value) {
            self.set_flag(Flag::C);
        }
        self.set_reg16(reg, r_val.wrapping_add(value));
    }

    fn add16(&mut self, r1: Reg16, r2: Reg16) {
        self.f = 0;
        let r2_val = self.get_reg16(r2);
        self.add16_val(r1, r2_val);
        self.m = 2; // jsGb has this as 3?
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

    fn inc16(&mut self, reg: Reg16) {
        let value = self.get_reg16(reg);
        self.set_reg16(reg, value.wrapping_add(1));
        self.m = 2; //jsGB has 1?
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

    fn dec16(&mut self, reg: Reg16) {
        let value = self.get_reg16(reg);
        self.set_reg16(reg, value.wrapping_sub(1));
        self.m = 2; //jsGB has 1?
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

    fn condition_is_met(&self, cond: Condition) -> bool {
        match cond {
            Condition::Z => self.test_flag(Flag::Z) == 1,
            Condition::NZ => self.test_flag(Flag::Z) == 0,
            Condition::C => self.test_flag(Flag::C) == 1,
            Condition::NC => self.test_flag(Flag::C) == 0,
        }
    }

    fn call(&mut self, cond: Option<Condition>, mmu: &mut IoDevice) {
        self.m = 3;
        if let Some(c) = cond {
            if self.condition_is_met(c) {
                self.do_call(mmu);
            } else {
                self.pc = self.pc.wrapping_add(2);
            }
        } else {
            self.do_call(mmu);
        }
    }

    fn do_call(&mut self, mmu: &mut IoDevice) {
        self.sp = self.sp.wrapping_sub(2);
        mmu.write_word(self.sp, self.pc.wrapping_add(2));
        self.pc = mmu.read_word(self.pc);
        self.m += 3;
    }

    fn jump(&mut self, cond: Option<Condition>, mmu: &IoDevice) {
        self.m = 3;
        if let Some(c) = cond {
            if self.condition_is_met(c) {
                self.do_jump(mmu);
            } else {
                self.pc = self.pc.wrapping_add(2);
            }
        } else {
            self.do_jump(mmu);
        }
    }

    fn do_jump(&mut self, mmu: &IoDevice) {
        self.pc = mmu.read_word(self.pc);
        self.m += 1;
    }

    fn jump_relative(&mut self, cond: Option<Condition>, mmu: &IoDevice) {
        self.m = 2;
        if let Some(c) = cond {
            if self.condition_is_met(c) {
                self.do_jump_relative(mmu);
            } else {
                self.pc = self.pc.wrapping_add(1);
            }
        } else {
            self.do_jump_relative(mmu);
        }
    }

    fn do_jump_relative(&mut self, mmu: &IoDevice) {
        let offset = mmu.read_byte(self.pc);
        if (offset & 0x80) == 0x80 {
            self.pc = self.pc.wrapping_sub((!offset + 1) as u16);
        } else {
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        self.m += 1;
    }

    fn ret(&mut self, cond: Option<Condition>, mmu: &IoDevice) {
        self.m = 1;
        if let Some(c) = cond {
            if self.condition_is_met(c) {
                self.do_ret(mmu);
            }
        } else {
            self.do_ret(mmu);
        }
    }

    fn do_ret(&mut self, mmu: &IoDevice) {
        self.pc = mmu.read_word(self.sp);
        self.sp = self.sp.wrapping_add(2);
        self.m += 2;
    }

    fn rst(&mut self, addr: u16, mmu: &mut IoDevice) {
        // mmu.rsv(self.clone());
        self.sp = self.sp.wrapping_sub(2);
        mmu.write_word(self.sp, self.pc);
        self.pc = addr;
        self.m = 4;
    }

    fn push(&mut self, reg: Reg16, mmu: &mut IoDevice) {
        let value = self.get_reg16(reg);
        self.sp = self.sp.wrapping_sub(1);
        mmu.write_byte(self.sp, hi_byte(value));
        self.sp = self.sp.wrapping_sub(1);
        mmu.write_byte(self.sp, lo_byte(value));
        self.m = 4;
    }

    fn pop(&mut self, reg: Reg16, mmu: &IoDevice) {
        let lo = mmu.read_byte(self.sp);
        self.sp = self.sp.wrapping_add(1);
        let hi = mmu.read_byte(self.sp);
        self.sp = self.sp.wrapping_add(1);
        self.set_reg16(reg, ((hi as u16) << 8) + (lo as u16));
        self.m = 3;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cartridge::Cartridge,
        gpu::GPU,
        timer::Timer,
        utils::{hi_byte, lo_byte},
    };
    use mockers::Scenario;
    use std::rc::Rc;

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

    fn register_pairs() -> Vec<(Reg, Reg)> {
        vec![(Reg::B, Reg::C), (Reg::D, Reg::E), (Reg::H, Reg::L)]
    }

    #[test]
    fn test_ldrn() {
        for (i, r) in registers().into_iter().enumerate() {
            let mut cpu = CPU::new();
            cpu.pc = 0x100;
            cpu.set_reg(r, 0xff);
            let scenario = Scenario::new();
            let mmu = scenario.create_mock_for::<IoDevice>();

            scenario.expect(mmu.read_byte_call(0x100).and_return(i as u8));
            cpu.ldrn(r, &mmu);
            assert_eq!(cpu.get_reg(r), i as u8);
            assert_eq!(cpu.pc, 0x101);
        }
    }

    #[test]
    fn test_ldrrnn() {
        for (i, (hi, lo)) in register_pairs().into_iter().enumerate() {
            let mut cpu = CPU::new();
            cpu.pc = 0x100;
            cpu.set_reg(hi, 0xff);
            cpu.set_reg(lo, 0xff);
            let scenario = Scenario::new();
            let mmu = scenario.create_mock_for::<IoDevice>();

            let value = (i as u16) << 8;
            scenario.expect(mmu.read_byte_call(0x100).and_return(lo_byte(value)));
            scenario.expect(mmu.read_byte_call(0x101).and_return(hi_byte(value)));
            cpu.ldrrnn(hi, lo, &mmu);
            assert_eq!(cpu.get_reg(hi), hi_byte(value));
            assert_eq!(cpu.get_reg(lo), lo_byte(value));
        }
    }

    #[test]
    fn test_ldrr() {
        for to in registers().into_iter() {
            for from in registers().into_iter() {
                let mut cpu = CPU::new();
                cpu.set_reg(to, 0x12);
                cpu.set_reg(from, 0xab);

                cpu.ldrr(to, from);
                assert_eq!(cpu.get_reg(to), 0xab);
                assert_eq!(cpu.get_reg(from), 0xab);
            }
        }
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
    fn test_call() {
        let mut cpu = CPU::new();
        let scenario = Scenario::new();
        let mut mmu = scenario.create_mock_for::<IoDevice>();
        cpu.sp = 0xfffe;
        cpu.pc = 0x200;

        scenario.expect(mmu.write_word_call(0xfffc, 0x202).and_return(()));
        scenario.expect(mmu.read_word_call(0x200).and_return(0x234));
        cpu.call(None, &mut mmu);
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.pc, 0x234);

        cpu.reset_flag(Flag::Z);
        cpu.call(Some(Condition::Z), &mut mmu);
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.pc, 0x236);

        cpu.set_flag(Flag::Z);
        scenario.expect(mmu.write_word_call(0xfffa, 0x238).and_return(()));
        scenario.expect(mmu.read_word_call(0x236).and_return(0x334));
        cpu.call(Some(Condition::Z), &mut mmu);
        assert_eq!(cpu.sp, 0xfffa);
        assert_eq!(cpu.pc, 0x334);
    }

    #[test]
    fn test_jump() {
        let mut cpu = CPU::new();
        let scenario = Scenario::new();
        let mmu = scenario.create_mock_for::<IoDevice>();
        cpu.pc = 0x200;

        scenario.expect(mmu.read_word_call(0x200).and_return(0x234));
        cpu.jump(None, &mmu);
        assert_eq!(cpu.pc, 0x234);

        cpu.jump(Some(Condition::Z), &mmu);
        assert_eq!(cpu.pc, 0x236);

        cpu.set_flag(Flag::Z);
        scenario.expect(mmu.read_word_call(0x236).and_return(0x123));
        cpu.jump(Some(Condition::Z), &mmu);
        assert_eq!(cpu.pc, 0x123);

        cpu.set_flag(Flag::C);
        scenario.expect(mmu.read_word_call(0x123).and_return(0x456));
        cpu.jump(Some(Condition::C), &mmu);
        assert_eq!(cpu.pc, 0x456);

        cpu.reset_flag(Flag::C);
        scenario.expect(mmu.read_word_call(0x456).and_return(0x400));
        cpu.jump(Some(Condition::NC), &mmu);
        assert_eq!(cpu.pc, 0x400);

        cpu.jump(Some(Condition::C), &mmu);
        assert_eq!(cpu.pc, 0x402);
    }

    #[test]
    fn test_jump_relative() {
        let mut cpu = CPU::new();
        let scenario = Scenario::new();
        let mmu = scenario.create_mock_for::<IoDevice>();
        cpu.pc = 0x200;

        scenario.expect(mmu.read_byte_call(0x200).and_return(0xf));
        cpu.jump_relative(None, &mmu);
        assert_eq!(cpu.pc, 0x20f);

        scenario.expect(mmu.read_byte_call(0x20f).and_return(0xf5));
        cpu.jump_relative(None, &mmu);
        assert_eq!(cpu.pc, 0x204);

        cpu.jump_relative(Some(Condition::C), &mmu);
        assert_eq!(cpu.pc, 0x205);

        scenario.expect(mmu.read_byte_call(0x205).and_return(0xff));
        cpu.jump_relative(Some(Condition::NC), &mmu);
        assert_eq!(cpu.pc, 0x204);
    }

    #[test]
    fn test_ret() {
        let mut cpu = CPU::new();
        let scenario = Scenario::new();
        let mmu = scenario.create_mock_for::<IoDevice>();
        cpu.pc = 0x200;
        cpu.sp = 0xfff4;

        scenario.expect(mmu.read_word_call(0xfff4).and_return(0x150));
        cpu.ret(None, &mmu);
        assert_eq!(cpu.pc, 0x150);
        assert_eq!(cpu.sp, 0xfff6);

        scenario.expect(mmu.read_word_call(0xfff6).and_return(0x100));
        cpu.ret(Some(Condition::NZ), &mmu);
        assert_eq!(cpu.pc, 0x100);
        assert_eq!(cpu.sp, 0xfff8);

        cpu.ret(Some(Condition::C), &mmu);
        assert_eq!(cpu.pc, 0x100);
        assert_eq!(cpu.sp, 0xfff8);
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
}
