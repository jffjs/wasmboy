use mmu::MMU;
use num::FromPrimitive;
use opcode::*;

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
  clock: Clock,
}

struct Clock {
  m: u32,
  t: u32,
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
      clock: Clock { m: 0, t: 0 },
    }
  }

  pub fn exec(&mut self, mmu: &mut MMU) -> Result<(), &str> {
    match Opcode::from_u8(mmu.read_byte(self.pc)) {
      Some(op) => match op {
        Opcode::NOP => {
          self.m = 1;
        }
        Opcode::LDBCnn => {
          self.c = mmu.read_byte(self.pc);
          self.b = mmu.read_byte(self.pc + 1);
          self.pc += 2;
          self.m = 3;
        }
        Opcode::LD_BC_A => {
          let addr = ((self.b as u16) << 8) + self.c as u16;
          mmu.write_byte(addr, self.a);
          self.m = 2;
        }
        Opcode::INCBC => {
          self.c += 1;
          if self.c == 0 {
            self.b += 1;
          }
          self.m = 1;
        }
        Opcode::INCB => {
          self.b += 1;
          // set zero flag
          if self.b == 0 {
            self.f |= 0x80;
          }
          // set half-carry flag
          self.f |= ((self.b & 0xf) + 1) & 0x10;
          self.m = 1;
        }
        Opcode::DECB => {
          self.b -= 1;
          if self.b == 0 {
            self.f |= 0x80;
          }
          self.f |= 0x40;
          self.f |= ((self.b & 0xf) - 1) & 0x10;
          self.m = 1;
        }
        Opcode::LDBn => {
          self.b = mmu.read_byte(self.pc);
          self.pc += 1;
          self.m = 2;
        },
        Opcode::RLCA => {
          self.f &= 0xbf;
          self.f &= 0xdf;
          let msb = (self.a & 0x80) >> 7;
          if msb == 1 {
            self.f |= 0x10;
          } else {
            self.f &= 0xef;
          }
          self.a = (self.a << 1) | msb;
          self.m = 1;
        },
        Opcode::LD_nn_SP => {
          let addr = mmu.read_word(self.pc);
          self.pc +=1;
          mmu.write_word(addr, self.sp);
          self.m = 5;
        },
        Opcode::ADDHLBC => {
          self.f &= 0xbf;
          let mut hl = ((self.h as u16) << 8) | self.l as u16;
          let bc = ((self.b as u16) << 8) | self.c as u16;
          // check half carry
          hl = hl + bc;
          self.h = (hl >> 8) as u8;
          self.l = (hl & 0x00ff) as u8;
          self.m = 2; // jsGb has this as 3?
        },
        _ => return Err("Unsupported operation."),
      },
      None => return Err("Unsupported operation."),
    }

    self.pc += 1;
    Ok(())
  }
}
