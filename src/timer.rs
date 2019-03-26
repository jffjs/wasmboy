use emulator::IntFlag;
use mmu::MMU;

pub struct Timer {
  clock_s: u8,
  clock_m: u8,
  clock_d: u8,
  div: u8,
  tma: u8,
  tima: u8,
  tac: u8,
}

impl Timer {
  pub fn new() -> Timer {
    Timer {
      clock_d: 0,
      clock_m: 0,
      clock_s: 0,
      div: 0,
      tma: 0,
      tima: 0,
      tac: 0,
    }
  }

  pub fn inc(&mut self, mmu: &mut MMU, cpu_m: u8) {
    self.clock_d += cpu_m;
    if self.clock_d >= 4 {
      self.clock_m += 1;
      self.clock_s -= 4;
      self.clock_d += 1;

      if self.clock_d == 16 {
        self.clock_d = 0;
        self.div.wrapping_add(1);
      }
    }

    if (self.tac & 0x4) > 0 {
      let threshold = match self.tac & 0x3 {
        0 => 64,
        1 => 1,
        2 => 4,
        3 => 16,
        _ => panic!("unreachable"),
      };

      if self.clock_m >= threshold {
        self.clock_m = 0;
        self.tima.wrapping_add(1);
        if self.tima == 0 {
          self.tima = self.tma;
          mmu.set_iflag(IntFlag::TimerOverflow);
        }
      }
    }
  }
}
