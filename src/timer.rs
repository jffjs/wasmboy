use emulator::IntFlag;
use std::cell::Cell;

struct Clock {
  sub: Cell<u8>,
  main: Cell<u8>,
  div: Cell<u8>,
}

impl Clock {
  fn new() -> Clock {
    Clock {
      sub: Cell::new(0),
      main: Cell::new(0),
      div: Cell::new(0),
    }
  }

  fn sub(&self) -> u8 {
    self.sub.get()
  }

  fn main(&self) -> u8 {
    self.main.get()
  }

  fn div(&self) -> u8 {
    self.div.get()
  }

  fn inc_sub(&self, value: u8) {
    self.sub.set(self.sub.get() + value);
  }

  fn correct_sub(&self) {
    self.sub.set(self.sub.get() - 4);
  }

  fn inc_div(&self) {
    self.div.set(self.div.get().wrapping_add(1));
  }

  fn reset_div(&self) {
    self.div.set(0);
  }

  fn inc_main(&self) {
    self.main.set(self.main.get().wrapping_add(1));
  }

  fn reset_main(&self) {
    self.main.set(0);
  }
}

pub struct Timer {
  clock: Clock,
  div: Cell<u8>,
  tma: Cell<u8>,
  tima: Cell<u8>,
  tac: Cell<u8>,
}

impl Timer {
  pub fn new() -> Timer {
    Timer {
      clock: Clock::new(),
      div: Cell::new(0),
      tma: Cell::new(0),
      tima: Cell::new(0),
      tac: Cell::new(0),
    }
  }

  pub fn inc(&self, cpu_m: u8) -> Option<IntFlag> {
    self.clock.inc_sub(cpu_m);
    if self.clock.sub() >= 4 {
      self.clock.correct_sub();
      self.clock.inc_main();
      self.clock.inc_div();

      if self.clock.div() == 16 {
        self.clock.reset_div();
        self.div.set(self.div.get().wrapping_add(1));
      }
    }

    if (self.tac.get() & 0x4) > 0 {
      let threshold = match self.tac.get() & 0x3 {
        0 => 64,
        1 => 1,
        2 => 4,
        3 => 16,
        _ => panic!("timer unreachable"),
      };

      if self.clock.main() >= threshold {
        self.clock.reset_main();
        self.tima.set(self.tima.get().wrapping_add(1));
        if self.tima.get() == 0 {
          self.tima.set(self.tma.get());
          return Some(IntFlag::TimerOverflow);
        }
      }
    }
    None
  }

  pub fn read_byte(&self, addr: u16) -> u8 {
    match addr {
      0xff04 => self.div.get(),
      0xff05 => self.tima.get(),
      0xff06 => self.tma.get(),
      0xff07 => self.tac.get(),
      _ => 0,
    }
  }

  pub fn write_byte(&self, addr: u16, value: u8) {
    match addr {
      0xff04 => self.div.set(0),
      0xff05 => self.tima.set(value),
      0xff06 => self.tma.set(value),
      0xff07 => self.tac.set(value & 0x7),
      _ => (),
    }
  }
}
