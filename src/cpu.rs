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

  pub fn exec(&mut self) {}
}
