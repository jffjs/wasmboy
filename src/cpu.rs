struct Register {
  a: i8,
  f: i8,
  b: i8,
  c: i8,
  d: i8,
  e: i8,
  h: i8,
  l: i8,
}

pub struct CPU {
  pc: i16,
  sp: i16,
  a: i8,
  f: i8,
  b: i8,
  c: i8,
  d: i8,
  e: i8,
  h: i8,
  l: i8,
  m: i8,
  t: i8,
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
    }
  }
}
