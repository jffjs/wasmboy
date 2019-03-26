use emulator::IntFlag;
use num::{FromPrimitive, ToPrimitive};
use std::cell::Cell;
use std::ops::{BitAnd, BitOr};

pub struct GPU {
    clock: Cell<u8>,
    lcdc: Cell<u8>,
    stat: Cell<u8>,
    scy: Cell<u8>,
    scx: Cell<u8>,
    ly: Cell<u8>,
    lyc: Cell<u8>,
}

impl GPU {
    pub fn new() -> GPU {
        GPU {
            clock: Cell::new(0),
            lcdc: Cell::new(0x91),
            stat: Cell::new(0),
            scy: Cell::new(0),
            scx: Cell::new(0),
            ly: Cell::new(0),
            lyc: Cell::new(0),
        }
    }

    pub fn execute(&self) -> Option<IntFlag> {
        None
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        let register = addr - 0xff40;
        match register {
            0 => self.lcdc.get(),
            1 => self.stat.get(),
            2 => self.scy.get(),
            3 => self.scx.get(),
            4 => self.ly.get(),
            5 => self.lyc.get(),
            6 => 0, // write-only
            _ => 0, // TODO: VRAM
        }
    }

    pub fn write_byte(&self, addr: u16, value: u8) {}

    fn mode(&self) -> Mode {
        Mode::from_u8(self.stat.get() & 0x3).unwrap()
    }

    fn set_mode(&self, mode: Mode) {
        self.stat.replace(mode | self.stat.get());
    }
}

#[derive(Debug, FromPrimitive, ToPrimitive)]
enum Mode {
    Hblank = 0x0,
    Vblank = 0x1,
    OAM = 0x2,
    VRAM = 0x3,
}

impl BitAnd<u8> for Mode {
    type Output = u8;

    fn bitand(self, other: u8) -> u8 {
        other & self.to_u8().unwrap()
    }
}

impl BitOr<u8> for Mode {
    type Output = u8;

    fn bitor(self, other: u8) -> u8 {
        other | self.to_u8().unwrap()
    }
}
