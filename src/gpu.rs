use emulator::IntFlag;
use num::{FromPrimitive, ToPrimitive};
use std::cell::{Cell, RefCell};
use std::ops::{BitAnd, BitOr};

pub struct GPU {
    vram: RefCell<[u8; 0x2000]>,
    oam: RefCell<[u8; 0xa0]>,
    clock: Cell<u8>,
    lcdc: Cell<u8>,
    stat: Cell<u8>,
    scy: Cell<u8>,
    scx: Cell<u8>,
    ly: Cell<u8>,
    lyc: Cell<u8>,
    bgp: Cell<u8>,
    obp0: Cell<u8>,
    obp1: Cell<u8>,
    wy: Cell<u8>,
    wx: Cell<u8>,
}

impl GPU {
    pub fn new() -> GPU {
        GPU {
            vram: RefCell::new([0; 0x2000]),
            oam: RefCell::new([0; 0xa0]),
            clock: Cell::new(0),
            lcdc: Cell::new(0x91),
            stat: Cell::new(0),
            scy: Cell::new(0),
            scx: Cell::new(0),
            ly: Cell::new(0),
            lyc: Cell::new(0),
            bgp: Cell::new(0),
            obp0: Cell::new(0),
            obp1: Cell::new(0),
            wy: Cell::new(0),
            wx: Cell::new(0),
        }
    }

    pub fn execute(&self) -> Option<IntFlag> {
        None
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match (addr & 0xf000) >> 12 {
            0x8 | 0x9 => self.vram.borrow()[(addr & 0x1fff) as usize],
            0xf => match (addr & 0xf00) >> 8 {
                0xe => {
                    if addr < 0xfea0 {
                        self.oam.borrow()[(addr & 0x9f) as usize]
                    } else {
                        0
                    }
                }
                0xf => match addr & 0xff {
                    0x40 => self.lcdc.get(),
                    0x41 => self.stat.get(),
                    0x42 => self.scy.get(),
                    0x43 => self.scx.get(),
                    0x44 => self.ly.get(),
                    0x45 => self.lyc.get(),
                    0x46 => 0, // write-only
                    0x47 => self.bgp.get(),
                    0x48 => self.obp0.get(),
                    0x49 => self.obp1.get(),
                    0x4a => self.wy.get(),
                    0x4b => self.wx.get(),
                    _ => 0,
                },
                _ => 0,
            },
            _ => 0,
        }
    }

    pub fn write_byte(&self, addr: u16, value: u8) {
        match (addr & 0xf000) >> 12 {
            0x8 | 0x9 => self.vram.borrow_mut()[(addr & 0x1fff) as usize] = value,
            0xf => match (addr & 0xf00) >> 8 {
                0xe => {
                    if addr < 0xfea0 {
                        self.oam.borrow_mut()[(addr & 0x9f) as usize] = value;
                    }
                }
                0xf => match addr & 0xff {
                    0x40 => {
                        self.lcdc.replace(value);
                    }
                    0x41 => {
                        self.stat.replace(value);
                    }
                    0x42 => {
                        self.scy.replace(value);
                    }
                    0x43 => {
                        self.scx.replace(value);
                    }
                    0x44 => {
                        self.ly.replace(value);
                    }
                    0x45 => {
                        self.lyc.replace(value);
                    }
                    0x46 => (), // write-only
                    0x47 => {
                        self.bgp.replace(value);
                    }
                    0x48 => {
                        self.obp0.replace(value);
                    }
                    0x49 => {
                        self.obp1.replace(value);
                    }
                    0x4a => {
                        self.wy.replace(value);
                    }
                    0x4b => {
                        self.wx.replace(value);
                    }
                    _ => (),
                },
                _ => (),
            },
            _ => (),
        }
    }

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
