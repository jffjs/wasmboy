use emulator::IntFlag;
use num::{FromPrimitive, ToPrimitive};
use std::cell::{Cell, RefCell};
use std::ops::{BitAnd, BitOr};

const SCAN_WIDTH: usize = 160 * 4;

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
    scan: Cell<usize>,
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
            scan: Cell::new(0),
        }
    }

    pub fn execute(&self, cpu_m: u8) -> Option<IntFlag> {
        self.inc_clock(cpu_m);

        match self.mode() {
            Mode::Hblank => {
                if self.clock() >= 51 {
                    if self.ly() == 143 {
                        self.set_mode(Mode::Vblank);
                        return Some(IntFlag::Vblank);
                    } else {
                        self.set_mode(Mode::OAM);
                    }
                    self.inc_ly();
                    self.jump_scan();
                    self.reset_clock();
                }
            }
            Mode::Vblank => {
                if self.clock() >= 114 {
                    self.reset_clock();
                    self.inc_ly();
                    if self.ly() > 153 {
                        self.reset_ly();
                        self.reset_scan();
                        self.set_mode(Mode::OAM);
                    }
                }
            }
            Mode::OAM => {
                if self.clock() >= 20 {
                    self.set_mode(Mode::VRAM);
                    self.reset_clock();
                }
            }
            Mode::VRAM => {
                if self.clock() >= 43 {
                    self.reset_clock();
                    self.set_mode(Mode::Hblank);
                    self.render_scanline();
                }
            }
        }

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
                    0x40 => self.lcdc.set(value),
                    0x41 => self.stat.set(value),
                    0x42 => self.scy.set(value),
                    0x43 => self.scx.set(value),
                    0x44 => (), // read-only
                    0x45 => self.lyc.set(value),
                    0x46 => (), // TODO: DMA transfer
                    0x47 => self.bgp.set(value),
                    0x48 => self.obp0.set(value),
                    0x49 => self.obp1.set(value),
                    0x4a => self.wy.set(value),
                    0x4b => self.wx.set(value),
                    _ => (),
                },
                _ => (),
            },
            _ => (),
        }
    }

    fn render_scanline(&self) {}

    fn lcd_on(&self) -> bool {
        (self.lcdc.get() & 0x80) == 0x80
    }

    fn window_on(&self) -> bool {
        (self.lcdc.get() & 0x20) == 0x20
    }

    fn obj_on(&self) -> bool {
        (self.lcdc.get() & 0x2) == 0x2
    }

    fn bg_on(&self) -> bool {
        (self.lcdc.get() & 0x1) == 0x1
    }

    fn mode(&self) -> Mode {
        Mode::from_u8(self.stat.get() & 0x3).unwrap()
    }

    fn set_mode(&self, mode: Mode) {
        self.stat.replace(mode | self.stat.get());
    }

    fn clock(&self) -> u8 {
        self.clock.get()
    }

    fn inc_clock(&self, value: u8) {
        self.clock.set(self.clock.get() + value);
    }

    fn reset_clock(&self) {
        self.clock.set(0);
    }

    fn ly(&self) -> u8 {
        self.ly.get()
    }

    fn inc_ly(&self) {
        self.ly.set(self.ly.get() + 1);
    }

    fn reset_ly(&self) {
        self.ly.set(0);
    }

    fn scan(&self) -> usize {
        self.scan.get()
    }

    fn jump_scan(&self) {
        self.scan.set(self.scan.get() + SCAN_WIDTH);
    }

    fn reset_scan(&self) {
        self.scan.set(0);
    }
}

#[derive(Debug, PartialEq, FromPrimitive, ToPrimitive)]
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_read_byte() {
        let gpu = GPU::new();

        gpu.vram.borrow_mut()[0xfa] = 0x34;
        gpu.vram.borrow_mut()[0xcfa] = 0x22;
        assert_eq!(gpu.read_byte(0x80fa), 0x34);
        assert_eq!(gpu.read_byte(0x8cfa), 0x22);

        gpu.lcdc.set(0x11);
        assert_eq!(gpu.read_byte(0xff40), 0x11);

        gpu.oam.borrow_mut()[0x80] = 0xdc;
        assert_eq!(gpu.read_byte(0xfe80), 0xdc);
    }

    #[test]
    fn test_write_byte() {
        let gpu = GPU::new();

        gpu.write_byte(0x80fa, 0x34);
        gpu.write_byte(0x8cfa, 0x22);
        assert_eq!(gpu.vram.borrow()[0xfa], 0x34);
        assert_eq!(gpu.vram.borrow()[0xcfa], 0x22);

        gpu.write_byte(0xff40, 0x11);
        assert_eq!(gpu.lcdc.get(), 0x11);

        gpu.write_byte(0xfe80, 0xdc);
        assert_eq!(gpu.oam.borrow()[0x80], 0xdc);
    }

    #[test]
    fn test_mode() {
        let gpu = GPU::new();
        assert_eq!(gpu.mode(), Mode::Hblank);
        gpu.set_mode(Mode::VRAM);
        assert_eq!(gpu.mode(), Mode::VRAM);
    }
}
