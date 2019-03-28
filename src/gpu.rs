use emulator::IntFlag;
use num::{FromPrimitive, ToPrimitive};
use std::cell::{Cell, RefCell};
use std::ops::{BitAnd, BitOr};

const SCAN_WIDTH: usize = 160 * 4;

type Screen = [u8; 144 * 160];

struct Tile<'a> {
    data: &'a [u8],
}

impl<'a> Tile<'a> {
    fn new(data: &'a [u8]) -> Tile {
        Tile { data }
    }

    fn color_at(&self, x: u8, y: u8) -> u8 {
        let index = (y * 2) as usize;
        let t0 = self.data[index];
        let t1 = self.data[index + 1];
        let b0 = (t0 >> (7 - x)) & 1;
        let b1 = ((t1 >> (7 - x)) << 1) & 2;
        b0 + b1
    }
}

struct Sprite<'a> {
    data: &'a [u8],
}

impl<'a> Sprite<'a> {
    fn new(data: &'a [u8]) -> Sprite {
        Sprite { data }
    }

    fn x(&self) -> u8 {
        self.data[1].wrapping_sub(8)
    }

    fn y(&self) -> u8 {
        self.data[0].wrapping_sub(16)
    }

    fn tile(&self) -> u8 {
        self.data[2]
    }

    fn has_priority(&self) -> bool {
        self.data[3] & 0x80 == 0x80
    }

    fn y_flip(&self) -> bool {
        self.data[3] & 0x40 == 0x40
    }

    fn x_flip(&self) -> bool {
        self.data[3] & 0x20 == 0x20
    }

    fn palette(&self) -> u8 {
        (self.data[3] >> 4) & 1
    }
}

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

    pub fn execute(&self, cpu_m: u8, screen: &mut Screen) -> Option<IntFlag> {
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
                    if self.lcd_on() {
                        self.render_scanline(screen);
                    }
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
                    0x44 => self.reset_ly(),
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

    fn render_scanline(&self, screen: &mut Screen) {
        if self.bg_on() {
            self.render_bg(screen);
        }

        if self.obj_on() {
            self.render_sprites(screen);
        }
    }

    fn render_bg(&self, screen: &mut Screen) {
        let tilemap_offset = self.bgmap_offset().wrapping_add(self.bg_line() as usize) >> 3;
        let mut line_offset = (self.scx() >> 3) as usize;
        let mut pixel_x = self.scx() & 7;
        let pixel_y = self.bg_line() & 7;
        let vram = self.vram.borrow();
        let mut tile_num = vram[tilemap_offset + line_offset];
        let mut tile_addr = self.bg_tile_addr(tile_num) as usize;
        let mut tile = Tile::new(&vram[tile_addr..tile_addr + 16]);

        for x in 0..160 {
            // TODO: if this is too slow, we can calculate when vram is updated
            // and keep tile data in separate data structure
            let t_color = tile.color_at(pixel_x, pixel_y);
            let p_color = self.bgp_color(t_color);
            // push to screen array
            screen[self.ly() as usize * 144 + x] = p_color;
            pixel_x += 1;
            // move to next tile
            if pixel_x > 7 {
                pixel_x = 0;
                line_offset = (line_offset + 1) & 31;
                tile_num = vram[tilemap_offset + line_offset];
                tile_addr = self.bg_tile_addr(tile_num);
                tile = Tile::new(&vram[tile_addr..tile_addr + 16]);
            }
        }
    }

    fn render_window(&self, screen: &mut Screen) {}

    fn render_sprites(&self, screen: &mut Screen) {
        let mut sprite_count = 0;
        for i in 0..0xa0 {
            let oam = self.oam.borrow();
            let sprite_addr = 0xfe00 + i * 4;
            let sprite = Sprite::new(&oam[sprite_addr..sprite_addr + 4]);
            let line = self.ly();
            let sprite_x = sprite.x();
            let sprite_y = sprite.y();

            // sprite is hidden
            if sprite_y >= 144 || sprite_count >= 10 {
                continue;
            }

            if sprite_y <= line && sprite_y + 8 > line {
                sprite_count += 1;
                // sprite is on line, but hidden - still counts for max sprites
                if sprite_x >= 160 {
                    continue;
                }

                let pixel_y = if sprite.y_flip() {
                    7 - (line - sprite_y)
                } else {
                    line - sprite_y
                };

                for x in 0..8 {
                    let vram = &self.vram.borrow();
                    let screen_coord = (line * 144 + sprite_x) as usize;
                    let mut tile_num = sprite.tile();
                    let mut tile_addr = self.sprite_tile_addr(tile_num);
                    let mut tile = Tile::new(&vram[tile_addr..tile_addr + 16]);
                    if sprite_x + x < 160 && (sprite.has_priority() || screen[screen_coord] != 0) {
                        let pixel_x = if sprite.x_flip() { 7 - x } else { x };
                        let t_color = tile.color_at(pixel_x, pixel_y);
                        let p_color = if sprite.palette() == 0 {
                            self.obp0_color(t_color)
                        } else {
                            self.obp1_color(t_color)
                        };

                        screen[screen_coord] = p_color;
                    }
                }
            }
        }
    }

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

    fn bgmap_offset(&self) -> usize {
        if self.lcdc.get() & 0x8 == 0x8 {
            0x1c00
        } else {
            0x1800
        }
    }

    fn bg_line(&self) -> u8 {
        (self.ly().wrapping_add(self.scy()))
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

    fn scy(&self) -> u8 {
        self.scy.get()
    }

    fn scx(&self) -> u8 {
        self.scx.get()
    }

    fn bg_tile_addr(&self, tile: u8) -> usize {
        if (self.lcdc.get() & 0x10) == 0x10 {
            (tile * 16) as usize
        } else {
            if (tile & 0x80) == 0x80 {
                0x1000 - ((!tile + 1) * 16) as usize
            } else {
                0x1000 + (tile * 16) as usize
            }
        }
    }

    fn sprite_tile_addr(&self, tile: u8) -> usize {
        (tile * 16) as usize
    }

    fn bgp_color(&self, color: u8) -> u8 {
        self.bgp.get() >> ((color & 3) * 2) & 3
    }

    fn obp0_color(&self, color: u8) -> u8 {
        self.obp0.get() >> ((color & 3) * 2) & 3
    }

    fn obp1_color(&self, color: u8) -> u8 {
        self.obp1.get() >> ((color & 3) * 2) & 3
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
