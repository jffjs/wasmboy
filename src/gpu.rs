use crate::emulator::IntFlag;
use num::{FromPrimitive, ToPrimitive};
use std::cell::{Cell, RefCell};
use std::ops::{BitAnd, BitOr};

type Screen = [u8];

pub struct Tile<'a> {
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
    big: bool,
}

impl<'a> Sprite<'a> {
    fn new(data: &'a [u8], big: bool) -> Sprite {
        Sprite { data, big }
    }

    fn x(&self) -> u8 {
        self.data[1].wrapping_sub(8)
    }

    fn y(&self) -> u8 {
        self.data[0].wrapping_sub(16)
    }

    fn tile(&self) -> u8 {
        if self.big {
            self.data[2] & 0xfe
        } else {
            self.data[2]
        }
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

    pub fn execute(&self, cpu_m: u8, screen: &mut Screen) -> Vec<IntFlag> {
        let mut interrupts = Vec::new();
        self.inc_clock(cpu_m);

        match self.mode() {
            Mode::Hblank => {
                if self.clock() >= 51 {
                    if self.ly() == 143 {
                        self.set_mode(Mode::Vblank);
                        interrupts.push(IntFlag::Vblank);
                        if self.vblank_int_on() {
                            interrupts.push(IntFlag::LCDC);
                        }
                        return interrupts;
                    } else {
                        if self.oam_int_on() {
                            interrupts.push(IntFlag::LCDC);
                        }
                        self.set_mode(Mode::OAM);
                    }
                    self.inc_ly();
                    self.reset_clock();
                }
            }
            Mode::Vblank => {
                if self.clock() >= 114 {
                    self.reset_clock();
                    self.inc_ly();
                    if self.ly() > 153 {
                        self.reset_ly();
                        if self.oam_int_on() {
                            interrupts.push(IntFlag::LCDC);
                        }
                        self.set_mode(Mode::OAM);
                    }
                }
            }
            Mode::OAM => {
                if self.clock() >= 20 {
                    if self.lyc_int_on() && self.lyc.get() == self.ly() {
                        interrupts.push(IntFlag::LCDC);
                    }
                    self.set_mode(Mode::VRAM);
                    self.reset_clock();
                }
            }
            Mode::VRAM => {
                if self.clock() >= 43 {
                    self.reset_clock();
                    if self.hblank_int_on() {
                        interrupts.push(IntFlag::LCDC);
                    }
                    self.set_mode(Mode::Hblank);
                    if self.lcd_on() {
                        self.render_scanline(screen);
                    }
                }
            }
        }
        interrupts
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
            0x8 | 0x9 => {
                self.vram.borrow_mut()[(addr & 0x1fff) as usize] = value;
            }
            0xf => match (addr & 0xf00) >> 8 {
                0xe => {
                    if addr < 0xfea0 {
                        self.oam.borrow_mut()[(addr & 0x9f) as usize] = value;
                    }
                }
                0xf => match addr & 0xff {
                    0x40 => self.lcdc.set(value),
                    0x41 => self.stat.set(value & 0x78),
                    0x42 => self.scy.set(value),
                    0x43 => self.scx.set(value),
                    0x44 => self.reset_ly(),
                    0x45 => self.lyc.set(value),
                    0x46 => (), // DMA transfer - see MMU
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

    pub fn dma_transfer(&self, oam_bytes: Vec<u8>) {
        for i in 0..0xa0 {
            self.oam.borrow_mut()[i] = oam_bytes[i];
        }
    }

    pub fn dbg_tile(&self, tile: u8) -> Vec<u8> {
        let vram = self.vram.borrow();
        let tile_addr = self.bg_tile_addr(tile) as usize;
        let tile = Tile::new(&vram[tile_addr..tile_addr + 16]);
        let mut tile_img = Vec::new();
        for y in 0..8 {
            for x in 0..8 {
                tile_img.push(tile.color_at(x, y));
            }
        }
        tile_img
    }

    fn render_scanline(&self, screen: &mut Screen) {
        self.render_tiles(screen);
        if self.bg_on() {
            // self.render_bg(screen);
        }

        if self.window_on() {
            // self.render_window(screen);
        }

        if self.obj_on() {
            // TODO: fix multiply overflow
            // self.render_sprites(screen);
        }
    }

    fn render_tiles(&self, screen: &mut Screen) {
        let mut tile_data: usize = 0;
        let mut unsigned = true;
        let mut bg_memory: usize = 0;
        let lcdc = self.lcdc.get();
        let ly = self.ly();
        let scy = self.scy();
        let scx = self.scx();
        let wy = self.wy();
        let wx = self.wx();
        let mut using_window = false;
        let vram = self.vram.borrow();

        if self.window_on() && wy <= ly {
            using_window = true;
        }

        if test_bit(lcdc, 4) {
            tile_data = 0;
        } else {
            tile_data = 0x1000;
            unsigned = false;
        }

        if using_window {
            if test_bit(lcdc, 6) {
                bg_memory = 0x1c00;
            } else {
                bg_memory = 0x1800
            }
        } else {
            if test_bit(lcdc, 3) {
                bg_memory = 0x1c00;
            } else {
                bg_memory = 0x1800;
            }
        }

        let mut y_pos = 0;

        if using_window {
            y_pos = ly - wy;
        } else {
            y_pos = scy.wrapping_add(ly);
        }

        let tile_row: usize = (y_pos as usize >> 3) << 5;
        let mut tile_col: usize;

        for pixel in 0..160 {
            let mut x_pos = scx.wrapping_add(pixel);

            if using_window {
                if pixel >= wx {
                    x_pos = pixel - wx;
                }
            }

            tile_col = x_pos as usize >> 3;

            let tile_addr = bg_memory + tile_row + tile_col;

            let mut tile_num = vram[tile_addr] as usize;

            let tile_location = if unsigned {
                tile_data + (tile_num << 4)
            } else {
                if (tile_num & 0x80) == 0x80 {
                    tile_data - ((!tile_num + 1) << 4)
                } else {
                    tile_data + (tile_num << 4)
                }
            };

            let y = y_pos & 7;
            let x = x_pos & 7;
            let tile = Tile::new(&vram[tile_location..tile_location + 16]);
            let t_color = tile.color_at(x, y);
            let p_color = self.bgp_color(t_color);

            if ly > 143 || pixel > 159 {
                continue;
            }
            screen[ly as usize * 144 + pixel as usize] = p_color;
        }
    }

    fn render_bg(&self, screen: &mut Screen) {
        let tile_y = (self.bg_line() as usize) >> 3;
        let mut tile_x = (self.scx() as usize) >> 3;
        let mut tilemap_addr = self.bgmap_offset().wrapping_add((tile_y << 5) + tile_x);
        let vram = self.vram.borrow();
        let mut tile_num = vram[tilemap_addr];
        let mut tile_addr = self.bg_tile_addr(tile_num) as usize;
        let mut tile = Tile::new(&vram[tile_addr..tile_addr + 16]);

        let mut pixel_x = self.scx() & 7;
        let pixel_y = self.bg_line() & 7;

        if tile_num != 6 {
            println!("break");
        }

        for screen_x in 0..160 {
            // TODO: if this is too slow, we can calculate when vram is updated
            // and keep tile data in separate data structure
            let t_color = tile.color_at(pixel_x, pixel_y);
            let p_color = self.bgp_color(t_color);
            // push to screen array
            screen[self.ly() as usize * 144 + screen_x] = p_color;
            pixel_x += 1;
            // move to next tile
            if pixel_x > 7 {
                pixel_x = 0;
                tile_x = (tile_x + 1) & 31;
                tilemap_addr = self.bgmap_offset().wrapping_add((tile_y << 5) + tile_x);
                tile_num = vram[tilemap_addr];
                tile_addr = self.bg_tile_addr(tile_num);
                tile = Tile::new(&vram[tile_addr..tile_addr + 16]);
            }
        }
    }

    fn render_window(&self, screen: &mut Screen) {
        let line = self.ly();
        let wy = self.wy();
        let wx = self.wx();
        if line < wy || wy > 143 || wx > 159 {
            return;
        }

        let tilemap_offset = (self.winmap_offset() + (self.win_line() as usize)) >> 3;
        let vram = self.vram.borrow();

        for x in wx..160 {
            let line_offset = ((x - wx) >> 3) as usize;
            let tile_num = vram[tilemap_offset + line_offset];
            let tile_addr = self.bg_tile_addr(tile_num);
            let tile = Tile::new(&vram[tile_addr..tile_addr + 16]);

            let t_color = tile.color_at(x - wx, line);
            let p_color = self.bgp_color(t_color);
            screen[(line * 144 + x) as usize] = p_color;
        }
    }

    fn render_sprites(&self, screen: &mut Screen) {
        let mut sprite_count = 0;
        for i in 0..40 {
            let oam = self.oam.borrow();
            let sprite_addr = i * 4;
            let sprite = Sprite::new(&oam[sprite_addr..sprite_addr + 4], self.obj_size() == 16);
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
                    let mut tile =
                        Tile::new(&vram[tile_addr..tile_addr + (self.obj_size() as usize) * 2]);
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

    fn obj_size(&self) -> u8 {
        if (self.lcdc.get() & 0x4) == 0x4 {
            16
        } else {
            8
        }
    }

    fn bg_on(&self) -> bool {
        (self.lcdc.get() & 0x1) == 0x1
    }

    fn lyc_int_on(&self) -> bool {
        (self.stat.get() & 0x40) == 0x40
    }

    fn oam_int_on(&self) -> bool {
        (self.stat.get() & 0x20) == 0x20
    }

    fn vblank_int_on(&self) -> bool {
        (self.stat.get() & 0x10) == 0x10
    }

    fn hblank_int_on(&self) -> bool {
        (self.stat.get() & 0x8) == 0x8
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

    fn winmap_offset(&self) -> usize {
        if self.lcdc.get() & 0x40 == 0x40 {
            0x1c00
        } else {
            0x1800
        }
    }

    fn win_line(&self) -> u8 {
        (self.ly() - self.wy())
    }

    fn mode(&self) -> Mode {
        Mode::from_u8(self.stat.get() & 0x3).unwrap()
    }

    fn set_mode(&self, mode: Mode) {
        let stat = mode | (self.stat.get() & 0xfc);
        self.stat.replace(stat);
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

    fn scy(&self) -> u8 {
        self.scy.get()
    }

    fn scx(&self) -> u8 {
        self.scx.get()
    }

    fn wy(&self) -> u8 {
        self.wy.get()
    }

    fn wx(&self) -> u8 {
        self.wx.get().wrapping_sub(7)
    }

    // Also used for window tile address
    fn bg_tile_addr(&self, tile: u8) -> usize {
        let t = tile as usize;
        if (self.lcdc.get() & 0x10) == 0x10 {
            t << 4
        } else {
            if (t & 0x80) == 0x80 {
                0x1000 - ((!t + 1) << 4)
            } else {
                0x1000 + (t << 4)
            }
        }
    }

    fn sprite_tile_addr(&self, tile: u8) -> usize {
        (tile * 16) as usize
    }

    fn bgp_color(&self, color: u8) -> u8 {
        (self.bgp.get() >> ((color & 3) << 1)) & 3
    }

    fn obp0_color(&self, color: u8) -> u8 {
        (self.obp0.get() >> ((color & 3) << 1)) & 3
    }

    fn obp1_color(&self, color: u8) -> u8 {
        (self.obp1.get() >> ((color & 3) << 1)) & 3
    }
}

// TODO: move to util module
fn test_bit(n: u8, b: u8) -> bool {
    (n & (1 << b)) >> b == 1
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
    fn test_set_mode() {
        let gpu = GPU::new();
        assert_eq!(gpu.mode(), Mode::Hblank);
        gpu.set_mode(Mode::OAM);
        assert_eq!(gpu.mode(), Mode::OAM);
        gpu.set_mode(Mode::Vblank);
        assert_eq!(gpu.mode(), Mode::Vblank);
        gpu.set_mode(Mode::VRAM);
        assert_eq!(gpu.mode(), Mode::VRAM);
        gpu.set_mode(Mode::Hblank);
        assert_eq!(gpu.mode(), Mode::Hblank);
    }

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
    fn test_render_bg() {
        let gpu = GPU::new();
        gpu.lcdc.set(0x91);
        gpu.bgp.set(0xe4);

        for i in 0..20 {
            let mut vram = gpu.vram.borrow_mut();

            vram[i * 20] = 0xff;
        }
        for i in 0..20 {
            let mut vram = gpu.vram.borrow_mut();
            vram[i * 20 + 1] = 0xff;
        }

        let mut screen: [u8; 160] = [0; 160];
        gpu.render_bg(&mut screen);
        for i in 0..160 {
            assert_eq!(screen[i], 3);
        }
    }

    #[test]
    fn test_palettes() {
        let gpu = GPU::new();
        gpu.write_byte(0xff47, 0b11_10_01_00);
        assert_eq!(gpu.bgp.get(), 0b11_10_01_00);

        assert_eq!(gpu.bgp_color(0b00), 0);
        assert_eq!(gpu.bgp_color(0b01), 1);
        assert_eq!(gpu.bgp_color(0b10), 2);
        assert_eq!(gpu.bgp_color(0b11), 3);

        gpu.write_byte(0xff47, 0b01_11_00_10);
        assert_eq!(gpu.bgp_color(0b00), 2);
        assert_eq!(gpu.bgp_color(0b01), 0);
        assert_eq!(gpu.bgp_color(0b10), 3);
        assert_eq!(gpu.bgp_color(0b11), 1);
    }
}
