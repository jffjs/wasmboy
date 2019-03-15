use cartridge::*;

pub struct MMU {
    cart: Option<Cartridge>,
    e_ram: [u8; 0x2000],
    w_ram: [u8; 0x2000],
    z_ram: [u8; 0x80],
}

impl MMU {
    pub fn new() -> MMU {
        MMU {
            cart: None,
            e_ram: [0; 0x2000],
            w_ram: [0; 0x2000],
            z_ram: [0; 0x80],
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match (addr & 0xf000) >> 12 {
            // BIOS or ROM bank 0 (BIOS will be ignored)
            0x0 => self.rom()[addr as usize],
            // ROM bank 0
            0x1 | 0x2 | 0x3 => self.rom()[addr as usize],
            // ROM bank 1
            0x4 | 0x5 | 0x6 | 0x7 => {
                let offset = self.rom_bank() as u16 * 0x4000;
                self.rom()[(addr + offset) as usize]
            }
            // Graphics: VRAM
            0x8 | 0x9 => 0, // TODO: read from VRAM
            // External RAM
            0xa | 0xb => self.e_ram[(addr & 0x1fff) as usize],
            // Working RAM
            0xc | 0xd => self.w_ram[(addr & 0x1fff) as usize],
            // Working RAM shadow
            0xe => self.w_ram[(addr & 0x1fff) as usize],
            // Working RAM shadow, I/O, Zero-page RAM
            0xf => match (addr & 0xf00) >> 8 {
                // Working RAM shadow
                0x0 | 0x1 | 0x2 | 0x3 | 0x4 | 0x5 | 0x6 | 0x7 | 0x8 | 0x9 | 0xa | 0xb | 0xc
                | 0xd => self.w_ram[(addr & 0x1fff) as usize],
                // Graphics: object attribute memory
                // OAM is 160 bytes, remaining bytes read as 0
                0xe => {
                    if addr < 0xfea0 {
                        0
                    // TODO: read from OAM
                    } else {
                        0
                    }
                }
                0xf => {
                    if addr >= 0xff80 {
                        self.z_ram[(addr & 0x7f) as usize]
                    } else {
                        0
                        // TODO: I/O control handling
                    }
                }
                _ => 0,
            },
            _ => 0,
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        self.read_byte(addr) as u16 + (self.read_byte(addr + 1) as u16) << 8
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {}

    pub fn write_word(&mut self, addr: u16, value: u16) {}

    pub fn load_cartridge(&mut self, rom: &[u8]) {
        self.cart = Some(Cartridge::new(rom));
    }

    fn rom(&self) -> &[u8] {
        if let Some(ref cart) = self.cart {
            cart.rom.as_slice()
        } else {
            panic!("Cartridge ROM has not been loaded!");
        }
    }

    fn rom_bank(&self) -> u8 {
        if let Some(ref cart) = self.cart {
            cart.rom_bank
        } else {
            panic!("Cartridge ROM has not been loaded!");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rom() {
        let mut cart_data = [0; 0x148];
        cart_data[0] = 1;
        cart_data[5] = 123;
        let mut mmu = MMU::new();
        mmu.load_cartridge(&cart_data);
        assert_eq!(mmu.rom()[0], 1);
        assert_eq!(mmu.rom()[5], 123);
    }
}
