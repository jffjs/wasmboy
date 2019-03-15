use cartridge::*;

pub struct MMU {
    cart: Cartridge,
    e_ram: [u8; 0x8000],
    w_ram: [u8; 0x2000],
    z_ram: [u8; 0x80],
}

impl MMU {
    pub fn new(cart: Cartridge) -> MMU {
        MMU {
            cart,
            e_ram: [0; 0x8000],
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
                let offset = self.rom_bank() * 0x4000;
                self.rom()[((addr & 0x3fff) + offset) as usize]
            }
            // Graphics: VRAM
            0x8 | 0x9 => 0, // TODO: read from VRAM
            // External RAM
            0xa | 0xb => {
                let offset = self.ram_bank() * 0x2000;
                self.e_ram[((addr & 0x1fff) + offset) as usize]
            }
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
        self.read_byte(addr) as u16 + ((self.read_byte(addr + 1) as u16) << 8)
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match (addr & 0xf000) >> 12 {
            // ROM bank 0
            // MBC1: Turn external RAM on
            0x0 | 0x1 => {
                match self.cart.cart_type {
                    CartType::MBC1 => {
                        // set RAM switch
                        self.cart.ram_enabled = (value & 0xf) == 0xa;
                    }
                    _ => (),
                }
            }
            // MBC1: ROM bank switch
            0x2 | 0x3 => {
                match self.cart.cart_type {
                    CartType::MBC1 => {
                        self.cart.rom_bank &= 0x60; // TODO: jsGB does this... why?
                        let mut rom_bank = value & 0x1f;
                        if rom_bank == 0 {
                            rom_bank = 1;
                        }
                        self.cart.rom_bank |= rom_bank;
                    }
                    _ => (),
                }
            }
            // ROM bank 1
            // MBC1: RAM bank switch
            0x4 | 0x5 => match self.cart.cart_type {
                CartType::MBC1 => match self.cart.mode {
                    CartMode::RamBank => {
                        if self.cart.ram_enabled {
                            self.cart.ram_bank = value & 0x3;
                        }
                    }
                    CartMode::Default => {
                        self.cart.rom_bank &= 0x1f;
                        self.cart.rom_bank |= (value & 0x3) << 5;
                    }
                },
                _ => (),
            },
            // MBC1: mode switch
            0x6 | 0x7 => match self.cart.cart_type {
                CartType::MBC1 => {
                    if (value & 0x1) == 1 {
                        self.cart.set_ram_bank_mode();
                    } else {
                        self.cart.set_default_mode();
                    }
                }
                _ => (),
            },
            // VRAM
            0x8 | 0x9 => (), // TODO
            // External RAM
            0xa | 0xb => {
                if self.cart.ram_enabled {
                    let offset = match self.cart.mode {
                        CartMode::RamBank => self.ram_bank() * 0x2000,
                        CartMode::Default => 0,
                    };
                    self.e_ram[((addr & 0x1fff) + offset) as usize] = value;
                }
            }
            // Working RAM and echo
            0xc | 0xd | 0xe => {
                self.w_ram[(addr & 0x1fff) as usize] = value;
            }
            0xf => {
                match (addr & 0xf00) >> 8 {
                    // OAM
                    0xe => {}
                    // Zeropage RAM, I/O, interrupts
                    0xf => {
                        if addr == 0xffff {
                            // TODO
                        } else if addr > 0xff7f {
                            self.z_ram[(addr & 0x7f) as usize] = value;
                        } else {
                            // TODO
                        }
                    }
                    // Echo RAM
                    _ => {
                        self.w_ram[(addr & 0x1fff) as usize] = value;
                    }
                }
            }
            _ => (), // Unaddressable
        }
    }

    pub fn write_word(&mut self, addr: u16, value: u16) {}

    fn rom(&self) -> &[u8] {
        self.cart.rom.as_slice()
    }

    fn rom_bank(&self) -> u16 {
        self.cart.rom_bank as u16
    }

    fn ram_bank(&self) -> u16 {
        self.cart.ram_bank as u16
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn mbc1_cart() -> Cartridge {
        let mut cart_data = [0; 0x148];
        cart_data[0x147] = 0x1; // MBC1
        Cartridge::new(&cart_data)
    }

    #[test]
    fn test_rom() {
        let mut cart_data = [0; 0x148];
        cart_data[0] = 1;
        cart_data[5] = 123;
        let cart = Cartridge::new(&cart_data);
        let mmu = MMU::new(cart);
        assert_eq!(mmu.rom()[0], 1);
        assert_eq!(mmu.rom()[5], 123);
    }

    #[test]
    fn test_mode_switch() {
        let mut mmu = MMU::new(mbc1_cart());
        assert_eq!(mmu.cart.mode, CartMode::Default);
        mmu.write_byte(0x6000, 0x1);
        assert_eq!(mmu.cart.mode, CartMode::RamBank);
    }

    #[test]
    fn test_rom_bank_switch() {
        let mut mmu = MMU::new(mbc1_cart());
        assert_eq!(mmu.cart.rom_bank, 1);
        mmu.write_byte(0x2000, 0x0);
        assert_eq!(mmu.cart.rom_bank, 1);
        mmu.write_byte(0x2000, 0x02);
        assert_eq!(mmu.cart.rom_bank, 2);
        mmu.write_byte(0x2000, 0xa3);
        assert_eq!(mmu.cart.rom_bank, 3);
    }

    #[test]
    fn test_ram_bank_switch() {
        let mut mmu = MMU::new(mbc1_cart());
        mmu.write_byte(0x6000, 0x1); // set RAM bank mode

        // RAM not enabled
        mmu.write_byte(0x4000, 0x1);
        assert_eq!(mmu.cart.ram_enabled, false);
        assert_eq!(mmu.cart.ram_bank, 0);

        // Enable RAM and switch bank
        mmu.write_byte(0x0000, 0x0a);
        mmu.write_byte(0x4000, 0x01);
        assert_eq!(mmu.cart.ram_enabled, true);
        assert_eq!(mmu.cart.ram_bank, 1);

        mmu.write_byte(0x4000, 0xf3);
        assert_eq!(mmu.cart.ram_bank, 3);

        // Disable RAM
        mmu.write_byte(0x0000, 0x1);
        assert_eq!(mmu.cart.ram_enabled, false);
    }

    #[test]
    fn test_read() {
        let mut cart_data = [0; 0xffff];
        cart_data[0x0] = 0x11;
        cart_data[0x1] = 0x22;
        cart_data[0x3333] = 0x33;
        cart_data[0x3334] = 0x44;
        let cart = Cartridge::new(&cart_data);
        let mmu = MMU::new(cart);

        assert_eq!(mmu.read_byte(0x0), 0x11);
        assert_eq!(mmu.read_byte(0x1), 0x22);
        assert_eq!(mmu.read_word(0x0), 0x2211);
        assert_eq!(mmu.read_byte(0x3333), 0x33);
        assert_eq!(mmu.read_byte(0x3334), 0x44);
        assert_eq!(mmu.read_word(0x3333), 0x4433);
    }

    #[test]
    fn test_write() {
        let mut cart_data = [0; 0xffff];
        cart_data[0x147] = 0x1;
        let cart = Cartridge::new(&cart_data);
        let mut mmu = MMU::new(cart);

        // working RAM
        mmu.write_byte(0xc000, 0x34);
        mmu.write_byte(0xd111, 0x75);
        assert_eq!(mmu.w_ram[0x0000], 0x34);
        assert_eq!(mmu.read_byte(0xc000), 0x34);
        assert_eq!(mmu.w_ram[0x1111], 0x75);
        assert_eq!(mmu.read_byte(0xd111), 0x75);

        // MBC1 external ram
        mmu.write_byte(0x0000, 0x0a); // enable RAM
        mmu.write_byte(0x6000, 0x1); // set RAM bank mode

        mmu.write_byte(0xa000, 0xff);
        mmu.write_byte(0xa0cb, 0x12);
        assert_eq!(mmu.e_ram[0x0000], 0xff);
        assert_eq!(mmu.read_byte(0xa000), 0xff);
        assert_eq!(mmu.e_ram[0x00cb], 0x12);
        assert_eq!(mmu.read_byte(0xa0cb), 0x12);

        // switch RAM bank
        mmu.write_byte(0x4000, 0x02);

        mmu.write_byte(0xa000, 0xaa);
        mmu.write_byte(0xa0cb, 0xde);
        assert_eq!(mmu.e_ram[0x4000], 0xaa);
        assert_eq!(mmu.read_byte(0xa000), 0xaa);
        assert_eq!(mmu.e_ram[0x40cb], 0xde);
        assert_eq!(mmu.read_byte(0xa0cb), 0xde);

        // switch RAM bank back
        mmu.write_byte(0x4000, 0x0);
        assert_eq!(mmu.read_byte(0xa000), 0xff);
        assert_eq!(mmu.read_byte(0xa0cb), 0x12);
    }
}
