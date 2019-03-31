use cartridge::*;
use emulator::IntFlag;
use gpu::GPU;
use std::rc::Rc;
use timer::Timer;

// TODO: need 0x1fff of e_ram for MBC5, but that breaks wasm
// maybe break e_ram up into banks (16 banks of 0x2000)
pub struct MMU {
    cart: Cartridge,
    e_ram: [u8; 0x8000],
    w_ram: [u8; 0x2000],
    z_ram: [u8; 0x80],
    ie: u8,    // interrupt enable
    iflag: u8, // if - interrupt flags
    gpu: Rc<GPU>,
    timer: Rc<Timer>,
}

impl MMU {
    pub fn new(cart: Cartridge, timer: Rc<Timer>, gpu: Rc<GPU>) -> MMU {
        MMU {
            cart,
            e_ram: [0; 0x8000],
            w_ram: [0; 0x2000],
            z_ram: [0; 0x80],
            ie: 0,
            iflag: 0,
            gpu,
            timer,
        }
    }

    pub fn ie(&self) -> u8 {
        self.ie
    }

    pub fn iflag(&self) -> u8 {
        self.iflag
    }

    pub fn set_iflag(&mut self, iflag: IntFlag) {
        self.iflag = iflag | self.iflag;
    }

    pub fn reset_iflag(&mut self, iflag: IntFlag) {
        self.iflag &= !iflag;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match (addr & 0xf000) >> 12 {
            // BIOS or ROM bank 0 (BIOS will be ignored)
            0x0 => self.rom()[addr as usize],
            // ROM bank 0
            0x1 | 0x2 | 0x3 => self.rom()[addr as usize],
            // ROM bank 1
            0x4 | 0x5 | 0x6 | 0x7 => {
                if self.rom_bank() > 0 {
                    let offset = self.rom_bank() * 0x4000;
                    self.rom()[((addr & 0x3fff) + offset) as usize]
                } else {
                    self.rom()[(addr & 0x3fff) as usize]
                }
            }
            // Graphics: VRAM
            0x8 | 0x9 => self.gpu.read_byte(addr),
            // External RAM
            0xa | 0xb => match self.cart.cart_type {
                CartType::MBC1 => {
                    let offset = self.ram_bank() * 0x2000;
                    self.e_ram[((addr & 0x1fff) + offset) as usize]
                }
                CartType::MBC2 => self.e_ram[(addr & 0x1ff) as usize],
                _ => 0,
            },
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
                0xe => self.gpu.read_byte(addr),
                0xf => {
                    if addr == 0xffff {
                        self.ie
                    } else if addr >= 0xff80 {
                        self.z_ram[(addr & 0x7f) as usize]
                    } else {
                        match addr & 0xf0 {
                            0x00 => match addr & 0xf {
                                0x0 => 0,       // TODO: joypad
                                0x1 | 0x2 => 0, // TODO: Serial I/O
                                0x4 | 0x5 | 0x6 | 0x7 => self.timer.read_byte(addr),
                                0xf => self.iflag,
                                _ => 0,
                            },
                            0x10 | 0x20 | 0x30 => 0, // TODO: Sound registers
                            0x40 => self.gpu.read_byte(addr),
                            _ => 0,
                        }
                    }
                }
                _ => 0,
            },
            _ => 0,
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        self.read_byte(addr) as u16 + ((self.read_byte(addr.wrapping_add(1)) as u16) << 8)
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match (addr & 0xf000) >> 12 {
            // ROM bank 0
            // Turn external RAM on
            0x0 | 0x1 => {
                match self.cart.cart_type {
                    CartType::MBC1 | CartType::MBC3 | CartType::MBC5 => {
                        // set RAM switch
                        self.cart.ram_enabled = (value & 0xf) == 0xa;
                    }
                    CartType::MBC2 => {
                        if (addr & 0x0100) == 0 {
                            self.cart.ram_enabled = !self.cart.ram_enabled;
                        }
                    }
                    _ => (),
                }
            }
            // ROM bank switch
            0x2 | 0x3 => match self.cart.cart_type {
                CartType::MBC1 => {
                    self.cart.rom_bank &= 0x60;
                    let mut rom_bank = value & 0x1f;
                    rom_bank = match rom_bank {
                        0x0 | 0x20 | 0x40 | 0x60 => rom_bank + 1,
                        _ => rom_bank,
                    };
                    self.cart.rom_bank |= rom_bank as u16;
                }
                CartType::MBC2 => {
                    if (addr & 0x0100) == 0x0100 {
                        self.cart.rom_bank &= 0x0;
                        let mut rom_bank = value & 0xf;
                        if rom_bank == 0 {
                            rom_bank = 1;
                        }
                        self.cart.rom_bank |= rom_bank as u16;
                    }
                }
                CartType::MBC3 => {
                    self.cart.rom_bank &= 0x00;
                    let mut rom_bank = value & 0x7f;
                    if rom_bank == 0 {
                        rom_bank = 1;
                    }
                    self.cart.rom_bank |= rom_bank as u16;
                }
                CartType::MBC5 => {
                    if addr < 0x3000 {
                        self.cart.rom_bank |= value as u16;
                    } else {
                        let rom_bank = self.cart.rom_bank | (value as u16) << 8;
                        self.cart.rom_bank = rom_bank & 0x1ff;
                    }
                }
                _ => (),
            },
            // ROM bank 1
            // RAM bank switch
            0x4 | 0x5 => match self.cart.cart_type {
                CartType::MBC1 => match self.cart.mode {
                    CartMode::RamBank => {
                        if self.cart.ram_enabled {
                            self.cart.ram_bank = value & 0x3;
                        }
                    }
                    CartMode::Default => {
                        self.cart.rom_bank &= 0x1f;
                        self.cart.rom_bank |= ((value & 0x3) << 5) as u16;
                    }
                },
                CartType::MBC3 => {
                    if self.cart.ram_enabled {
                        self.cart.ram_bank = value & 0x7;
                    }
                }
                CartType::MBC5 => {
                    if self.cart.ram_enabled {
                        self.cart.ram_bank = value & 0xf;
                    }
                }
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
            0x8 | 0x9 => self.gpu.write_byte(addr, value),
            // External RAM
            0xa | 0xb => {
                if self.cart.ram_enabled {
                    match self.cart.cart_type {
                        CartType::MBC1 => {
                            let offset = match self.cart.mode {
                                CartMode::RamBank => self.ram_bank() * 0x2000,
                                CartMode::Default => 0,
                            };
                            self.e_ram[((addr & 0x1fff) + offset) as usize] = value;
                        }
                        CartType::MBC2 => {
                            self.e_ram[(addr & 0x1ff) as usize] = value & 0xf;
                        }
                        _ => (),
                    }
                }
            }
            // Working RAM and echo
            0xc | 0xd | 0xe => {
                self.w_ram[(addr & 0x1fff) as usize] = value;
            }
            0xf => {
                match (addr & 0xf00) >> 8 {
                    // OAM
                    0xe => self.gpu.write_byte(addr, value),
                    // Zeropage RAM, I/O, interrupts
                    0xf => {
                        if addr == 0xffff {
                            self.ie = value;
                        } else if addr >= 0xff80 {
                            self.z_ram[(addr & 0x7f) as usize] = value;
                        } else {
                            match addr & 0xf0 {
                                0x00 => match addr & 0xf {
                                    0x0 => (),       // TODO: joypad
                                    0x1 | 0x2 => (), // TODO: Serial I/O
                                    0x4 | 0x5 | 0x6 | 0x7 => self.timer.write_byte(addr, value),
                                    0xf => self.iflag = value,
                                    _ => (),
                                },
                                0x10 | 0x20 | 0x30 => (), // TODO: Sound registers
                                0x40 => {
                                    if addr == 0xff46 {
                                        let mut oam_bytes = Vec::new();
                                        let addr = (value as u16) << 8;
                                        for i in 0..0xa0 {
                                            oam_bytes.push(self.read_byte(addr + i as u16));
                                        }
                                        self.gpu.dma_transfer(oam_bytes);
                                    } else {
                                        self.gpu.write_byte(addr, value);
                                    }
                                }
                                _ => (),
                            }
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

    pub fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xff) as u8);
        self.write_byte(addr.wrapping_add(1), (value >> 8) as u8);
    }

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
    use gpu::GPU;
    use timer::Timer;

    fn timer() -> Rc<Timer> {
        Rc::new(Timer::new())
    }

    fn gpu() -> Rc<GPU> {
        Rc::new(GPU::new())
    }

    fn mbc1_cart() -> Cartridge {
        let mut cart_data = [0; 0x148];
        cart_data[0x147] = 0x1; // MBC1
        Cartridge::new(&cart_data)
    }

    fn mbc2_cart() -> Cartridge {
        let mut cart_data = [0; 0x148];
        cart_data[0x147] = 0x5; // MBC2
        Cartridge::new(&cart_data)
    }

    #[test]
    fn test_rom() {
        let mut cart_data = [0; 0x148];
        cart_data[0] = 1;
        cart_data[5] = 123;
        let cart = Cartridge::new(&cart_data);
        let mmu = MMU::new(cart, timer(), gpu());
        assert_eq!(mmu.rom()[0], 1);
        assert_eq!(mmu.rom()[5], 123);
    }

    #[test]
    fn test_mode_switch() {
        let mut mmu = MMU::new(mbc1_cart(), timer(), gpu());
        assert_eq!(mmu.cart.mode, CartMode::Default);
        mmu.write_byte(0x6000, 0x1);
        assert_eq!(mmu.cart.mode, CartMode::RamBank);
    }

    #[test]
    fn test_rom_bank_switch() {
        let mut mmu = MMU::new(mbc1_cart(), timer(), gpu());
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
        let mut mmu = MMU::new(mbc1_cart(), timer(), gpu());
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
        let mmu = MMU::new(cart, timer(), gpu());

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
        let mut mmu = MMU::new(cart, timer(), gpu());

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

    #[test]
    fn test_mbc2() {
        let mut mmu = MMU::new(mbc2_cart(), timer(), gpu());

        mmu.write_byte(0x00ff, 0x1);
        assert!(mmu.cart.ram_enabled);
        mmu.write_byte(0x02ff, 0x2);
        assert!(!mmu.cart.ram_enabled);
        mmu.write_byte(0x03ff, 0x2);
        assert!(!mmu.cart.ram_enabled);

        mmu.write_byte(0x00ff, 0x1);
        mmu.write_byte(0xa000, 0xff);
        assert_eq!(mmu.read_byte(0xa000), 0x0f);

        mmu.write_byte(0x2100, 0xf3);
        assert_eq!(mmu.cart.rom_bank, 3);

        mmu.write_byte(0x2000, 0x82);
        assert_eq!(mmu.cart.rom_bank, 3);
    }
}
