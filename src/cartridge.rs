pub struct Cartridge {
    pub cart_type: CartType,
    pub mode: CartMode,
    pub rom: Vec<u8>,
    pub rom_bank: u8,
    pub ram_bank: u8,
    pub ram_enabled: bool,
}

impl Cartridge {
    pub fn new(data: &[u8]) -> Cartridge {
        let cart_type = match data[0x147] {
            0x1 | 0x2 | 0x3 => CartType::MBC1,
            0x5 | 0x6 => CartType::MBC2,
            0xf | 0x10 | 0x11 | 0x12 | 0x13 => CartType::MBC3,
            0x19 | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e => CartType::MBC5,
            _ => CartType::ROM,
        };

        Cartridge {
            cart_type,
            mode: CartMode::Default,
            rom: Vec::from(data),
            rom_bank: 1,
            ram_bank: 0,
            ram_enabled: false,
        }
    }

    pub fn set_default_mode(&mut self) {
        self.mode = CartMode::Default;
    }

    pub fn set_ram_bank_mode(&mut self) {
        self.mode = CartMode::RamBank;
    }
}

#[derive(PartialEq, Debug)]
pub enum CartType {
    ROM,
    MBC1,
    MBC2,
    MBC3,
    MBC5,
}

#[derive(PartialEq, Debug)]
pub enum CartMode {
    Default,
    RamBank,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_cart_type() {
        let mut cart_data = [0; 0x148];
        let mut cart = Cartridge::new(&cart_data);
        assert_eq!(cart.cart_type, CartType::ROM);

        cart_data[0x147] = 0x1;
        cart = Cartridge::new(&cart_data);
        assert_eq!(cart.cart_type, CartType::MBC1);

        cart_data[0x147] = 0x5;
        cart = Cartridge::new(&cart_data);
        assert_eq!(cart.cart_type, CartType::MBC2);
    }
}
