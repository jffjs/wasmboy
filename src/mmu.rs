pub struct MMU {}

impl MMU {
    pub fn read_byte(&self, addr: u16) -> u8 {
        0
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        0
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {}

    pub fn write_word(&mut self, addr: u16, value: u16) {}
}
