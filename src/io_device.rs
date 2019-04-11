#[cfg(test)]
use mockers_derive::mocked;

#[cfg_attr(test, mocked)]
pub trait IoDevice {
    fn read_byte(&self, addr: u16) -> u8;

    fn write_byte(&mut self, addr: u16, value: u8);

    fn read_word(&self, addr: u16) -> u16 {
        self.read_byte(addr) as u16 + ((self.read_byte(addr.wrapping_add(1)) as u16) << 8)
    }

    fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xff) as u8);
        self.write_byte(addr.wrapping_add(1), (value >> 8) as u8);
    }
}
