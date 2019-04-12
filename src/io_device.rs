use crate::cpu::CPU;
use crate::utils::{hi_byte, lo_byte};
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
        self.write_byte(addr, lo_byte(value));
        self.write_byte(addr.wrapping_add(1), hi_byte(value));
    }

    fn rsv(&mut self, cpu: CPU) {}
    fn rrs(&self) -> CPU {
        CPU::new()
    }
}
