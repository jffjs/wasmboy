use crate::{
    cartridge::Cartridge, cpu::CPU, gpu::GPU, io_device::IoDevice, mmu::MMU, timer::Timer,
    utils::test_bit,
};
use num::{FromPrimitive, ToPrimitive};
use std::ops::{BitAnd, BitOr, Not};
use std::rc::Rc;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Emulator {
    cpu: CPU,
    gpu: Rc<GPU>,
    mmu: MMU,
    timer: Rc<Timer>,
}

#[wasm_bindgen]
impl Emulator {
    #[wasm_bindgen(constructor)]
    pub fn new(rom: &[u8]) -> Emulator {
        let cart = Cartridge::new(rom);
        let timer = Rc::new(Timer::new());
        let gpu = Rc::new(GPU::new());
        let mut mmu = MMU::new(cart, timer.clone(), gpu.clone());
        let mut cpu = CPU::new();
        cpu.post_bios();
        mmu.post_bios();
        Emulator {
            cpu,
            gpu: gpu.clone(),
            mmu,
            timer: timer.clone(),
        }
    }

    #[wasm_bindgen]
    pub fn frame(&mut self, screen: &mut [u8]) {
        // (144 scanlines + 10-line vblank) * 114 M-cycles
        const M_CYCLES: u32 = (144 + 10) * 114;
        let fclock = self.cpu.clock_m() + M_CYCLES;
        while self.cpu.clock_m() < fclock {
            self.cpu.exec(&mut self.mmu);
            let mut interrupts: Vec<IntFlag> = Vec::new();
            if let Some(timer_int) = self.timer.inc(self.cpu.m()) {
                interrupts.push(timer_int);
            }
            let mut gpu_interrupts = self.gpu.execute(self.cpu.m(), screen);
            interrupts.append(&mut gpu_interrupts);
            self.check_interrupts();
        }
    }

    #[wasm_bindgen]
    pub fn run(&mut self) {
        self.cpu.stop = false;
    }

    #[wasm_bindgen]
    pub fn pause(&mut self) {
        self.cpu.stop = true;
    }

    fn check_interrupts(&mut self) {
        if self.cpu.ime {
            let iflag = self.mmu.iflag();
            let enabled = self.mmu.ie();

            if iflag > 0 {
                for int in IntFlag::priority().into_iter() {
                    let int_requested = enabled & iflag;
                    if let Some(int_val) = int.to_u8() {
                        if int & int_requested == int_val {
                            self.cpu.handle_interrupt(int, &mut self.mmu);
                        }
                    }
                }
            }
        }
    }

    #[wasm_bindgen]
    pub fn dbg_step(&mut self, screen: &mut [u8]) {
        if !self.cpu.halted() {
            self.cpu.exec(&mut self.mmu);
        }

        // Run interrupt routine
        self.check_interrupts();

        let interrupts = self.gpu.execute(self.cpu.m(), screen);
        for int in interrupts {
            if int == IntFlag::Vblank {
                // TODO: add screen render hook here
            }
            self.mmu.set_iflag(int);
        }

        if let Some(timer_int) = self.timer.inc(self.cpu.m()) {
            self.mmu.set_iflag(timer_int);
        }
    }

    #[wasm_bindgen]
    pub fn dbg_cpu_snapshot(&self) -> JsValue {
        JsValue::from_serde(&self.cpu).unwrap()
    }

    #[wasm_bindgen]
    pub fn dbg_mem_snapshot(&self, addr: usize, length: usize) -> Box<[u8]> {
        let mut snapshot = Vec::new();
        for i in 0..length {
            let byte = self.mmu.read_byte((addr + i) as u16);
            snapshot.push(byte);
        }
        snapshot.into_boxed_slice()
    }

    pub fn dbg_tile_data(&self, tile_num: u8) -> Box<[u8]> {
        self.gpu.dbg_tile(tile_num).into_boxed_slice()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, ToPrimitive)]
pub enum IntFlag {
    Vblank = 0x1,
    LCDC = 0x2,
    TimerOverflow = 0x4,
    SerialIO = 0x8,
    JoyPad = 0x10,
}

impl IntFlag {
    fn priority() -> Vec<IntFlag> {
        vec![
            IntFlag::Vblank,
            IntFlag::LCDC,
            IntFlag::TimerOverflow,
            IntFlag::SerialIO,
            IntFlag::JoyPad,
        ]
    }
}

impl BitAnd<u8> for IntFlag {
    type Output = u8;

    fn bitand(self, other: u8) -> u8 {
        other & self.to_u8().unwrap()
    }
}

impl BitOr<u8> for IntFlag {
    type Output = u8;

    fn bitor(self, other: u8) -> u8 {
        other | self.to_u8().unwrap()
    }
}

impl Not for IntFlag {
    type Output = u8;

    fn not(self) -> u8 {
        !(self.to_u8().unwrap())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_int_flag() {
        assert_eq!(!IntFlag::Vblank, 0xfe);
        assert_eq!(!IntFlag::LCDC, 0xfd);
        assert_eq!(!IntFlag::TimerOverflow, 0xfb);
        assert_eq!(!IntFlag::SerialIO, 0xf7);
        assert_eq!(!IntFlag::JoyPad, 0xef);

        assert_eq!(IntFlag::Vblank & 0xff, 0x1);
        assert_eq!(IntFlag::LCDC & 0xff, 0x2);
        assert_eq!(IntFlag::TimerOverflow & 0xff, 0x4);
        assert_eq!(IntFlag::SerialIO & 0xff, 0x8);
        assert_eq!(IntFlag::JoyPad & 0xff, 0x10);
    }
}
