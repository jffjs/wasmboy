use cartridge::Cartridge;
use cpu::CPU;
use gpu::GPU;
use mmu::MMU;
use num::ToPrimitive;
use std::ops::{BitAnd, BitOr, Not};
use std::rc::Rc;
use timer::Timer;
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
        let mmu = MMU::new(cart, timer.clone(), gpu.clone());
        let mut cpu = CPU::new();
        cpu.post_bios();
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
            // Execute next instruction
            if !self.cpu.halt() {
                match self.cpu.exec(&mut self.mmu) {
                    Ok(_) => (),
                    Err(msg) => panic!(msg),
                }
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
    }

    #[wasm_bindgen]
    pub fn run(&mut self) {
        self.cpu.stop = false;
    }

    #[wasm_bindgen]
    pub fn pause(&mut self) {
        self.cpu.stop = true;
    }

    #[wasm_bindgen]
    pub fn dbg_step(&mut self, screen: &mut [u8]) {
        if !self.cpu.halt() {
            self.cpu.exec(&mut self.mmu).expect("CPU exec error.");
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
    pub fn dbg_cpu_snapshot(&mut self) -> JsValue {
        JsValue::from_serde(&self.cpu).unwrap()
    }

    fn check_interrupts(&mut self) {
        if self.cpu.ime && self.mmu.ie() != 0 && self.mmu.iflag() != 0 {
            self.cpu.halt = true;
            self.cpu.ime = false;
            let int_fired = self.mmu.ie() & self.mmu.iflag();

            if (IntFlag::Vblank & int_fired) == 0x1 {
                self.mmu.reset_iflag(IntFlag::Vblank);
                self.cpu.handle_interrupt(IntFlag::Vblank, &mut self.mmu);
            } else if (IntFlag::LCDC & int_fired) == 0x2 {
                self.mmu.reset_iflag(IntFlag::LCDC);
                self.cpu.handle_interrupt(IntFlag::LCDC, &mut self.mmu);
            } else if (IntFlag::TimerOverflow & int_fired) == 0x4 {
                self.mmu.reset_iflag(IntFlag::TimerOverflow);
                self.cpu
                    .handle_interrupt(IntFlag::TimerOverflow, &mut self.mmu);
            } else if (IntFlag::SerialIO & int_fired) == 0x8 {
                self.mmu.reset_iflag(IntFlag::SerialIO);
                self.cpu.handle_interrupt(IntFlag::SerialIO, &mut self.mmu);
            } else if (IntFlag::JoyPad & int_fired) == 0x10 {
                self.mmu.reset_iflag(IntFlag::JoyPad);
                self.cpu.handle_interrupt(IntFlag::JoyPad, &mut self.mmu);
            } else {
                self.cpu.ime = true;
            }
        }
    }
}

#[derive(Debug, PartialEq, ToPrimitive)]
pub enum IntFlag {
    Vblank = 0x1,
    LCDC = 0x2,
    TimerOverflow = 0x4,
    SerialIO = 0x8,
    JoyPad = 0x10,
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
