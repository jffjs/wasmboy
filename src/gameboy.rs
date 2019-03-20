use cartridge::Cartridge;
use cpu::CPU;
use mmu::MMU;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Gameboy {
    debug: bool,
    cpu: CPU,
    mmu: MMU,
}

#[wasm_bindgen]
impl Gameboy {
    #[wasm_bindgen(constructor)]
    pub fn new(rom: &[u8], debug: bool) -> Gameboy {
        let cart = Cartridge::new(rom);
        let mmu = MMU::new(cart);
        let mut cpu = CPU::new();
        cpu.post_bios();
        Gameboy { cpu, mmu, debug }
    }

    #[wasm_bindgen]
    pub fn dbg_step(&mut self) {
        self.cpu.exec(&mut self.mmu).expect("CPU exec error.");
    }

    #[wasm_bindgen]
    pub fn dbg_cpu_snapshot(&mut self) -> JsValue {
        JsValue::from_serde(&self.cpu).unwrap()
    }
}
