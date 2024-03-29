extern crate cfg_if;
extern crate num;
extern crate wasm_bindgen;
#[macro_use]
extern crate num_derive;
#[macro_use]
extern crate serde_derive;

mod cartridge;
mod cpu;
pub mod emulator;
mod gpu;
mod mmu;
mod opcode;
mod timer;
mod utils;
mod io_device;

use cfg_if::cfg_if;
use wasm_bindgen::prelude::*;

cfg_if! {
    // When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
    // allocator.
    if #[cfg(feature = "wee_alloc")] {
        extern crate wee_alloc;
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
    }
}

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, wasmboy!");
}
