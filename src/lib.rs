extern crate cfg_if;
extern crate num;
extern crate wasm_bindgen;
#[macro_use]
extern crate num_derive;

mod cpu;
mod mmu;
mod opcode;
mod utils;

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

#[test]
fn it_works() {
    println!("{}", ((0xff as i8) as u8) as u16);
}
