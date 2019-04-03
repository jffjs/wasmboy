extern crate wasmboy;
use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let mut f = File::open("test_roms/opus5.gb")?;
    let mut rom = Vec::new();
    f.read_to_end(&mut rom)?;

    let mut gb = wasmboy::emulator::Emulator::new(&rom);
    let mut screen = [0; 160 * 144];

    for _ in 0..1000 {
        gb.frame(&mut screen);
    }

    Ok(())
}
