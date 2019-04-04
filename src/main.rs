extern crate wasmboy;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use wasmboy::emulator::Emulator;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Please specify ROM to load.");
        Ok(())
    } else {
        let filename = &args[1];
        let mut f = File::open(filename)?;
        let mut rom = Vec::new();
        f.read_to_end(&mut rom)?;

        let mut gb = Emulator::new(&rom);
        let mut screen = [0; 160 * 144];

        loop {
            gb.frame(&mut screen);
        }
    }
}
