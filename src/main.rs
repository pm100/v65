use std::{fs::File, io::Read};
#[macro_export]
macro_rules! trace {
    ($fmt:literal, $($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            if cfg!(test){
                println!($fmt, $($arg),*);
            } else {
                log::warn!($fmt, $($arg),*);
            }
        }
    };
    ($msg:expr) => {
        #[cfg(debug_assertions)]
        {
            if cfg!(test){
                println!($msg);
            } else {
                log::warn!($msg);
            }
        }
    };
}
use cpu::Cpu;

mod cpu;
mod instructions;

use simplelog::*;
//use std::fs::File;
use std::io;

fn main() -> io::Result<()> {
    let _ = CombinedLogger::init(vec![WriteLogger::new(
        LevelFilter::Info,
        Config::default(),
        File::create("my_rust_binary.log").unwrap(),
    )]);
    let mut cpu = Cpu::new();
    trace!("Loading binary");
    let bin = load_binary("6502_functional_test.bin");
    for i in 0..bin.len() {
        cpu.write(i as u16, bin[i]);
    }

    cpu.pc = 0x0400;

    cpu.run();
    Ok(())
}

fn load_binary(file: &str) -> Vec<u8> {
    let mut file = File::open(file).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    buffer
}
