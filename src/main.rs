use std::{
    collections::{binary_heap, LinkedList},
    fs::File,
    io::Read,
};
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
struct MyLog {
    buffer: LinkedList<String>,
    max_size: usize,
    current: String,
}
impl MyLog {
    fn new(max_size: usize) -> Self {
        Self {
            buffer: LinkedList::new(),
            max_size,
            current: String::new(),
        }
    }
}
impl std::io::Write for MyLog {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = std::str::from_utf8(buf).unwrap();
        if s == "\n" {
            self.buffer.push_back(self.current.clone());
            self.current.clear();
            if self.buffer.len() > self.max_size {
                self.buffer.pop_front();
            }
        } else {
            self.current.push_str(s);
        }
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        for s in &self.buffer {
            println!("{}", s);
        }
        Ok(())
    }
}
use simplelog::*;
//use std::fs::File;
use std::io;

fn main() -> io::Result<()> {
    let _ll = MyLog::new(100);
    let _ = CombinedLogger::init(vec![WriteLogger::new(
        LevelFilter::Info,
        Config::default(),
        File::create("my_rust_binary.log").unwrap(),
        //ll,
    )]);
    let mut cpu = Cpu::new();
    trace!("Loading binary");
    //   let bin = load_binary("6502_functional_test.bin");
    let bin = load_binary("c:/work/msbasic65/tmp/kim1.bin");
    for i in 0..bin.len() {
        cpu.write((i + 0x2000) as u16, bin[i]);
    }
    log::logger().flush();
    cpu.pc = 0x4062;
    //cpu.pc = 0x2000;

    cpu.run();
    Ok(())
}
#[test]
fn read() {
    let mut line = String::new();
    println!("yo");
    let s = std::io::stdin().read_line(&mut line).unwrap();
    print!("{}", s);
}
fn load_binary(file: &str) -> Vec<u8> {
    let mut file = File::open(file).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    buffer
}
#[test]
fn test_cast() {
    let a_u8 = 0x81u8;
    let m_u8 = 0x01u8;
    let c = true;

    let aexp = a_u8 as i8 as i16;
    let mexp = m_u8 as i8 as i16;
    let mexp_inv = !mexp;
    let cexp = c as i16;

    let res = aexp + mexp_inv + cexp;
    println!("SUB: {} {} {} ", aexp, mexp_inv, cexp);
    println!(
        "SUB: {:0>16b} {:0>16b} {:0>16b} {:0>16b}",
        aexp, mexp, mexp_inv, cexp
    );
    println!("SUB: {} {:0>16b}", res, res);
    println!("SUB: {} {:0>8b}", res as u8, res as u8);

    let res = aexp + mexp + cexp;
    println!("ADD: {} {} {} ", aexp, mexp, cexp);
    println!(
        "Add: {:0>16b} {:0>16b} {:0>16b} {:0>16b}",
        aexp, mexp, mexp_inv, cexp
    );
    println!("ADD: {} {:0>16b}", res, res);
    println!("ADD: {} {:0>8b}", res as u8, res as u8);
    println!("ADD: {:?}", adc(a_u8, m_u8, c));
}
#[test]
fn test_adc() {
    assert_eq!(adc(0x01, 0x01, false), (0x02, false, false));
    assert_eq!(adc(0x01, 0xff, false), (0x00, true, false));
    assert_eq!(adc(0x7f, 0x01, false), (128, false, true));
    assert_eq!(adc(0x80, 0xff, false), (0x7f, true, true));
}
#[test]
fn test_adc_dec() {
    // assert_eq!(hex_to_bcd(150), 0x18);
    assert_eq!(adc_dec(0x58, 0x46, true), (0x05, true, false));
    assert_eq!(adc_dec(0x12, 0x34, false), (0x46, false, false));
    assert_eq!(adc_dec(0x81, 0x92, false), (0x73, true, false));
}
#[test]
fn test_sbc() {
    assert_eq!(sbc(0x00, 0x01, true), (0xff, false, false));
    assert_eq!(sbc(0x00, 0x00, true), (0x00, true, false));
    assert_eq!(sbc(0x7f, 0xff, true), (128, false, true));
}
#[test]
fn test_sbc_dec() {
    assert_eq!(sbc_dec(0x46, 0x12, true), (0x34, true, false));
    assert_eq!(sbc_dec(0x12, 0x21, true), (0x91, false, false));
    assert_eq!(sbc_dec(0x21, 0x34, true), (0x87, false, false));
}
fn bcd_to_hex(bcd: u8) -> u8 {
    let decimal = (bcd >> 4) * 10 + (bcd & 0x0F);
    decimal
}
fn hex_to_bcd(hex: u8) -> (u8, bool) {
    if hex > 99 {
        ((hex - 100) / 10 << 4 | (hex - 100) % 10, true)
    } else {
        (hex / 10 << 4 | hex % 10, false)
    }
}

pub fn adc_dec(a: u8, m: u8, c: bool) -> (u8, bool, bool) {
    let ba = bcd_to_hex(a);
    let bm = bcd_to_hex(m);
    let (res, _, _) = adc(ba, bm, c);
    let (bcd, carry) = hex_to_bcd(res);
    (bcd, carry, false)
}
pub fn adc(a: u8, m: u8, c: bool) -> (u8, bool, bool) {
    let (am, c1) = a.overflowing_add(m);
    let (resu8, c2) = am.overflowing_add(c as u8);
    let c = c1 || c2;
    //https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    let v = ((a ^ resu8) & (m ^ resu8) & 0x80) != 0;
    (resu8, c, v)
}

pub fn sbc_dec(a: u8, m: u8, c: bool) -> (u8, bool, bool) {
    let ba = bcd_to_hex(a);
    let bm = bcd_to_hex(m);

    let mut res = ba as i16 - bm as i16 - if c { 0 } else { 1 };
    let carry = if res < 0 {
        res += 100;
        false
    } else {
        true
    };
    //  let (bcd, _) = hex_to_bcd(res);
    let out = (((res / 10) << 4 | (res % 10)) as u8, carry, false);
    //println!("SBC: {} {:02X}", carry, out.0);
    out
}
pub fn sbc(a: u8, m: u8, c: bool) -> (u8, bool, bool) {
    adc(a, !m, c)
}
