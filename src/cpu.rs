use core::fmt;

use bitflags::bitflags;

pub(crate) struct Cpu {
    pub(crate) ram: Vec<u8>,
    pub(crate) pc: u16,
    pub(crate) sp: u8,
    pub(crate) ac: u8,
    pub(crate) ix: u8,
    pub(crate) iy: u8,
    pub(crate) status: Status,
    pub(crate) trace_text: String,
}

bitflags! {
    #[derive(Copy, Clone, Default)]
   pub(crate) struct Status:u8{
        const CARRY =       0b0000_0001;
        const ZERO =        0b0000_0010;
        const IDISABLE =    0b0000_0100;
        const DECIMAL =     0b0000_1000;
        const BREAK =       0b0001_0000;
        const UNUSED =      0b0010_0000;
        const OVF =         0b0100_0000;
        const NEGATIVE =    0b1000_0000;

    }
}

impl fmt::Debug for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = String::new();
        if self.contains(Status::NEGATIVE) {
            str.push('N');
        } else {
            str.push('n');
        };
        if self.contains(Status::OVF) {
            str.push('O');
        } else {
            str.push('o');
        };
        str.push('-');
        if self.contains(Status::BREAK) {
            str.push('B');
        } else {
            str.push('b');
        };

        if self.contains(Status::DECIMAL) {
            str.push('D');
        } else {
            str.push('d');
        };
        if self.contains(Status::IDISABLE) {
            str.push('I');
        } else {
            str.push('i');
        };
        if self.contains(Status::CARRY) {
            str.push('C');
        } else {
            str.push('c');
        };
        if self.contains(Status::ZERO) {
            str.push('Z');
        } else {
            str.push('z');
        };
        write!(f, "{}", str)
    }
}
pub(crate) const NMI: u16 = 0xfffa;
pub(crate) const RESET: u16 = 0xfffc;
pub(crate) const IRQ: u16 = 0xfffe;
pub(crate) const STACK: u16 = 0x0100;
pub(crate) const ZERO_PAGE: u16 = 0x0000;
pub(crate) const STACK_SIZE: u8 = 0xff;

impl Cpu {
    pub(crate) fn new() -> Cpu {
        Cpu {
            ram: vec![0; 0x10000],
            pc: 0,
            sp: 0,
            ac: 0,
            ix: 0,
            iy: 0,
            status: Status::empty() | Status::IDISABLE,
            trace_text: String::new(),
        }
    }
    pub(crate) fn reset(&mut self) {
        self.pc = 0xfffc;
        self.sp = 0xff;
        self.ac = 0;
        self.ix = 0;
        self.iy = 0;
        self.status = Status::empty() | Status::IDISABLE;
    }
    pub(crate) fn run(&mut self) {
        loop {
            let inst = self.ram[self.pc as usize];

            let save_pc = self.pc;
            self.pc += 1;

            match inst {
                0x00 => self.brk(inst),
                0x01 => self.ora(inst),
                0x05 => self.ora(inst),
                0x06 => self.asl(inst),
                0x08 => self.php(inst),
                0x09 => self.ora(inst),
                0x0a => self.asl(inst),
                0x0d => self.ora(inst),
                0x0e => self.asl(inst),

                0x10 => self.bpl(inst),
                0x11 => self.ora(inst),
                0x15 => self.ora(inst),
                0x16 => self.asl(inst),
                0x18 => self.clc(inst),
                0x19 => self.ora(inst),
                0x1d => self.ora(inst),
                0x1e => self.asl(inst),

                0x20 => self.jsr(inst),
                0x21 => self.and(inst),
                0x24 => self.bit(inst),
                0x25 => self.and(inst),
                0x26 => self.rol(inst),
                0x28 => self.plp(inst),
                0x29 => self.and(inst),
                0x2a => self.rol(inst),
                0x2c => self.bit(inst),
                0x2d => self.and(inst),
                0x2e => self.rol(inst),

                0x30 => self.bmi(inst),
                0x31 => self.and(inst),
                0x35 => self.and(inst),
                0x36 => self.rol(inst),
                0x38 => self.sec(inst),
                0x39 => self.and(inst),
                0x3d => self.and(inst),
                0x3e => self.rol(inst),

                0x40 => self.rti(inst),
                0x41 => self.eor(inst),
                0x45 => self.eor(inst),
                0x46 => self.lsr(inst),
                0x48 => self.pha(inst),
                0x49 => self.eor(inst),
                0x4a => self.lsr(inst),
                0x4c => self.jmp(inst),
                0x4d => self.eor(inst),
                0x4e => self.lsr(inst),

                0x50 => self.bvc(inst),

                0x51 => self.eor(inst),
                0x55 => self.eor(inst),
                0x56 => self.lsr(inst),
                0x58 => self.cli(inst),
                0x59 => self.eor(inst),
                0x5d => self.eor(inst),
                0x5e => self.lsr(inst),

                0x60 => self.rts(inst),
                0x61 => self.adc(inst),
                0x65 => self.adc(inst),
                0x66 => self.ror(inst),
                0x68 => self.pla(inst),
                0x69 => self.adc(inst),
                0x6a => self.ror(inst),
                0x6c => self.jmp_ind(inst),
                0x6d => self.adc(inst),
                0x6e => self.ror(inst),

                0x70 => self.bvs(inst),
                0x71 => self.adc(inst),
                0x75 => self.adc(inst),
                0x76 => self.ror(inst),
                0x78 => self.sei(inst),
                0x79 => self.adc(inst),
                0x7d => self.adc(inst),
                0x7e => self.ror(inst),

                0x81 => self.sta(inst),
                0x84 => self.sty(inst),
                0x85 => self.sta(inst),
                0x86 => self.stx(inst),
                0x88 => self.dey(inst),
                0x8a => self.txa(inst),
                0x8c => self.sty(inst),
                0x8d => self.sta(inst),
                0x8e => self.stx(inst),

                0x90 => self.bcc(inst),
                0x91 => self.sta(inst),
                0x94 => self.sty(inst),
                0x95 => self.sta(inst),
                0x96 => self.stx(inst),
                0x98 => self.tya(inst),
                0x99 => self.sta(inst),
                0x9a => self.txs(inst),
                0x9d => self.sta(inst),

                0xa0 => self.ldy(inst),
                0xa1 => self.lda(inst),
                0xa2 => self.ldx(inst),
                0xa4 => self.ldy(inst),
                0xa5 => self.lda(inst),
                0xa6 => self.ldx(inst),
                0xa8 => self.tay(inst),
                0xa9 => self.lda(inst),
                0xaa => self.tax(inst),
                0xac => self.ldy(inst),
                0xad => self.lda(inst),
                0xae => self.ldx(inst),

                0xb0 => self.bcs(inst),
                0xb1 => self.lda(inst),
                0xb4 => self.ldy(inst),
                0xb5 => self.lda(inst),
                0xb6 => self.ldx(inst),

                0xb8 => self.clv(inst),
                0xb9 => self.lda(inst),
                0xba => self.tsx(inst),
                0xbc => self.ldy(inst),
                0xbd => self.lda(inst),
                0xbe => self.ldx(inst),

                0xc0 => self.cpy(inst),
                0xc1 => self.cmp(inst),
                0xc4 => self.cpy(inst),
                0xc5 => self.cmp(inst),
                0xc6 => self.dec(inst),
                0xc8 => self.iny(inst),
                0xc9 => self.cmp(inst),
                0xca => self.dex(inst),
                0xcc => self.cpy(inst),
                0xcd => self.cmp(inst),
                0xce => self.dec(inst),

                0xd0 => self.bne(inst),
                0xd1 => self.cmp(inst),
                0xd5 => self.cmp(inst),
                0xd6 => self.dec(inst),
                0xd8 => self.cld(inst),
                0xd9 => self.cmp(inst),
                0xdd => self.cmp(inst),
                0xde => self.dec(inst),

                0xe0 => self.cpx(inst),
                0xe1 => self.sbc(inst),
                0xe4 => self.cpx(inst),
                0xe5 => self.sbc(inst),
                0xe6 => self.inc(inst),
                0xe8 => self.inx(inst),
                0xe9 => self.sbc(inst),
                0xea => self.nop(inst),
                0xec => self.cpx(inst),
                0xed => self.sbc(inst),
                0xee => self.inc(inst),

                0xf0 => self.beq(inst),
                0xf1 => self.sbc(inst),
                0xf5 => self.sbc(inst),
                0xf6 => self.inc(inst),
                0xf8 => self.sed(inst),
                0xf9 => self.sbc(inst),
                0xfd => self.sbc(inst),
                0xfe => self.inc(inst),
                _ => assert!(false),
            }
            trace!(
                "PC: {:04X}, X:{:02X} Y:{:02X} AC:{:02X} S:{:02X} ST:{:?} inst:{:02X} {}",
                save_pc,
                self.ix,
                self.iy,
                self.ac,
                self.sp,
                self.status,
                inst,
                self.trace_text
            );
        }
    }

    pub(crate) fn push(&mut self, val: u8) {
        self.ram[0x100 + self.sp as usize] = val;
        self.trace_text
            .push_str(&format!("{:02X} -> {:04X} ", val, 0x100 + self.sp as usize));
        self.sp = self.sp.wrapping_sub(1);
    }
    pub(crate) fn pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let val = self.ram[0x100 + self.sp as usize];
        self.trace_text.push_str(&format!(
            "{:02X} <- {:04X}  ",
            val,
            0x100 + self.sp as usize
        ));
        val
    }

    pub(crate) fn read_operand(&mut self, inst: u8, adjust_pc: bool) -> u8 {
        match inst {
            0xA2 | 0xa0 | 0xc0 | 0xe0 => {
                // immediate
                if adjust_pc {
                    self.pc += 1;
                }
                let v = self.read(self.pc - 1);
                self.trace_text.push_str(&format!("#${:02X} ", v));
                return v;
            }
            _ if inst & 0b00011100 == 0b00001000 => {
                // immediate
                if adjust_pc {
                    self.pc += 1;
                }
                let v = self.read(self.pc - 1);
                self.trace_text.push_str(&format!("#${:02X} ", v));
                return v;
            }
            _ => {
                let (opaddr, delta) = self.operand_addr(inst);
                if adjust_pc {
                    self.pc += delta;
                }
                return self.read(opaddr);
            }
        }
    }
    pub(crate) fn write_operand(&mut self, inst: u8, val: u8) {
        if inst & 0b000_111_00 == 0b000_101_00 {
            // accumulator
            self.ac = val;
            return;
        }
        let (opaddr, delta) = self.operand_addr(inst);
        self.pc += delta;
        self.write(opaddr, val);
    }

    fn operand_addr(&mut self, inst: u8) -> (u16, u16) {
        // calculate the address of operand plus pc delta
        let operand = match inst & 0b00011100 {
            0b000_001_00 => {
                // zero page
                self.trace_text
                    .push_str(&format!("${:02X} ", self.read(self.pc)));
                let zpaddr = self.read(self.pc);
                (zpaddr as u16, 1)
            }
            0b000_011_00 => {
                // absolute
                let lo = self.read(self.pc) as u16;
                let hi = self.read(self.pc + 1) as u16;
                self.trace_text.push_str(&format!("${:02X}{:02X}", hi, lo));
                ((hi << 8) | lo, 2)
            }
            0b000_101_00 => {
                // zpg, x -- except
                if inst == 0x96 || inst == 0xb6 {
                    self.zpg_y()
                } else {
                    self.zpg_x()
                }
            }
            0b000_111_00 => {
                // abs,x -- except
                if inst == 0xbe {
                    self.abs_y()
                } else {
                    self.abs_x()
                }
            }
            0b000_110_00 => {
                // abs,y
                self.abs_y()
            }
            0b000_000_00 => {
                // (ind),y
                let zpaddr = self.read(self.pc) as u16;

                let lo = self.read(zpaddr) as u16;
                let hi = self.read(zpaddr + 1) as u16;
                self.trace_text.push_str(&format!("(${:02X}),Y", zpaddr));
                (((hi << 8) | lo) + self.iy as u16, 1)
            }
            0b000_100_00 => {
                // (ind,x)
                let zpaddr = self.read(self.pc) as u16;

                let lo = self.read(zpaddr.wrapping_add(self.ix as u16)) as u16;
                let hi = self.read(zpaddr.wrapping_add(self.ix as u16) + 1) as u16;
                self.trace_text.push_str(&format!("(${:02X},X)", zpaddr));
                ((hi << 8) | lo, 1)
            }
            _ => panic!("Unknown addr format: {:02X}", inst),
        };
        operand
    }
    fn abs_y(&mut self) -> (u16, u16) {
        let lo = self.read(self.pc) as u16;
        let hi = self.read(self.pc + 1) as u16;
        self.trace_text
            .push_str(&format!("${:02X}{:02X},Y", hi, lo));
        (((hi << 8) | lo).wrapping_add(self.iy as u16), 2)
    }
    fn abs_x(&mut self) -> (u16, u16) {
        let lo = self.read(self.pc) as u16;
        let hi = self.read(self.pc + 1) as u16;
        self.trace_text
            .push_str(&format!("${:02X}{:02X},X", hi, lo));
        (((hi << 8) | lo).wrapping_add(self.ix as u16), 2)
    }
    fn zpg_x(&mut self) -> (u16, u16) {
        // zpg, x
        let zpaddr = self.read(self.pc) as u16;
        self.trace_text
            .push_str(&format!("${:02X},X", zpaddr as u8));
        (zpaddr.wrapping_add(self.ix as u16), 1)
    }
    fn zpg_y(&mut self) -> (u16, u16) {
        // zpg, y
        let zpaddr = self.read(self.pc) as u16;
        self.trace_text
            .push_str(&format!("${:02X},Y", zpaddr as u8));
        (zpaddr.wrapping_add(self.iy as u16), 1)
    }
    pub(crate) fn read(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }
    pub(crate) fn write(&mut self, addr: u16, val: u8) {
        self.ram[addr as usize] = val;
    }
}
