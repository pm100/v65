use crate::cpu::IRQ;
use crate::cpu::{Cpu, Status};
impl Cpu {
    pub(crate) fn brk(&mut self, _inst: u8) {
        self.trace_text = String::from("BRK ");
        self.push(((self.pc + 1) >> 8) as u8);
        self.push((self.pc + 1) as u8);
        // self.status.set(Status::BREAK, true);
        self.push((self.status | Status::BREAK | Status::UNUSED).bits());
        let target = self.read(IRQ) as u16 | (self.read(IRQ + 1) as u16) << 8;
        self.status.set(Status::IDISABLE, true);
        //self.pc += 1;
        self.pc = target;
    }
    pub(crate) fn ora(&mut self, inst: u8) {
        self.trace_text = String::from("ORA ");
        let operand = self.read_operand(inst, true);
        self.ac |= operand;
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }
    pub(crate) fn asl(&mut self, inst: u8) {
        self.trace_text = String::from("ASL ");
        let operand = self.read_operand(inst, false);
        let result = operand << 1;
        self.status.set(Status::CARRY, operand & 0b10000000 != 0);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & 0b10000000 != 0);
        self.write_operand(inst, result);
    }
    pub(crate) fn php(&mut self, _inst: u8) {
        self.trace_text = String::from("PHP ");
        self.push((self.status | Status::BREAK | Status::UNUSED).bits());
    }

    pub(crate) fn clc(&mut self, _inst: u8) {
        self.trace_text = String::from("CLC ");
        self.status.set(Status::CARRY, false);
    }
    pub(crate) fn jsr(&mut self, _inst: u8) {
        self.trace_text = String::from("JSR ");
        let lo = self.read(self.pc) as u16;
        let hi = self.read(self.pc + 1) as u16;
        let addr = (hi << 8) | lo;
        self.push(((self.pc + 1) >> 8) as u8);
        self.push((self.pc + 1) as u8);
        self.pc = addr;
    }
    pub(crate) fn and(&mut self, inst: u8) {
        self.trace_text = String::from("AND ");
        let operand = self.read_operand(inst, true);
        self.ac &= operand;
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }
    pub(crate) fn bit(&mut self, inst: u8) {
        self.trace_text = String::from("BIT ");
        let operand = self.read_operand(inst, true);
        self.status.set(Status::ZERO, operand & self.ac == 0);
        self.status.set(Status::OVF, operand & 0b01000000 != 0);
        self.status.set(Status::NEGATIVE, operand & 0b10000000 != 0);
    }
    pub(crate) fn rol(&mut self, inst: u8) {
        self.trace_text = String::from("ROL ");
        let operand = self.read_operand(inst, false);
        let result = (operand << 1) | (self.status.contains(Status::CARRY) as u8);
        self.status.set(Status::CARRY, operand & 0b10000000 != 0);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & 0b10000000 != 0);
        self.write_operand(inst, result);
    }
    pub(crate) fn plp(&mut self, _inst: u8) {
        self.trace_text = String::from("PLP ");
        self.status = Status::from_bits_truncate(self.pop());
        self.status.set(Status::BREAK, false);
        self.status.set(Status::UNUSED, false);
    }

    pub(crate) fn sec(&mut self, _inst: u8) {
        self.trace_text = String::from("SEC ");
        self.status.set(Status::CARRY, true);
    }
    pub(crate) fn rti(&mut self, _inst: u8) {
        self.trace_text = String::from("RTI ");
        self.status = Status::from_bits_truncate(self.pop());
        self.status.set(Status::BREAK, false);
        self.status.set(Status::UNUSED, false);
        self.pc = self.pop() as u16 | ((self.pop() as u16) << 8);
    }
    pub(crate) fn eor(&mut self, inst: u8) {
        self.trace_text = String::from("EOR ");
        let operand = self.read_operand(inst, true);
        self.ac ^= operand;
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }
    pub(crate) fn lsr(&mut self, inst: u8) {
        self.trace_text = String::from("LSR ");
        let operand = self.read_operand(inst, false);
        let result = operand >> 1;
        self.status.set(Status::CARRY, operand & 0b00000001 != 0);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & 0b10000000 != 0);
        self.write_operand(inst, result);
    }
    pub(crate) fn pha(&mut self, _inst: u8) {
        self.trace_text = String::from("PHA ");
        self.push(self.ac);
    }
    pub(crate) fn jmp(&mut self, _inst: u8) {
        self.trace_text = String::from("JMP ");
        let lo = self.read(self.pc) as u16;
        let hi = self.read(self.pc + 1) as u16;
        let addr = (hi << 8) | lo;
        assert_ne!(
            addr + 1,
            self.pc,
            "Infinite loop detected at {:04X}",
            self.pc
        );
        self.pc = addr;
    }
    pub(crate) fn jmp_ind(&mut self, _inst: u8) {
        self.trace_text = String::from("JMP ");
        let lo = self.read(self.pc) as u16;
        let hi = self.read(self.pc + 1) as u16;
        let addr = (hi << 8) | lo;
        let target = self.read(addr) as u16 | ((self.read(addr + 1) as u16) << 8);
        assert_ne!(
            target + 1,
            self.pc,
            "Infinite loop detected at {:04X}",
            self.pc
        );
        self.pc = target;
    }
    pub(crate) fn cli(&mut self, _inst: u8) {
        self.trace_text = String::from("CLI ");
        self.status.set(Status::IDISABLE, false);
    }
    pub(crate) fn rts(&mut self, _inst: u8) {
        self.trace_text = String::from("RTS ");
        self.pc = self.pop() as u16 | ((self.pop() as u16) << 8);
        self.pc += 1;
    }
    pub(crate) fn adc(&mut self, inst: u8) {
        self.trace_text = String::from("ADC ");
        let operand = self.read_operand(inst, true);
        let result = self.ac as u16 + operand as u16 + self.status.contains(Status::CARRY) as u16;
        self.status.set(Status::CARRY, result > 0xFF);
        self.status.set(Status::ZERO, result & 0xFF == 0);
        self.status.set(
            Status::OVF,
            (self.ac ^ result as u8) & (operand ^ result as u8) & 0x80 != 0,
        );
        self.status.set(Status::NEGATIVE, result & 0x80 != 0);
        self.ac = result as u8;
    }
    pub(crate) fn ror(&mut self, inst: u8) {
        self.trace_text = String::from("ROR ");
        let operand = self.read_operand(inst, false);
        let result = (operand >> 1) | (self.status.contains(Status::CARRY) as u8) << 7;
        self.status.set(Status::CARRY, operand & 0b00000001 != 0);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & 0b10000000 != 0);
        self.write_operand(inst, result);
    }
    pub(crate) fn pla(&mut self, _inst: u8) {
        self.trace_text = String::from("PLA ");
        self.ac = self.pop();
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }

    pub(crate) fn sei(&mut self, _inst: u8) {
        self.trace_text = String::from("SEI ");
        self.status.set(Status::IDISABLE, true);
    }
    pub(crate) fn sta(&mut self, inst: u8) {
        self.trace_text = String::from("STA ");
        self.write_operand(inst, self.ac)
    }
    pub(crate) fn sty(&mut self, inst: u8) {
        self.trace_text = String::from("STY ");
        self.write_operand(inst, self.iy);
    }
    pub(crate) fn stx(&mut self, inst: u8) {
        self.trace_text = String::from("STX ");
        self.write_operand(inst, self.ix);
    }
    pub(crate) fn dey(&mut self, _inst: u8) {
        self.trace_text = String::from("DEY ");
        self.iy = self.iy.wrapping_sub(1);
        self.status.set(Status::ZERO, self.iy == 0);
        self.status.set(Status::NEGATIVE, self.iy & 0b10000000 != 0);
    }
    pub(crate) fn txa(&mut self, _inst: u8) {
        self.trace_text = String::from("TXA ");
        self.ac = self.ix;
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }

    pub(crate) fn tya(&mut self, _inst: u8) {
        self.trace_text = String::from("TYA ");
        self.ac = self.iy;
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }
    pub(crate) fn txs(&mut self, _inst: u8) {
        self.trace_text = String::from("TXS ");
        self.sp = self.ix;
    }
    pub(crate) fn ldy(&mut self, inst: u8) {
        self.trace_text = String::from("LDY ");
        let operand = self.read_operand(inst, true);
        self.iy = operand;
        self.status.set(Status::ZERO, self.iy == 0);
        self.status.set(Status::NEGATIVE, self.iy & 0b10000000 != 0);
    }
    pub(crate) fn lda(&mut self, inst: u8) {
        self.trace_text = String::from("LDA ");
        let operand = self.read_operand(inst, true);
        self.ac = operand;
        self.status.set(Status::ZERO, self.ac == 0);
        self.status.set(Status::NEGATIVE, self.ac & 0b10000000 != 0);
    }
    pub(crate) fn ldx(&mut self, inst: u8) {
        self.trace_text = String::from("LDX ");
        let operand = self.read_operand(inst, true);
        self.ix = operand;
        self.status.set(Status::ZERO, self.ix == 0);
        self.status.set(Status::NEGATIVE, self.ix & 0b10000000 != 0);
    }
    pub(crate) fn tay(&mut self, _inst: u8) {
        self.trace_text = String::from("TAY ");
        self.iy = self.ac;
        self.status.set(Status::ZERO, self.iy == 0);
        self.status.set(Status::NEGATIVE, self.iy & 0b10000000 != 0);
    }
    pub(crate) fn tax(&mut self, _inst: u8) {
        self.trace_text = String::from("TAX ");
        self.ix = self.ac;
        self.status.set(Status::ZERO, self.ix == 0);
        self.status.set(Status::NEGATIVE, self.ix & 0b10000000 != 0);
    }

    pub(crate) fn clv(&mut self, _inst: u8) {
        self.trace_text = String::from("CLV ");
        self.status.set(Status::OVF, false);
    }
    pub(crate) fn tsx(&mut self, _inst: u8) {
        self.trace_text = String::from("TSX ");
        self.ix = self.sp;
        self.status.set(Status::ZERO, self.ix == 0);
        self.status.set(Status::NEGATIVE, self.ix & 0b10000000 != 0);
    }
    pub(crate) fn cpy(&mut self, inst: u8) {
        self.trace_text = String::from("CPY ");
        self.compare(inst, self.iy);
    }
    pub(crate) fn cmp(&mut self, inst: u8) {
        self.trace_text = String::from("CMP ");
        self.compare(inst, self.ac);
    }
    pub(crate) fn compare(&mut self, inst: u8, reg: u8) {
        let operand = self.read_operand(inst, true);
        let result = (reg as i8).wrapping_sub(operand as i8);
        self.status.set(Status::CARRY, reg >= operand);
        self.status.set(Status::ZERO, operand == reg);
        self.status.set(Status::NEGATIVE, result as u8 & 0x80 != 0);
    }
    pub(crate) fn dec(&mut self, inst: u8) {
        self.trace_text = String::from("DEC ");
        let operand = self.read_operand(inst, false);
        let result = operand.wrapping_sub(1);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & 0b10000000 != 0);
        self.write_operand(inst, result);
    }
    pub(crate) fn iny(&mut self, _inst: u8) {
        self.trace_text = String::from("INY ");
        self.iy = self.iy.wrapping_add(1);
        self.status.set(Status::ZERO, self.iy == 0);
        self.status.set(Status::NEGATIVE, self.iy & 0b10000000 != 0);
    }
    pub(crate) fn dex(&mut self, _inst: u8) {
        self.trace_text = String::from("DEX ");
        self.ix = self.ix.wrapping_sub(1);
        self.status.set(Status::ZERO, self.ix == 0);
        self.status.set(Status::NEGATIVE, self.ix & 0b10000000 != 0);
    }

    pub(crate) fn cld(&mut self, _inst: u8) {
        self.trace_text = String::from("CLD ");
        self.status.set(Status::DECIMAL, false);
    }
    pub(crate) fn cpx(&mut self, inst: u8) {
        self.trace_text = String::from("CPX ");
        self.compare(inst, self.ix);
    }
    pub(crate) fn sbc(&mut self, inst: u8) {
        self.trace_text = String::from("SBC ");
        let operand = self.read_operand(inst, true);
        let result = self.ac as u16 - operand as u16 - !self.status.contains(Status::CARRY) as u16;
        self.status.set(Status::CARRY, result < 0x100);
        self.status.set(Status::ZERO, result & 0xFF == 0);
        self.status.set(
            Status::OVF,
            (self.ac ^ result as u8) & (!operand ^ result as u8) & 0x80 != 0,
        );
        self.status.set(Status::NEGATIVE, result & 0x80 != 0);
        self.ac = result as u8;
    }
    pub(crate) fn inc(&mut self, inst: u8) {
        self.trace_text = String::from("INC ");
        let operand = self.read_operand(inst, false);
        let result = operand.wrapping_add(1);
        self.status.set(Status::ZERO, result == 0);
        self.status.set(Status::NEGATIVE, result & 0b10000000 != 0);
        self.write_operand(inst, result);
    }
    pub(crate) fn inx(&mut self, _inst: u8) {
        self.trace_text = String::from("INX ");
        self.ix = self.ix.wrapping_add(1);
        self.status.set(Status::ZERO, self.ix == 0);
        self.status.set(Status::NEGATIVE, self.ix & 0b10000000 != 0);
    }
    pub(crate) fn sed(&mut self, _inst: u8) {
        self.trace_text = String::from("SED ");
        self.status.set(Status::DECIMAL, true);
    }
    pub(crate) fn nop(&mut self, _inst: u8) {
        self.trace_text = String::from("NOP ");
    }
    pub(crate) fn beq(&mut self, _inst: u8) {
        self.trace_text = String::from("BEQ ");
        self.branchs(Status::ZERO);
    }
    pub(crate) fn bne(&mut self, _inst: u8) {
        self.trace_text = String::from("BNE ");
        self.branchc(Status::ZERO);
    }
    pub(crate) fn bcs(&mut self, _inst: u8) {
        self.trace_text = String::from("BCS ");
        self.branchs(Status::CARRY);
    }
    pub(crate) fn bcc(&mut self, _inst: u8) {
        self.trace_text = String::from("BCC ");
        self.branchc(Status::CARRY);
    }
    pub(crate) fn bvc(&mut self, _inst: u8) {
        self.trace_text = String::from("BVC ");
        self.branchc(Status::OVF);
    }
    pub(crate) fn bvs(&mut self, _inst: u8) {
        self.trace_text = String::from("BVS ");
        self.branchs(Status::OVF);
    }
    pub(crate) fn bmi(&mut self, _inst: u8) {
        self.trace_text = String::from("BMI ");
        self.branchs(Status::NEGATIVE);
    }

    pub(crate) fn bpl(&mut self, _inst: u8) {
        self.trace_text = String::from("BPL ");
        self.branchc(Status::NEGATIVE);
    }

    fn branchs(&mut self, flag: Status) {
        let delta = self.ram[(self.pc) as usize] as i8;
        self.pc += 1;
        self.trace_text.push_str(&format!(
            "{:02X}->{:04X} ",
            delta,
            self.pc.wrapping_add_signed(delta as i16)
        ));

        if self.status.contains(flag) {
            assert!(delta != -2, "Infinite loop detected at {:04X}", self.pc - 1);
            self.pc = self.pc.wrapping_add_signed(delta as i16);
        }
    }
    fn branchc(&mut self, flag: Status) {
        let delta = self.ram[(self.pc) as usize] as i8;
        self.pc += 1;
        self.trace_text.push_str(&format!(
            "{:02X}->{:04X} ",
            delta,
            self.pc.wrapping_add_signed(delta as i16)
        ));
        if !self.status.contains(flag) {
            assert!(delta != -2, "Infinite loop detected at {:04X}", self.pc - 1);
            self.pc = self.pc.wrapping_add_signed(delta as i16);
        }
    }
}
