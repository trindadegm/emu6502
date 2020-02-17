use crate::{EmuResult, error::*};
use crate::code::*;

pub struct Emulator {
    ram: [u8; 0x10000],     // 65536 bytes of ram
    a_register: u8,
    x_register: u8,
    y_register: u8,         
    stack_pointer: u8,     // The stack pointer points to wherever the stack is at
    program_counter: u16,   // The famous PC
    status_register: u8,    // NV-BDIZC flags
}

impl Default for Emulator {
    fn default() -> Self {
        Self {
            ram: [0; 0x10000],
            a_register: 0,
            x_register: 0,
            y_register: 0,
            stack_pointer: 0,
            program_counter: 0,
            status_register: 0,
        }
    }
}

impl Emulator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_rom<R>(rom: R, offset: usize) -> Self
    where
        R: AsRef<[u8]>
    {
        let rom = rom.as_ref();
        let mut emu = Self::default();
        emu.ram[offset..rom.len() + offset].copy_from_slice(rom);

        emu
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.program_counter = pc;
    }

    pub fn ram(&self) -> &[u8] {
        &self.ram
    }

    pub fn step(&mut self) -> EmuResult<()> {
        let ram_from_pc = &self.ram[self.program_counter as usize..];
        let instruction = Instruction::decode(ram_from_pc)?;
        log::debug!("Decoded instruction: {:?}", instruction);

        self.perform_instruction(instruction);

        self.program_counter += (1 + instruction.addr_mode().length()) as u16;
        Ok(())
    }

    fn perform_instruction(&mut self, instruction: Instruction) {
        use OpClass::*;
        use AddressingMode::*;

        match instruction.tuple() {
            (ADC, Immediate(val)) => {
                self.a_register += val;
                // TODO check for carry!
            },
            (ADC, ZeroPage(val)) => {
                self.a_register += self.ram[val as usize];
                // TODO check for carry!
            },
            (ADC, ZeroPageX(val)) => {
                self.a_register += self.ram[(val as usize + self.x_register as usize) % 0xff];
                // TODO check for carry!
            },
            (ADC, Absolute(val)) => {
                self.a_register += self.ram[val as usize];
                // TODO check for carry!
            },
            (ADC, AbsoluteX(val)) => {
                self.a_register += self.ram[(val as usize + self.x_register as usize) % 0x10000];
                // TODO check for carry!
            },
            (ADC, AbsoluteY(val)) => {
                self.a_register += self.ram[(val as usize + self.y_register as usize) % 0x10000];
                // TODO check for carry!
            },
            (ADC, IndirectX(val)) => {
                self.a_register += self.ram[self.ram[(val as usize + self.x_register as usize) % 0x10000] as usize];
                // TODO check for carry!
            },
            (ADC, IndirectY(val)) => {
                self.a_register += self.ram[self.ram[(val as usize + self.y_register as usize) % 0x10000] as usize];
                // TODO check for carry!
            },
            _ => {
                unimplemented!()
            }
        }
    }
} // End of impl Emulator
