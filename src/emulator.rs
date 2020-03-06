/* *****************************************************************************
   Copyright 2020 Gustavo Moitinho Trindade

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
***************************************************************************** */
// src/emulator.rs
use crate::{EmuResult, error::*};
use crate::code::*;

const RAM_SIZE: usize = 0x10000;

/// The max value for a byte + 1
const BYTE_LIMIT: usize = 0x100;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum StatusFlag {
    Carry,
    Zero,
    InterruptDisable,
    DecimalMode,
    BreakCommand,
    Overflow,
    Negative,
}

pub struct Emulator {
    ram: [u8; RAM_SIZE],     // 65536 bytes of ram
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
            ram: [0; RAM_SIZE],
            a_register: 0,
            x_register: 0,
            y_register: 0,
            stack_pointer: 0,
            program_counter: 0,
            status_register: 0b0010_0100, // The 6502 starts with those flags up (the bit 5 is not used, but set anyway)
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

    #[inline]
    pub fn set_pc(&mut self, pc: u16) {
        self.program_counter = pc;
    }

    #[inline]
    /// Checks whether some status register flag is set.
    pub fn is_flag_set(&self, flag: StatusFlag) -> bool {
        // NV-BDIZC
        match flag {
            StatusFlag::Carry               => self.status_register & 0b0000_0001 != 0,
            StatusFlag::Zero                => self.status_register & 0b0000_0010 != 0,
            StatusFlag::InterruptDisable    => self.status_register & 0b0000_0100 != 0,
            StatusFlag::DecimalMode         => self.status_register & 0b0000_1000 != 0,
            StatusFlag::BreakCommand        => self.status_register & 0b0001_0000 != 0,
            StatusFlag::Overflow            => self.status_register & 0b0100_0000 != 0,
            StatusFlag::Negative            => self.status_register & 0b1000_0000 != 0,
        }
    }

    #[inline]
    fn set_flag(&mut self, flag: StatusFlag, set_to: bool) {
        // NV-BDIZC
        let bin_flag = match flag {
            StatusFlag::Carry               => 0b0000_0001,
            StatusFlag::Zero                => 0b0000_0010,
            StatusFlag::InterruptDisable    => 0b0000_0100,
            StatusFlag::DecimalMode         => 0b0000_1000,
            StatusFlag::BreakCommand        => 0b0001_0000,
            StatusFlag::Overflow            => 0b0100_0000,
            StatusFlag::Negative            => 0b1000_0000,
        };
        if set_to == true {
            self.status_register |= bin_flag; 
        } else {
            self.status_register &= !bin_flag;
        }
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

    /// This function returns the value that `addressing_mode` refers to. It returns a byte (`u8`).
    /// In case the addressing mode is `Immediate`, it will return the immediate itself.
    ///
    /// # Panics
    /// This function panics if the `addressing_mode` is either `Implied` or `Accumulator`.
    #[inline]
    fn lookup(&self, addressing_mode: AddressingMode) -> u8 {
        use AddressingMode::*;
        match addressing_mode {
            Implied => panic!("Addressing implied instruction!"),
            Accumulator => panic!("Addressing accumultator instruction!"),
            Immediate(val)  => val,
            ZeroPage(val)   => self.ram[val as usize],
            ZeroPageX(val)  => self.ram[(val as usize + self.x_register as usize) % BYTE_LIMIT],
            ZeroPageY(val)  => self.ram[(val as usize + self.y_register as usize) % BYTE_LIMIT],
            Absolute(val)   => self.ram[val as usize],
            AbsoluteX(val)  => self.ram[(val as usize + self.x_register as usize) % RAM_SIZE],
            AbsoluteY(val)  => self.ram[(val as usize + self.y_register as usize) % RAM_SIZE],
            Indirect(val)   => self.ram[self.ram[val as usize] as usize],
            IndirectX(val)  => self.ram[self.ram[val as usize] as usize % BYTE_LIMIT],
            IndirectY(val)  => self.ram[self.ram[val as usize] as usize % BYTE_LIMIT],
            Relative(val)   => self.ram[(self.program_counter as isize + val as isize) as usize % RAM_SIZE]
        }
    }

    /// This function returns a mutable reference (borrow) to what `addressing_mode` refers (as a
    /// &mut u8).
    ///
    /// # Panics
    /// This function panics if the `addressing_mode` is either `Implied`, `Accumulator` or
    /// `Immediate`.
    #[inline]
    fn lookup_mut(&mut self, addressing_mode: AddressingMode) -> &mut u8 {
        use AddressingMode::*;
        match addressing_mode {
            Implied => panic!("Addressing implied instruction!"),
            Accumulator => panic!("Addressing accumultator instruction!"),
            Immediate(_)  => panic!("Addressing immediate as mutable!"),
            ZeroPage(val)   => &mut self.ram[val as usize],
            ZeroPageX(val)  => &mut self.ram[(val as usize + self.x_register as usize) % BYTE_LIMIT],
            ZeroPageY(val)  => &mut self.ram[(val as usize + self.y_register as usize) % BYTE_LIMIT],
            Absolute(val)   => &mut self.ram[val as usize],
            AbsoluteX(val)  => &mut self.ram[(val as usize + self.x_register as usize) % RAM_SIZE],
            AbsoluteY(val)  => &mut self.ram[(val as usize + self.y_register as usize) % RAM_SIZE],
            Indirect(val)   => &mut self.ram[self.ram[val as usize] as usize],
            IndirectX(val)  => &mut self.ram[self.ram[val as usize] as usize % BYTE_LIMIT],
            IndirectY(val)  => &mut self.ram[self.ram[val as usize] as usize % BYTE_LIMIT],
            Relative(val)   => &mut self.ram[(self.program_counter as isize + val as isize) as usize % RAM_SIZE]
        }
    }

    // Sets the Zero and Negative flags depending on whether n is Zero or Negative. This exists
    // because it is very common for this to be required.
    #[inline]
    fn set_result_flags_for(&mut self, n: i8) {
        self.set_flag(StatusFlag::Zero, n == 0);
        self.set_flag(StatusFlag::Negative, n < 0);
    }

    fn ula_add(&mut self, ra: i8, rb: i8) -> i8 {
        let correct_result = ra as i32 + rb as i32;
        let result = ra.wrapping_add(rb);

        // Set Overflow when the sign is wrong (from positive to negative or the opposite)
        self.set_flag(StatusFlag::Overflow, (correct_result > 0 && result < 0) || (correct_result < 0 && result > 0));
        // If overflow in bit 7 (has only bits 0 to 7, so overflow in bit 7 means number too big)
        self.set_flag(StatusFlag::Carry, correct_result >= (BYTE_LIMIT/2) as i32 || correct_result < (BYTE_LIMIT/2) as i32);
        // Very simple. Sets Zero and Negative flags. This is used so often it warranted a
        // function.
        self.set_result_flags_for(result);

        result
    }

    fn ula_sub(&mut self, ra: i8, rb: i8) -> i8 {
        let correct_result = ra as i32 - rb as i32;
        let result = ra.wrapping_sub(rb);

        // Set Overflow when the sign is wrong (from positive to negative or the opposite)
        self.set_flag(StatusFlag::Overflow, (correct_result > 0 && result < 0) || (correct_result < 0 && result > 0));
        // If overflow in bit 7 (has only bits 0 to 7, so overflow in bit 7 means number too big)
        self.set_flag(StatusFlag::Carry, correct_result >= (BYTE_LIMIT/2) as i32 || correct_result < (BYTE_LIMIT/2) as i32);
        // Very simple. Sets Zero and Negative flags. This is used so often it warranted a
        // function.
        self.set_result_flags_for(result);

        result
    }

    fn perform_instruction(&mut self, instruction: Instruction) {
        use OpClass::*;

        match instruction.tuple() {
            (ADC, addr_mode) => {
                self.a_register = self.ula_add(self.a_register as i8, self.lookup(addr_mode) as i8) as u8;
            },
            (BEQ, AddressingMode::Relative(val)) => {
                if self.is_flag_set(StatusFlag::Zero) {
                    self.program_counter = (self.program_counter as isize + val as isize) as u16;
                }
            },
            (BMI, AddressingMode::Relative(val)) => {
                if self.is_flag_set(StatusFlag::Negative) {
                    self.program_counter = (self.program_counter as isize + val as isize) as u16;
                }
            },
            (BRK, AddressingMode::Implied) => {
                // TODO this one is FAR from done
                self.set_flag(StatusFlag::BreakCommand, true);
            }
            (JMP, AddressingMode::Relative(val)) => {
                self.program_counter = (self.program_counter as isize + val as isize) as u16;
            },
            (LDA, addr_mode) => {
                self.a_register = self.lookup(addr_mode);

                if self.a_register == 0 {
                    self.set_flag(StatusFlag::Zero, true);
                } else if (self.a_register as i8) < 0 {
                    self.set_flag(StatusFlag::Negative, true);
                }
            },
            (SBC, addr_mode) => {
                self.a_register = self.ula_sub(self.a_register as i8, self.lookup(addr_mode) as i8) as u8;
            },
            (STA, addr_mode) => {
                *self.lookup_mut(addr_mode) = self.a_register;
            },
            (TAX, AddressingMode::Implied) => {
                self.x_register = self.a_register;

                if self.x_register == 0 {
                    self.set_flag(StatusFlag::Zero, true);
                } else if (self.x_register as i8) < 0 {
                    self.set_flag(StatusFlag::Negative, true);
                }
            },
            (TAY, AddressingMode::Implied) => {
                self.y_register = self.a_register;

                if self.y_register == 0 {
                    self.set_flag(StatusFlag::Zero, true);
                } else if (self.y_register as i8) < 0 {
                    self.set_flag(StatusFlag::Negative, true);
                }
            },
            _ => {
                unimplemented!()
            }
        } // End of match instruction
    } // End of fn perform_instruction(..)
} // End of impl Emulator
