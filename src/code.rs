pub use crate::{EmuResult, error::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpClass {
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
    JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI,
    RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressingMode {
    // These are the addressing modes of the 6502. You should look them up on the 6502
    // documentation.
    // It's important to note that the values are stored here as unsigned, but the 6502 actually
    // takes a flexible approach to the ways this values are used, so it can interpret values as
    // signed or unsigned depending on the context. The biggest exception is Relative, which is
    // stored as i8, as it is always intended to be a value "relative" to the current PC, that can
    // actually mean something that is lower or greater than PC (negative or positive respectively).
    Implied,
    Immediate(u8),
    ZeroPage(u8),
    ZeroPageX(u8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Indirect(u16),
    IndirectX(u8),
    IndirectY(u8),
    Relative(i8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instruction(OpClass, AddressingMode);

impl AddressingMode {
    pub fn length(self) -> usize {
        use AddressingMode::*;
        match self {
            Implied => 0,
            Immediate(_) | ZeroPage(_) | ZeroPageX(_) | IndirectX(_) | IndirectY(_) | Relative(_) => 1,
            Absolute(_) | AbsoluteX(_) | AbsoluteY(_) | Indirect(_) => 2,
        }
    }

    pub fn new_immediate(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 1 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::Immediate(data[0]))
        }
    }

    pub fn new_zeropage(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 1 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::ZeroPage(data[0]))
        }
    }

    pub fn new_zeropagex(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 1 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::ZeroPageX(data[0]))
        }
    }

    pub fn new_absolute(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 2 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::Absolute(data[0] as u16 + ((data[1] as u16) << 8)))
        }
    }

    pub fn new_absolutex(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 2 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::AbsoluteX(data[0] as u16 + ((data[1] as u16) << 8)))
        }
    }

    pub fn new_absolutey(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 2 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::AbsoluteY(data[0] as u16 + ((data[1] as u16) << 8)))
        }
    }

    pub fn new_indirect(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 2 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::Indirect(data[0] as u16 + ((data[1] as u16) << 8)))
        }
    }

    pub fn new_indirectx(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 1 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::IndirectX(data[0]))
        }
    }

    pub fn new_indirecty(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 1 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::IndirectY(data[0]))
        }
    }

    pub fn new_relative(data: &[u8]) -> EmuResult<Self> {
        if data.len() < 1 {
            Err(Error::new(ErrorClass::InvalidArgument, "coult not create address mode from the given slice"))
        } else {
            Ok(AddressingMode::Relative(data[0] as i8))
        }
    }
}

impl Instruction {
    #[inline]
    /// Creates a new instruction. This function SHOULD (IT DOES NOT YET!) return an error when the
    /// instruction formed by the given parameters is not valid.
    pub fn new(opclass: OpClass, addr_mode: AddressingMode) -> EmuResult<Self> {
        Ok(Self(opclass, addr_mode))
    }

    pub fn class(self) -> OpClass {
        self.0
    }

    pub fn addr_mode(self) -> AddressingMode {
        self.1
    }
    
    /// This function returns the instruction data as a tuple
    // So, why does this function exists? Well, I decided previously that an INCORRECT instruction
    // was not supposed to be possible to create safely. This meant that the fields of the
    // instruction are set to private, and creating one should require using a method that will
    // check to see if the instruction is proper.
    //
    // Nevertheless, there would be times I would want to match the instruction, as a pattern, and,
    // for that reason, I would WANT to access the elements inside of the Instruction. For this
    // purpose, this method was created, so that it is possible to match instructions in a match.
    // This is perfect because it still keeps the requirement that every instruction created safely
    // is a valid instruction. Something that couldn't be enforced if the instruction was a tuple
    // to begin with.
    pub fn tuple(self) -> (OpClass, AddressingMode) {
        (self.0, self.1)
    }

    /// Not implemented yet.
    /// This function encodes the instruction into 6502 machine code.
    pub fn encode(self) -> Vec<u8> {
        unimplemented!()
    }

    /// This function takes 6502 machine code representing a single instrution, and proceeds to
    /// build an instruction from it. This function SHOULD (IT DOES NOT YET!) return an error if
    /// the given machine code section does not represent a valid instruction.
    pub fn decode(data: &[u8]) -> EmuResult<Self> {
        if data.len() == 0 {
            Err(Error::new(ErrorClass::ParseError, "asked to parse empty slice into instruction"))
        } else {
            let opcode = data[0];
            // This will be a big enum
            // Lets start with 9 instructions and all of their variants. Thats few, but it will set
            // our framework here.
            // Those will be:
            // ADC,
            // SBC,
            // STA,
            // LDA,
            //
            // JMP,
            // BEQ,
            // BMI,
            // TAX,
            // TAY,
            // ... for now
            match opcode {
                // ADC
                0x69 => Instruction::new(OpClass::ADC, AddressingMode::new_immediate(&data[1..])?), // Immediate
                0x65 => Instruction::new(OpClass::ADC, AddressingMode::new_zeropage(&data[1..])?),  // ZeroPage
                0x75 => Instruction::new(OpClass::ADC, AddressingMode::new_zeropagex(&data[1..])?), // ZeroPageX
                0x6d => Instruction::new(OpClass::ADC, AddressingMode::new_absolute(&data[1..])?),  // Absolute
                0x7d => Instruction::new(OpClass::ADC, AddressingMode::new_absolutex(&data[1..])?), // AbsoluteX
                0x79 => Instruction::new(OpClass::ADC, AddressingMode::new_absolutey(&data[1..])?), // AbsoluteY
                0x61 => Instruction::new(OpClass::ADC, AddressingMode::new_indirectx(&data[1..])?), // IndirectX
                0x71 => Instruction::new(OpClass::ADC, AddressingMode::new_indirecty(&data[1..])?), // IndirectY
                // SBC
                0xe9 => Instruction::new(OpClass::SBC, AddressingMode::new_immediate(&data[1..])?), // Immediate
                0xe5 => Instruction::new(OpClass::SBC, AddressingMode::new_zeropage(&data[1..])?),  // ZeroPage
                0xf5 => Instruction::new(OpClass::SBC, AddressingMode::new_zeropagex(&data[1..])?), // ZeroPageX
                0xed => Instruction::new(OpClass::SBC, AddressingMode::new_absolute(&data[1..])?),  // Absolute
                0xfd => Instruction::new(OpClass::SBC, AddressingMode::new_absolutex(&data[1..])?), // AbsoluteX
                0xf9 => Instruction::new(OpClass::SBC, AddressingMode::new_absolutey(&data[1..])?), // AbsoluteY
                0xe1 => Instruction::new(OpClass::SBC, AddressingMode::new_indirectx(&data[1..])?), // IndirectX
                0xf1 => Instruction::new(OpClass::SBC, AddressingMode::new_indirecty(&data[1..])?), // IndirectY
                // STA
                0x85 => Instruction::new(OpClass::STA, AddressingMode::new_zeropage(&data[1..])?),  // ZeroPage
                0x95 => Instruction::new(OpClass::STA, AddressingMode::new_zeropagex(&data[1..])?), // ZeroPageX
                0x8d => Instruction::new(OpClass::STA, AddressingMode::new_absolute(&data[1..])?),  // Absolute
                0x9d => Instruction::new(OpClass::STA, AddressingMode::new_absolutex(&data[1..])?), // AbsoluteX
                0x99 => Instruction::new(OpClass::STA, AddressingMode::new_absolutey(&data[1..])?), // AbsoluteY
                0x81 => Instruction::new(OpClass::STA, AddressingMode::new_indirectx(&data[1..])?), // IndirectX
                0x91 => Instruction::new(OpClass::STA, AddressingMode::new_indirecty(&data[1..])?), // IndirectY
                // LDA
                0xa9 => Instruction::new(OpClass::LDA, AddressingMode::new_immediate(&data[1..])?), // Immediate
                0xa5 => Instruction::new(OpClass::LDA, AddressingMode::new_zeropage(&data[1..])?),  // ZeroPage
                0xb5 => Instruction::new(OpClass::LDA, AddressingMode::new_zeropagex(&data[1..])?), // ZeroPageX
                0xad => Instruction::new(OpClass::LDA, AddressingMode::new_absolute(&data[1..])?),  // Absolute
                0xbd => Instruction::new(OpClass::LDA, AddressingMode::new_absolutex(&data[1..])?), // AbsoluteX
                0xb9 => Instruction::new(OpClass::LDA, AddressingMode::new_absolutey(&data[1..])?), // AbsoluteY
                0xa1 => Instruction::new(OpClass::LDA, AddressingMode::new_indirectx(&data[1..])?), // IndirectX
                0xb1 => Instruction::new(OpClass::LDA, AddressingMode::new_indirecty(&data[1..])?), // IndirectY
                // JMP
                0x4c => Instruction::new(OpClass::JMP, AddressingMode::new_absolute(&data[1..])?),  // Absolute
                0x6c => Instruction::new(OpClass::JMP, AddressingMode::new_indirect(&data[1..])?),  // Indirect
                // BEQ
                0xf0 => Instruction::new(OpClass::BEQ, AddressingMode::new_relative(&data[1..])?),  // Relative
                // BMI
                0x30 => Instruction::new(OpClass::BMI, AddressingMode::new_relative(&data[1..])?),  // Relative
                // TAX
                0xaa => Instruction::new(OpClass::TAX, AddressingMode::Implied),                    // Implied
                // TAY
                0xa8 => Instruction::new(OpClass::TAY, AddressingMode::Implied),                    // Implied
                n => {
                    Err(Error::new(ErrorClass::ParseError, format!("unknown opcode {}", n)))
                }
            }
        }
    } // End of fn decode(..)
} // End of impl Instruction
