use crate::EmuResult;
use crate::error::*;
use crate::code::*;

use std::collections::HashMap;

pub fn assemble<S>(source: S) -> EmuResult<Vec<u8>>
where
    S: AsRef<str>,
{
    let mut label_table = HashMap::with_capacity(64);
    let mut code_buff = Vec::with_capacity(0x4000);
    let mut code_length = 0usize;

    let lines_it = source.as_ref().split("\n");
    for (line_number, loc) in lines_it.enumerate() {
        let line_number = line_number + 1; // Lines should be starting at 1, not at 0
        let loc = loc.trim();
        //log::debug!("{:04x}: {}", line_number, loc);
        // Jump comment lines or empty lines
        if loc.starts_with(";") || loc.len() == 0 {
            continue;
        }

        let mut instruction_it = loc.split_whitespace();
        let lhs = instruction_it.next()
            .ok_or(Error::new(ErrorClass::ParseError, format!("failed to parse instruction at line {}", line_number)))?
            .trim();

        // If it ends with ':', then it is a label
        if lhs.ends_with(':') {
            // Inserts the code length up until this point to implement a label
            label_table.insert(lhs.trim_end_matches(':').to_string(), code_length);
            log::debug!("Symbols table: {:?}", label_table);
            continue;
        }

        let opclass = match_instruction(&lhs)
            .or_else(|err| {
                Err(Error::with_source(ErrorClass::ParseError, format!("at line {}: {}", line_number, err), err))
            })?;

        let addr_mode;
        if let Some(rhs) = instruction_it.next() {
            let rhs = rhs.trim();
            addr_mode = match_addressing_mode(&label_table, code_length, &rhs)
                .or_else(|err| {
                    Err(Error::with_source(ErrorClass::ParseError, format!("at line {}: {}", line_number, err), err))
                })?;
        } else {
            addr_mode = AddressingMode::Implied;
        }

        let instruction = Instruction::new(opclass, addr_mode)
            .or_else(|err| {
                Err(Error::with_source(ErrorClass::ParseError, format!("at line {}: {}", line_number, err), err))
            })?;

        code_buff.append(&mut instruction.encode());
        log::debug!("Parsed instruction: {:?}", instruction);

        code_length += 1 + instruction.addr_mode().length(); // OpCode + address mode
        log::debug!("Code lenght as: {:?}", code_length);
    } // End of for: each line of code

    //Err(Error::new(ErrorClass::ParseError, "failed to assemble code"))
    Ok(code_buff)
}

fn match_addressing_mode<S>(_label_table: &HashMap<String, usize>, _current_pos: usize, param: S) -> EmuResult<AddressingMode>
where
    S: AsRef<str>,
{
    use regex::Regex;
    // Create some regexpr
    lazy_static::lazy_static! {
        static ref IMMEDIATE_REGEX: Regex = Regex::new(
                r"^#(?P<hex>\$)?(?P<val>[0-9a-fA-F]{2})$"
            )
            .unwrap();
        static ref ZERO_PAGE_REGEX: Regex = Regex::new(
                r"^(?P<hex>\$)?(?P<val>[0-9a-fA-F]{2})(?P<index>,X)?$"
            )
            .unwrap();
        static ref ABSOLUTE_REGEX: Regex = Regex::new(
                r"^(?P<hex>\$)?(?P<val>[0-9a-fA-F]{4})(?P<index>,(X|Y))?$"
            )
            .unwrap();
        static ref INDIRECT_REGEX: Regex = Regex::new(
                r"^\((?P<hex>\$)?(?P<val>[0-9a-fA-F]{4})\)$"
            )
            .unwrap();
        static ref INDIRECT_X_REGEX: Regex = Regex::new(
                r"^\((?P<hex>\$)?(?P<val>[0-9a-fA-F]{2}),X\)$"
            )
            .unwrap();
        static ref INDIRECT_Y_REGEX: Regex = Regex::new(
                r"^\((?P<hex>\$)?(?P<val>[0-9a-fA-F]{2})\),Y$"
            )
            .unwrap();
        static ref RELATIVE_WITH_LABEL: Regex = Regex::new(
                r"(?P<val>[_a-zA-Z][_a-zA-Z0-9]*)$"
            )
            .unwrap();
    }

    let param = param.as_ref();

    // When there is no "parameters" to the instruction, its adressing mode is said implied.
    if param.len() == 0 {
        Ok(AddressingMode::Implied)
    } else {
        // IMMEDIATE
        if let Some(captures) = IMMEDIATE_REGEX.captures(param) {
            let radix = if let Some(_) = captures.name("hex") { 16 } else { 10 };
            let val = u8::from_str_radix(captures.name("val").unwrap().as_str(), radix)
                .or(Err(Error::new(ErrorClass::ParseError, format!("invalid numeric representation (radix {})", radix))))?;

            Ok(AddressingMode::Immediate(val))
        // ABSOLUTE
        } else if let Some(captures) = ABSOLUTE_REGEX.captures(param) {
            let radix = if let Some(_) = captures.name("hex") { 16 } else { 10 };
            let val = u16::from_str_radix(captures.name("val").unwrap().as_str(), radix)
                .or(Err(Error::new(ErrorClass::ParseError, format!("invalid numeric representation (radix {})", radix))))?;
            let index = {
                if let Some(text) = captures.name("index") {
                    match text.as_str() {
                        ",X" => Some('X'),
                        ",Y" => Some('Y'),
                        _ => panic!("Invalid match"),
                    }
                } else {
                    None
                }
            };

            match index {
                None => Ok(AddressingMode::Absolute(val)),
                Some('X') => Ok(AddressingMode::AbsoluteX(val)),
                Some('Y') => Ok(AddressingMode::AbsoluteY(val)),
                _ => panic!("Invalid match"),
            }
        // ZERO_PAGE
        } else if let Some(captures) = ZERO_PAGE_REGEX.captures(param) {
            let radix = if let Some(_) = captures.name("hex") { 16 } else { 10 };
            let val = u8::from_str_radix(captures.name("val").unwrap().as_str(), radix)
                .or(Err(Error::new(ErrorClass::ParseError, format!("invalid numeric representation (radix {})", radix))))?;
            let index = captures.name("index").is_some();

            if index {
                Ok(AddressingMode::ZeroPageX(val))
            } else {
                Ok(AddressingMode::ZeroPage(val))
            }
        // INDIRECT NO INDEX
        } else if let Some(captures) = INDIRECT_X_REGEX.captures(param) {
            let radix = if let Some(_) = captures.name("hex") { 16 } else { 10 };
            let val = u16::from_str_radix(captures.name("val").unwrap().as_str(), radix)
                .or(Err(Error::new(ErrorClass::ParseError, format!("invalid numeric representation (radix {})", radix))))?;

            Ok(AddressingMode::Indirect(val))
        // INDIRECT_X
        } else if let Some(captures) = INDIRECT_X_REGEX.captures(param) {
            let radix = if let Some(_) = captures.name("hex") { 16 } else { 10 };
            let val = u8::from_str_radix(captures.name("val").unwrap().as_str(), radix)
                .or(Err(Error::new(ErrorClass::ParseError, format!("invalid numeric representation (radix {})", radix))))?;

            Ok(AddressingMode::IndirectX(val))
        // INDIRECT_Y
        } else if let Some(captures) = INDIRECT_Y_REGEX.captures(param) {
            let radix = if let Some(_) = captures.name("hex") { 16 } else { 10 };
            let val = u8::from_str_radix(captures.name("val").unwrap().as_str(), radix)
                .or(Err(Error::new(ErrorClass::ParseError, format!("invalid numeric representation (radix {})", radix))))?;

            Ok(AddressingMode::IndirectY(val))
        // RELATIVE_WITH_LABEL
        } else if let Some(_captures) = RELATIVE_WITH_LABEL.captures(param) {
            //let val = captures.name("val").unwrap().as_str();
            //let relative_val = if let Some(label_pos) = label_table.get(val) {
            //}

            //Ok(AddressingMode::Relative(relative_val))
            unimplemented!()
        } else {
            Err(Error::new(ErrorClass::ParseError, format!("failed to parse address mode string {}", param)))
        }
    } // End of else: param.len() > 0
} // End of fn match_addressing_mode(..)

// Big function ahead :)
fn match_instruction<S>(opname: S) -> EmuResult<OpClass>
where
    S: AsRef<str>,
{
    match opname.as_ref().to_uppercase().as_str() {
        "ADC" => {
            Ok(OpClass::ADC)
        },
        "AND" => {
            Ok(OpClass::AND)
        },
        "ASL" => {
            Ok(OpClass::ASL)
        },
        "BCC" => {
            Ok(OpClass::BCC)
        },
        "BCS" => {
            Ok(OpClass::BCS)
        },
        "BEQ" => {
            Ok(OpClass::BEQ)
        },
        "BIT" => {
            Ok(OpClass::BIT)
        },
        "BMI" => {
            Ok(OpClass::BMI)
        },
        "BNE" => {
            Ok(OpClass::BNE)
        },
        "BPL" => {
            Ok(OpClass::BPL)
        },
        "BRK" => {
            Ok(OpClass::BRK)
        },
        "BVC" => {
            Ok(OpClass::BVC)
        },
        "BVS" => {
            Ok(OpClass::BVS)
        },
        "CLC" => {
            Ok(OpClass::CLC)
        },
        "CLD" => {
            Ok(OpClass::CLD)
        },
        "CLI" => {
            Ok(OpClass::CLI)
        },
        "CLV" => {
            Ok(OpClass::CLV)
        },
        "CMP" => {
            Ok(OpClass::CMP)
        },
        "CPX" => {
            Ok(OpClass::CPX)
        },
        "CPY" => {
            Ok(OpClass::CPY)
        },
        "DEC" => {
            Ok(OpClass::DEC)
        },
        "DEX" => {
            Ok(OpClass::DEX)
        },
        "DEY" => {
            Ok(OpClass::DEY)
        },
        "EOR" => {
            Ok(OpClass::EOR)
        },
        "INC" => {
            Ok(OpClass::INC)
        },
        "INX" => {
            Ok(OpClass::INX)
        },
        "INY" => {
            Ok(OpClass::INY)
        },
        "JMP" => {
            Ok(OpClass::JMP)
        },
        "JSR" => {
            Ok(OpClass::JSR)
        },
        "LDA" => {
            Ok(OpClass::LDA)
        },
        "LDX" => {
            Ok(OpClass::LDX)
        },
        "LDY" => {
            Ok(OpClass::LDY)
        },
        "LSR" => {
            Ok(OpClass::LSR)
        },
        "NOP" => {
            Ok(OpClass::NOP)
        },
        "ORA" => {
            Ok(OpClass::ORA)
        },
        "PHA" => {
            Ok(OpClass::PHA)
        },
        "PHP" => {
            Ok(OpClass::PHP)
        },
        "PLA" => {
            Ok(OpClass::PLA)
        },
        "PLP" => {
            Ok(OpClass::PLP)
        },
        "ROL" => {
            Ok(OpClass::ROL)
        },
        "ROR" => {
            Ok(OpClass::ROR)
        },
        "RTI" => {
            Ok(OpClass::RTI)
        },
        "RTS" => {
            Ok(OpClass::RTS)
        },
        "SBC" => {
            Ok(OpClass::SBC)
        },
        "SEC" => {
            Ok(OpClass::SEC)
        },
        "SED" => {
            Ok(OpClass::SED)
        },
        "SEI" => {
            Ok(OpClass::SEI)
        },
        "STA" => {
            Ok(OpClass::STA)
        },
        "STX" => {
            Ok(OpClass::STX)
        },
        "STY" => {
            Ok(OpClass::STY)
        },
        "TAX" => {
            Ok(OpClass::TAX)
        },
        "TAY" => {
            Ok(OpClass::TAY)
        },
        "TSX" => {
            Ok(OpClass::TSX)
        },
        "TXA" => {
            Ok(OpClass::TXA)
        },
        "TXS" => {
            Ok(OpClass::TXS)
        },
        "TYA" => {
            Ok(OpClass::TYA)
        },
        s => {
            Err(Error::new(ErrorClass::ParseError, format!("unknown instruction '{}'", s)))
        },
    }
} // End of fn match_instruction(..)
