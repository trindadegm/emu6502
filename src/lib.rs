pub mod assembler;
pub use assembler::*;

pub mod code;

pub mod emulator;
pub use emulator::*;

pub mod error;

pub type EmuResult<T> = Result<T, error::Error>;
