use std::{error::Error, fmt::Display};

#[derive(Debug)]
#[allow(clippy::enum_variant_names)] // there should be other errors by for now there's only "invalid" errors
pub enum ParseError {
    InvalidOpCodeGroup(u8),
    InvalidAddressingMode(u8),
    InvalidOpCode(&'static str, u8),
    InvalidByteOpCode(u8),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidOpCodeGroup(byte) => write!(f, "Invalid OpCode group: {byte:b}"),
            ParseError::InvalidAddressingMode(byte) => write!(f, "Invalid addressing mode: {byte:b}"),
            ParseError::InvalidOpCode(label, byte) => write!(f, "Invalid OpCode. Label: {label} Value: {byte}"),
            ParseError::InvalidByteOpCode(byte) => write!(f, "Invalid byte for Opcode. Byte: {byte}"),
        }
    }
}

impl Error for ParseError {}


#[derive(Debug)]
pub enum ExecutionError {
    InvalidAddressingMode,
    InvalidBcdValue(u8)
}

impl Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionError::InvalidAddressingMode => write!(f, "Invalid addressing mode"),
            ExecutionError::InvalidBcdValue(x) => write!(f, "Invalid BCD value: {x:#04X}")
        }
    }
}

impl Error for ExecutionError {}