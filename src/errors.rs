use std::{error::Error, fmt::Display};

#[derive(Debug)]
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
    InvalidAddressingMode
}

impl Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionError::InvalidAddressingMode => write!(f, "Invalid addressing mode"),
        }
    }
}

impl Error for ExecutionError {}