use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum ParseError {
    InvalidOpCodeGroup(u8),
    InvalidAddressingMode(u8),
    InvalidOpCode(&'static str, u8)
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidOpCodeGroup(byte) => write!(f, "Invalid OpCode group: {:b}", byte),
            ParseError::InvalidAddressingMode(byte) => write!(f, "Invalid addressing mode: {:b}", byte),
            ParseError::InvalidOpCode(label, byte) => write!(f, "Invalid OpCode. Label: {} Value: {}", label, byte),

        }
    }
}

impl Error for ParseError {}
