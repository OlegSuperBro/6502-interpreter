use std::{error::Error, fmt::{Debug, Display}};

#[derive(Debug)]
pub enum CommandError {
    InvalidCommand,
    InvalidArgument,
    FailedToExecute(Box<dyn Error>),
    InvalidRange,
}

impl Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for CommandError {}

#[derive(Debug)]
pub enum HotkeyError {
    InvalidHotkey,
}

impl Display for HotkeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for HotkeyError {}

pub enum TracerError {
    CommandError(CommandError),
    HotkeyError(HotkeyError),

    IOError(std::io::Error),

    TerminalError(&'static str),
}

impl Display for TracerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for TracerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TracerError::TerminalError(x) => write!(f, "Terminal Error: {}", x),
            TracerError::CommandError(x) => write!(f, "Command Error: {}", x),
            TracerError::HotkeyError(x) => write!(f, "Hotkey Error: {}", x),
            TracerError::IOError(x) => write!(f, "IO Error: {}", x),
        }
    }
}

impl Error for TracerError {}
