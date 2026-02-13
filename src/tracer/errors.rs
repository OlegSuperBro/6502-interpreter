use std::{
    error::Error,
    fmt::{Debug, Display},
};

#[derive(Debug)]
pub enum CommandError {
    InvalidCommand,
    InvalidArgument,
    FailedToExecute(Box<dyn Error>),
    InvalidRange,
}

impl Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::FailedToExecute(err) => write!(f, "{self:?}: {err:?}"),
            _ => write!(f, "{self:?}")
        }
    }
}

impl Error for CommandError {}

#[derive(Debug)]
pub enum HotkeyError {
    InvalidHotkey,
}

impl Display for HotkeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for HotkeyError {}

pub enum TracerError {
    Command(CommandError),
    Hotkey(HotkeyError),

    IO(std::io::Error),

    Terminal(&'static str),
}

impl Display for TracerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Debug for TracerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TracerError::Terminal(x) => write!(f, "Terminal Error: {x}"),
            TracerError::Command(x) => write!(f, "Command Error: {x}"),
            TracerError::Hotkey(x) => write!(f, "Hotkey Error: {x}"),
            TracerError::IO(x) => write!(f, "IO Error: {x}"),
        }
    }
}

impl Error for TracerError {}
