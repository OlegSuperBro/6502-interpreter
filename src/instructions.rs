#![allow(clippy::upper_case_acronyms)] // idk, uppercase instructions feels better for me

use std::fmt::{Display, format, write};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LoadOp {
    LDA,
    LDX,
    LDY,
    STA,
    STX,
    STY,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegTransOp {
    TAX,
    TAY,
    TXA,
    TYA,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StackOp {
    TSX,
    TXS,
    PHA,
    PHP,
    PLA,
    PLP,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LogicOp {
    AND,
    EOR,
    ORA,
    BIT,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArithmeticOp {
    ADC,
    SBC,
    CMP,
    CPX,
    CPY,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IncDecOp {
    INC,
    INX,
    INY,
    DEC,
    DEX,
    DEY,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ShiftOp {
    ASL,
    LSR,
    ROL,
    ROR,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum JumpCallOp {
    JMP,
    JSR,
    RTS,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BranchOp {
    BCC,
    BCS,
    BEQ,
    BMI,
    BNE,
    BPL,
    BVC,
    BVS,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StatusFlagOp {
    CLC,
    CLD,
    CLI,
    CLV,
    SEC,
    SED,
    SEI,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SystemFuncOp {
    BRK,
    NOP,
    RTI,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OpCode {
    Load(LoadOp),
    RegTrans(RegTransOp),
    Stack(StackOp),
    Logical(LogicOp),
    Arithmetic(ArithmeticOp),
    IncDec(IncDecOp),
    Shift(ShiftOp),
    JumpCall(JumpCallOp),
    Branch(BranchOp),
    StatusFlag(StatusFlagOp),
    SystemFunc(SystemFuncOp),
    Unknown,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line = match *self {
            OpCode::Load(op) =>
                match op {
                    LoadOp::LDA => "LDA",
                    LoadOp::LDX => "LDX",
                    LoadOp::LDY => "LDY",
                    LoadOp::STA => "STA",
                    LoadOp::STX => "STX",
                    LoadOp::STY => "STY",
                },
            OpCode::RegTrans(op) =>
                match op {
                    RegTransOp::TAX => "TAX",
                    RegTransOp::TAY => "TAY",
                    RegTransOp::TXA => "TXA",
                    RegTransOp::TYA => "TYA",
                },
            OpCode::Stack(op) =>
                match op {
                    StackOp::TSX => "TSX",
                    StackOp::TXS => "TXS",
                    StackOp::PHA => "PHA",
                    StackOp::PHP => "PHP",
                    StackOp::PLA => "PLA",
                    StackOp::PLP => "PLP",
                },
            OpCode::Logical(op) =>
                match op {
                    LogicOp::AND => "AND",
                    LogicOp::EOR => "EOR",
                    LogicOp::ORA => "ORA",
                    LogicOp::BIT => "BIT",
                },
            OpCode::Arithmetic(op) =>
                match op {
                    ArithmeticOp::ADC => "ADC",
                    ArithmeticOp::SBC => "SBC",
                    ArithmeticOp::CMP => "CMP",
                    ArithmeticOp::CPX => "CPX",
                    ArithmeticOp::CPY => "CPY",
                },
            OpCode::IncDec(op) =>
                match op {
                    IncDecOp::INC => "INC",
                    IncDecOp::INX => "INX",
                    IncDecOp::INY => "INY",
                    IncDecOp::DEC => "DEC",
                    IncDecOp::DEX => "DEX",
                    IncDecOp::DEY => "DEY",
                },
            OpCode::Shift(op) =>
                match op {
                    ShiftOp::ASL => "ASL",
                    ShiftOp::LSR => "LSR",
                    ShiftOp::ROL => "ROL",
                    ShiftOp::ROR => "ROR",
                },
            OpCode::JumpCall(op) =>
                match op {
                    JumpCallOp::JMP => "JMP",
                    JumpCallOp::JSR => "JSR",
                    JumpCallOp::RTS => "RTS",
                },
            OpCode::Branch(op) =>
                match op {
                    BranchOp::BCC => "BCC",
                    BranchOp::BCS => "BCS",
                    BranchOp::BEQ => "BEQ",
                    BranchOp::BMI => "BMI",
                    BranchOp::BNE => "BNE",
                    BranchOp::BPL => "BPL",
                    BranchOp::BVC => "BVC",
                    BranchOp::BVS => "BVS",
                },
            OpCode::StatusFlag(op) =>
                match op {
                    StatusFlagOp::CLC => "CLC",
                    StatusFlagOp::CLD => "CLD",
                    StatusFlagOp::CLI => "CLI",
                    StatusFlagOp::CLV => "CLV",
                    StatusFlagOp::SEC => "SEC",
                    StatusFlagOp::SED => "SED",
                    StatusFlagOp::SEI => "SEI",
                },
            OpCode::SystemFunc(op) =>
                match op {
                    SystemFuncOp::BRK => "BRK",
                    SystemFuncOp::NOP => "NOP",
                    SystemFuncOp::RTI => "RTI",
                },
            OpCode::Unknown => "???",
        };
        f.write_str(line)
    }
}


#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    Implicit,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub opcode: OpCode,
    pub addressing_mode: AddressingMode,
    pub value: u16
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.opcode, value_to_string(self.addressing_mode, self.value))
    }
}

fn value_to_string(mode: AddressingMode, value: u16) -> String {
    match mode {
        AddressingMode::Implicit |
        AddressingMode::Unknown => "".into(),

        AddressingMode::Accumulator => "A".into(),

        AddressingMode::Immediate => format!("#${:x}", value),
        AddressingMode::ZeroPage => format!("${:02x}", value),
        AddressingMode::ZeroPageX => format!("${:02x},X", value),
        AddressingMode::ZeroPageY => format!("${:02x},Y", value),
        AddressingMode::Relative => format!("*+{}", value),
        
        AddressingMode::Indirect => format!("(${:04x})", value),
        AddressingMode::IndexedIndirect => format!("(${},X)", value),
        AddressingMode::IndirectIndexed => format!("(${}),Y", value),

        AddressingMode::Absolute => format!("${:04x}", value),
        AddressingMode::AbsoluteX => format!("${:04x},X", value),
        AddressingMode::AbsoluteY => format!("${:04x},Y", value),
    }
}
