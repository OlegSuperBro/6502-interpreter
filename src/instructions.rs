#![allow(clippy::upper_case_acronyms)] // idk, uppercase instructions feels better for me

use std::{error::Error, fmt::Display};

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

impl<T: AsRef<str>> From<T> for OpCode {
    fn from(value: T) -> Self {
        match value.as_ref() {
                "LDA" => OpCode::Load(LoadOp::LDA),
                "LDX" => OpCode::Load(LoadOp::LDX),
                "LDY" => OpCode::Load(LoadOp::LDY),
                "STA" => OpCode::Load(LoadOp::STA),
                "STX" => OpCode::Load(LoadOp::STX),
                "STY" => OpCode::Load(LoadOp::STY),

                "TAX" => OpCode::RegTrans(RegTransOp::TAX),
                "TAY" => OpCode::RegTrans(RegTransOp::TAY),
                "TXA" => OpCode::RegTrans(RegTransOp::TXA),
                "TYA" => OpCode::RegTrans(RegTransOp::TYA),

                "TSX" => OpCode::Stack(StackOp::TSX),
                "TXS" => OpCode::Stack(StackOp::TXS),
                "PHA" => OpCode::Stack(StackOp::PHA),
                "PHP" => OpCode::Stack(StackOp::PHP),
                "PLA" => OpCode::Stack(StackOp::PLA),
                "PLP" => OpCode::Stack(StackOp::PLP),

                "AND" => OpCode::Logical(LogicOp::AND),
                "EOR" => OpCode::Logical(LogicOp::EOR),
                "ORA" => OpCode::Logical(LogicOp::ORA),
                "BIT" => OpCode::Logical(LogicOp::BIT),

                "ADC" => OpCode::Arithmetic(ArithmeticOp::ADC),
                "SBC" => OpCode::Arithmetic(ArithmeticOp::SBC),
                "CMP" => OpCode::Arithmetic(ArithmeticOp::CMP),
                "CPX" => OpCode::Arithmetic(ArithmeticOp::CPX),
                "CPY" => OpCode::Arithmetic(ArithmeticOp::CPY),

                "INC" => OpCode::IncDec(IncDecOp::INC),
                "INX" => OpCode::IncDec(IncDecOp::INX),
                "INY" => OpCode::IncDec(IncDecOp::INY),
                "DEC" => OpCode::IncDec(IncDecOp::DEC),
                "DEX" => OpCode::IncDec(IncDecOp::DEX),
                "DEY" => OpCode::IncDec(IncDecOp::DEY),

                "ASL" => OpCode::Shift(ShiftOp::ASL),
                "LSR" => OpCode::Shift(ShiftOp::LSR),
                "ROL" => OpCode::Shift(ShiftOp::ROL),
                "ROR" => OpCode::Shift(ShiftOp::ROR),

                "JMP" => OpCode::JumpCall(JumpCallOp::JMP),
                "JSR" => OpCode::JumpCall(JumpCallOp::JSR),
                "RTS" => OpCode::JumpCall(JumpCallOp::RTS),

                "BCC" => OpCode::Branch(BranchOp::BCC),
                "BCS" => OpCode::Branch(BranchOp::BCS),
                "BEQ" => OpCode::Branch(BranchOp::BEQ),
                "BMI" => OpCode::Branch(BranchOp::BMI),
                "BNE" => OpCode::Branch(BranchOp::BNE),
                "BPL" => OpCode::Branch(BranchOp::BPL),
                "BVC" => OpCode::Branch(BranchOp::BVC),
                "BVS" => OpCode::Branch(BranchOp::BVS),

                "CLC" => OpCode::StatusFlag(StatusFlagOp::CLC),
                "CLD" => OpCode::StatusFlag(StatusFlagOp::CLD),
                "CLI" => OpCode::StatusFlag(StatusFlagOp::CLI),
                "CLV" => OpCode::StatusFlag(StatusFlagOp::CLV),
                "SEC" => OpCode::StatusFlag(StatusFlagOp::SEC),
                "SED" => OpCode::StatusFlag(StatusFlagOp::SED),
                "SEI" => OpCode::StatusFlag(StatusFlagOp::SEI),

                "BRK" => OpCode::SystemFunc(SystemFuncOp::BRK),
                "NOP" => OpCode::SystemFunc(SystemFuncOp::NOP),
                "RTI" => OpCode::SystemFunc(SystemFuncOp::RTI),

                _ => OpCode::Unknown,
            }
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

impl AddressingMode {
    pub fn check_addr_mode<T: AsRef<str>>(string: T) -> Result<Self, Box<dyn Error>> {
        let val = string.as_ref().replace("$", "");

        if val == "A" {
            return Ok(AddressingMode::Accumulator);
        } else if val.starts_with("#") {
            return Ok(Self::Immediate);
        } else if val.len() < 4 {
            if val.starts_with("#") {
                return Ok(Self::Immediate);
            } else if val.len() > 3 {
                return Ok(Self::ZeroPage);
            } else {
                return Ok(Self::Absolute);
            }
        } else if let Some(left) = val.split(",").collect::<Vec<&str>>().first() {
            if left.len() > 2 {
                if val.ends_with(",X") {
                    return Ok(Self::ZeroPageX);
                } else if val.ends_with(",Y") {
                    return Ok(Self::ZeroPageY);
                }
            } else if val.starts_with("*+") {
                return Ok(Self::Relative);
            } else if val.starts_with("(") && val.ends_with(",X)") {
                return Ok(Self::IndexedIndirect);
            } else if val.starts_with("(") && val.ends_with("),Y") {
                return Ok(Self::IndirectIndexed);
            } else if val.starts_with("(") && val.ends_with(")") {
                return Ok(Self::Indirect)
            } else if val.ends_with(",X"){
                return Ok(Self::AbsoluteX);
            } else if val.ends_with(",Y"){
                return Ok(Self::AbsoluteY);
            }
        } else {
            todo!("How in tf you got this?")
        }

        Ok(AddressingMode::Unknown)
    }
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

impl TryFrom<&str> for Instruction {
    type Error = Box<dyn Error>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let values: Vec<&str> = value.split(" ").filter(|x| !x.is_empty()).collect();

        let opcode: OpCode = OpCode::from(&values.first().unwrap());

        if values.len() == 2 {
            let addressing_mode = AddressingMode::check_addr_mode(values[1])?;
            let value = parse_value(addressing_mode, values[1]);

            Ok(Instruction { opcode, addressing_mode, value })
        } else {
            Ok(Instruction { opcode, addressing_mode: AddressingMode::Implicit, value: 0 })
        }

    }
}

impl Instruction {
    pub fn get_code_size(&self) -> usize {
         match self.addressing_mode {
                // Read 0 more bytes
                AddressingMode::Implicit |
                AddressingMode::Accumulator => 1,

                // Read 1 more byte
                AddressingMode::Immediate |
                AddressingMode::ZeroPage |
                AddressingMode::ZeroPageX |
                AddressingMode::ZeroPageY |
                AddressingMode::IndexedIndirect |
                AddressingMode::IndirectIndexed |
                AddressingMode::Relative => 2,

                // Read 2 more bytes
                AddressingMode::Absolute |
                AddressingMode::AbsoluteX |
                AddressingMode::AbsoluteY |
                AddressingMode::Indirect => 3,

                AddressingMode::Unknown => 1,
        }
    }
}

fn value_to_string(mode: AddressingMode, value: u16) -> String {
    match mode {
        AddressingMode::Implicit |
        AddressingMode::Unknown => "".into(),

        AddressingMode::Accumulator => "A".into(),

        AddressingMode::Immediate => format!("#${value:02x}"),
        AddressingMode::ZeroPage => format!("${value:02x}"),
        AddressingMode::ZeroPageX => format!("${value:02x},X"),
        AddressingMode::ZeroPageY => format!("${value:02x},Y"),
        AddressingMode::Relative => if value as i8 > 0 {format!("*+{}", value as i8)} else {format!("*{}", value as i8)},

        AddressingMode::Indirect => format!("(${value:04x})"),
        AddressingMode::IndexedIndirect => format!("(${value:04x},X)"),
        AddressingMode::IndirectIndexed => format!("(${value:04x}),Y"),

        AddressingMode::Absolute => format!("${value:04x}"),
        AddressingMode::AbsoluteX => format!("${value:04x},X"),
        AddressingMode::AbsoluteY => format!("${value:04x},Y"),
    }
}

fn parse_value<T: AsRef<str>>(addressing_mode: AddressingMode, string: T) -> u16 {
    let val = string.as_ref().to_string();

    match addressing_mode {
        AddressingMode::Implicit |
        AddressingMode::Accumulator => 0,

        AddressingMode::Immediate => process_value(val.replace("#", "")),
        AddressingMode::ZeroPage => process_value(val),
        AddressingMode::ZeroPageX => process_value(val.replace(",X", "")),
        AddressingMode::ZeroPageY => process_value(val.replace(",Y", "")),
        AddressingMode::Relative => process_value(val.replace("*+", "")),

        AddressingMode::Indirect => process_value(val.replace("(", "").replace(")", "")),
        AddressingMode::IndexedIndirect => process_value(val.replace("(", "").replace(",X)", "")),
        AddressingMode::IndirectIndexed => process_value(val.replace("(", "").replace("),Y", "")),

        AddressingMode::Absolute => process_value(val),
        AddressingMode::AbsoluteX => process_value(val.replace(",X", "")),
        AddressingMode::AbsoluteY =>  process_value(val.replace(",Y", "")),

        AddressingMode::Unknown => 0,
    }
}

fn process_value<T: AsRef<str>>(value: T) -> u16 {
    let value = value.as_ref();

    if let Some(value) = value.strip_prefix("$") {
        u16::from_str_radix(value, 16).unwrap()
    } else {
        value.parse::<u16>().unwrap()
    }
}
