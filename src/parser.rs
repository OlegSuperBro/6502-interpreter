use std::error::Error;

use crate::{errors::ParseError, instructions::{AddressingMode, ArithmeticOp, BranchOp, IncDecOp, Instruction, JumpCallOp, LoadOp, LogicOp, OpCode, RegTransOp, ShiftOp, StackOp, StatusFlagOp, SystemFuncOp}};


pub fn parse_instruction(addr: u16, data: &[u8]) -> Result<(usize, Instruction), Box<dyn Error>>  {
    let mut result_offset = 1;

    let instruction = data[addr as usize];

    let opcode_group = parse_group(&instruction)?;
    let opcode = parse_opcode(&instruction, &opcode_group)?;

    let addressing_mode = parse_address_mode(&instruction, &opcode, &opcode_group)?;

    let mut value: u16 = 0;

    match addressing_mode {
        // Read 0 more bytes
        AddressingMode::Implicit |
        AddressingMode::Accumulator => {}

        // Read 1 more byte
        AddressingMode::Immediate |
        AddressingMode::ZeroPage |
        AddressingMode::ZeroPageX |
        AddressingMode::ZeroPageY |
        AddressingMode::IndexedIndirect |
        AddressingMode::IndirectIndexed |
        AddressingMode::Relative => {
            result_offset += 1;
            value = data[(addr + 1) as usize] as u16;
        }

        // Read 2 more bytes
        AddressingMode::Absolute |
        AddressingMode::AbsoluteX |
        AddressingMode::AbsoluteY |
        AddressingMode::Indirect => {
            result_offset += 1;
            value = data[(addr + 1) as usize] as u16;
            result_offset += 1;
            value |= (data[(addr + 2) as usize] as u16) << 8;
        }

        _ => {
            todo!("addressing mode {addressing_mode:?}")
        }
    }

    Ok((result_offset, Instruction { opcode, addressing_mode, value }))
}

#[derive(Debug, PartialEq)]
enum OpCodeGroup {
    Group1 = 0b01,
    Group2 = 0b10,
    Group3 = 0b00,

    // those groups are handled separatly
    BranchGroup = 0b0100,
    OtherGroup
}

impl TryFrom<u8> for OpCodeGroup {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == OpCodeGroup::Group1 as u8 => Ok(OpCodeGroup::Group1),
            x if x == OpCodeGroup::Group2 as u8 => Ok(OpCodeGroup::Group2),
            x if x == OpCodeGroup::Group3 as u8 => Ok(OpCodeGroup::Group3),

            _ => Err(ParseError::InvalidOpCodeGroup(value)),
        }
    }
}

fn parse_group(byte: &u8) -> Result<OpCodeGroup, ParseError> {
    let group = byte & 0b00000011;

    if (byte & 0b00010000 == 0b00010000) && (byte & 0b00001111 == 0) {
        return Ok(OpCodeGroup::BranchGroup);
    } else if parse_other_opcode(byte).is_some(){
        return Ok(OpCodeGroup::OtherGroup)
    } else if let Ok(opcode_group) = OpCodeGroup::try_from(group) {
        return Ok(opcode_group);
    }

    Err(ParseError::InvalidOpCodeGroup(*byte))
}

fn parse_opcode(byte: &u8, group: &OpCodeGroup) -> Result<OpCode, ParseError> {
    let opcode_byte = (byte & 0b11100000) >> 5;

    // TODO should be refactored, but i need to check, how those command are distinct from others
    if let Some(opcode) = parse_other_opcode(byte) {
        return Ok(opcode)
    }

    match group {
        OpCodeGroup::Group1 =>
            match opcode_byte {
                0b000 => Ok(OpCode::Logical(LogicOp::ORA)),
                0b001 => Ok(OpCode::Logical(LogicOp::AND)),
                0b010 => Ok(OpCode::Logical(LogicOp::EOR)),
                0b011 => Ok(OpCode::Arithmetic(ArithmeticOp::ADC)),
                0b100 => Ok(OpCode::Load(LoadOp::STA)),
                0b101 => Ok(OpCode::Load(LoadOp::LDA)),
                0b110 => Ok(OpCode::Arithmetic(ArithmeticOp::CMP)),
                0b111 => Ok(OpCode::Arithmetic(ArithmeticOp::SBC)),
                _ => Err(ParseError::InvalidByteOpCode(opcode_byte))
            }
        OpCodeGroup::Group2 =>
            match opcode_byte {
                0b000 => Ok(OpCode::Shift(ShiftOp::ASL)),
                0b001 => Ok(OpCode::Shift(ShiftOp::ROL)),
                0b010 => Ok(OpCode::Shift(ShiftOp::LSR)),
                0b011 => Ok(OpCode::Shift(ShiftOp::ROR)),
                0b100 => Ok(OpCode::Load(LoadOp::STX)),
                0b101 => Ok(OpCode::Load(LoadOp::LDX)),
                0b110 => Ok(OpCode::IncDec(IncDecOp::DEC)),
                0b111 => Ok(OpCode::IncDec(IncDecOp::INC)),
                _ => Err(ParseError::InvalidByteOpCode(opcode_byte))
            }
        OpCodeGroup::Group3 =>
            match opcode_byte {
                0b001 => Ok(OpCode::Logical(LogicOp::BIT)),

                // Addressing mode for both jumps should be handled manually
                0b010 => Ok(OpCode::JumpCall(JumpCallOp::JMP)), // this JMP is indirect
                0b011 => Ok(OpCode::JumpCall(JumpCallOp::JMP)), // this JMP is absolute

                0b100 => Ok(OpCode::Load(LoadOp::STY)),
                0b101 => Ok(OpCode::Load(LoadOp::LDY)),
                0b110 => Ok(OpCode::Arithmetic(ArithmeticOp::CPY)),
                0b111 => Ok(OpCode::Arithmetic(ArithmeticOp::CPX)),
                _ => Err(ParseError::InvalidByteOpCode(opcode_byte))
            }
        OpCodeGroup::BranchGroup => {
            match byte {
                0x10 => Ok(OpCode::Branch(BranchOp::BPL)),
                0x30 => Ok(OpCode::Branch(BranchOp::BMI)),
                0x50 => Ok(OpCode::Branch(BranchOp::BVC)),
                0x70 => Ok(OpCode::Branch(BranchOp::BVS)),
                0x90 => Ok(OpCode::Branch(BranchOp::BCC)),
                0xB0 => Ok(OpCode::Branch(BranchOp::BCS)),
                0xD0 => Ok(OpCode::Branch(BranchOp::BNE)),
                0xF0 => Ok(OpCode::Branch(BranchOp::BEQ)),

                _ => Err(ParseError::InvalidOpCode("Branch", *byte))
            }
        }

        OpCodeGroup::OtherGroup => Err(ParseError::InvalidOpCode("OtherUnreachable", *byte)) // before this match statement, we did handled "other" instructions
    }
}

fn parse_other_opcode(byte: &u8) -> Option<OpCode> {
    match byte {
        0x00 => Some(OpCode::SystemFunc(SystemFuncOp::BRK)),
        0x20 => Some(OpCode::JumpCall(JumpCallOp::JSR)),
        0x40 => Some(OpCode::SystemFunc(SystemFuncOp::RTI)),
        0x60 => Some(OpCode::JumpCall(JumpCallOp::RTS)),

        0x08 => Some(OpCode::Stack(StackOp::PHP)),
        0x28 => Some(OpCode::Stack(StackOp::PLP)),
        0x48 => Some(OpCode::Stack(StackOp::PHA)),
        0x68 => Some(OpCode::Stack(StackOp::PLA)),
        0x88 => Some(OpCode::IncDec(IncDecOp::DEY)),
        0xA8 => Some(OpCode::RegTrans(RegTransOp::TAY)),
        0xC8 => Some(OpCode::IncDec(IncDecOp::INY)),
        0xE8 => Some(OpCode::IncDec(IncDecOp::INX)),

        0x18 => Some(OpCode::StatusFlag(StatusFlagOp::CLC)),
        0x38 => Some(OpCode::StatusFlag(StatusFlagOp::SEC)),
        0x58 => Some(OpCode::StatusFlag(StatusFlagOp::CLI)),
        0x78 => Some(OpCode::StatusFlag(StatusFlagOp::SEI)),
        0x98 => Some(OpCode::RegTrans(RegTransOp::TYA)),
        0xB8 => Some(OpCode::StatusFlag(StatusFlagOp::CLV)),
        0xD8 => Some(OpCode::StatusFlag(StatusFlagOp::CLD)),
        0xF8 => Some(OpCode::StatusFlag(StatusFlagOp::SED)),

        0x8A => Some(OpCode::RegTrans(RegTransOp::TXA)),
        0x9A => Some(OpCode::Stack(StackOp::TXS)),
        0xAA => Some(OpCode::RegTrans(RegTransOp::TAX)),
        0xBA => Some(OpCode::Stack(StackOp::TSX)),
        0xCA => Some(OpCode::IncDec(IncDecOp::DEX)),
        0xEA => Some(OpCode::SystemFunc(SystemFuncOp::NOP)),

        _ => None
    }
}

fn parse_address_mode(byte: &u8, opcode: &OpCode, group: &OpCodeGroup) -> Result<AddressingMode, ParseError> {
    // don't ask. i have no clue why this exist.
    if opcode == &OpCode::JumpCall(JumpCallOp::JMP) {
        let masked_byte = (byte & 0b11100000) >> 5;

        return match masked_byte {
            0b010 => Ok(AddressingMode::Absolute),
            0b011 => Ok(AddressingMode::Indirect),

            _ => todo!("address_mode jmp handler How in tf you got here? {0:#010b}, {1:#010b}", byte, masked_byte)
        };
    }

    let opcode_mode = (byte & 0b00011100) >> 2;

    match group {
        OpCodeGroup::Group1 =>
            match opcode_mode {
                0b000 => Ok(AddressingMode::IndexedIndirect),
                0b001 => Ok(AddressingMode::ZeroPage),
                0b010 => Ok(AddressingMode::Immediate),
                0b011 => Ok(AddressingMode::Absolute),
                0b100 => Ok(AddressingMode::IndirectIndexed),
                0b101 => Ok(AddressingMode::ZeroPageX),
                0b110 => Ok(AddressingMode::AbsoluteY),
                0b111 => Ok(AddressingMode::AbsoluteX),
                _ => Err(ParseError::InvalidAddressingMode(*byte)),
            }

        OpCodeGroup::Group2 =>
            match opcode_mode {
                0b000 => Ok(AddressingMode::Immediate),
                0b001 => Ok(AddressingMode::ZeroPage),
                0b010 => Ok(AddressingMode::Accumulator),
                0b011 => Ok(AddressingMode::Absolute),
                0b101 => {
                    if opcode == &OpCode::Load(LoadOp::STX) ||
                       opcode == &OpCode::Load(LoadOp::LDX) {
                        Ok(AddressingMode::ZeroPageY)
                    } else {
                        Ok(AddressingMode::ZeroPageX)
                    }
                },
                0b111 => {
                    if opcode == &OpCode::Load(LoadOp::LDX) {
                        Ok(AddressingMode::AbsoluteY)
                    } else {
                        Ok(AddressingMode::AbsoluteX)
                    }
                },
                _ => Err(ParseError::InvalidAddressingMode(*byte)),

            }
        OpCodeGroup::Group3 =>
            match opcode_mode {
                0b000 => Ok(AddressingMode::Immediate),
                0b001 => Ok(AddressingMode::ZeroPage),
                0b011 => Ok(AddressingMode::Absolute),
                0b101 => Ok(AddressingMode::ZeroPageX),
                0b111 => Ok(AddressingMode::AbsoluteX),
                _ => Err(ParseError::InvalidAddressingMode(*byte)),
            }
        OpCodeGroup::BranchGroup => Ok(AddressingMode::Relative), // all branch instructions realative only

        OpCodeGroup::OtherGroup => {
            match opcode {
                OpCode::JumpCall(JumpCallOp::JSR) => Ok(AddressingMode::Absolute),
                _ => Ok(AddressingMode::Implicit)
            }
        }
    }
}