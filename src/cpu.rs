use std::{error::Error, fmt::{Debug, Display}};
use bitflags::{bitflags};

use crate::{
    errors::ExecutionError,
    instructions::{AddressingMode, Instruction, OpCode},
    parser::parse_instruction,
};



bitflags! {
    #[derive(Debug,Clone, Copy)]
    pub struct ProcessorStatus: u8 {
        const CarryFlag = 1 << 0;
        const ZeroFlag = 1 << 1;
        const InterruptDisable = 1 << 2;
        const DecimalMode = 1 << 3;
        const BreakCommand = 1 << 4;

        const OverflowFlag = 1 << 6;
        const NegativeFlag = 1 << 7;
    }
}

impl Display for ProcessorStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "N: {} V: {} B: {} D: {} I: {} Z: {} C: {} \n({:#010b} - {})\nWith B:\n({:#010b} - {})",
            self.contains(ProcessorStatus::NegativeFlag),
            self.contains(ProcessorStatus::OverflowFlag),
            self.contains(ProcessorStatus::BreakCommand),
            self.contains(ProcessorStatus::DecimalMode),
            self.contains(ProcessorStatus::InterruptDisable),
            self.contains(ProcessorStatus::ZeroFlag),
            self.contains(ProcessorStatus::CarryFlag),
            self.bits() | 1 << 5,
            self.bits() | 1 << 5,
            self.bits() | 1 << 5 | ProcessorStatus::BreakCommand.bits(),
            self.bits() | 1 << 5 | ProcessorStatus::BreakCommand.bits(),
        )
    }
}

pub struct Registers {
    pub program_counter: u16,
    pub stack_pointer: u8,

    pub accumulator: u8,
    pub x: u8,
    pub y: u8,

    pub processor_status: ProcessorStatus
}

impl Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
        format_args!("PC: {:#X} SP: {:#X} \nA: {} ({}) X: {} ({}) Y: {} ({})\n{}",
            self.program_counter,
            self.stack_pointer,
            self.accumulator, self.accumulator as i8,
            self.x, self.x as i8,
            self.y, self.y as i8,
            self.processor_status))
    }
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU {
    pub memory: [u8; u16::MAX as usize + 1],
    pub registers: Registers,
}

impl Default for CPU {
    fn default() -> Self {
        CPU {
            memory: [0; u16::MAX as usize + 1],
            registers: Registers { program_counter: 0xFFFC, stack_pointer: 0x0, accumulator: 0x0, x: 0x0, y: 0x0, processor_status: ProcessorStatus::empty() }
        }
    }
}

pub trait ProcessOpcode<T> {
    fn process(&mut self, instruction: Instruction, op: T) -> Result<bool, Box<dyn Error>>;
}


impl CPU {
    pub fn run_instruction(&mut self, instruction: Instruction) -> Result<bool, Box<dyn Error>> {
        match instruction.opcode {
            OpCode::Load(op) => {
                self.process(instruction, op)
            }
            OpCode::Arithmetic(op) => {
                self.process(instruction, op)
            }
            OpCode::JumpCall(op) => {
                self.process(instruction, op)
            }
            OpCode::Stack(op) => {
                self.process(instruction, op)
            }
            OpCode::SystemFunc(op) => {
                self.process(instruction, op)
            }
            OpCode::StatusFlag(op) => {
                self.process(instruction, op)
            }
            OpCode::Branch(op) => {
                self.process(instruction, op)
            }
            OpCode::IncDec(op) => {
                self.process(instruction, op)
            }
            OpCode::RegTrans(op) => {
                self.process(instruction, op)
            }
            OpCode::Logical(op) => {
                self.process(instruction, op)
            }
            OpCode::Shift(op) => {
                self.process(instruction, op)
            }
            _ => {
                todo!("WIP2, {instruction:?}")
            }
        }
    }

    pub fn get_address(&self, mode: AddressingMode, value: u16) -> Result<u16, ExecutionError> {
        match mode {
            AddressingMode::ZeroPage => Ok(value),
            AddressingMode::ZeroPageX => Ok(self.registers.x.wrapping_add(value as u8) as u16),
            AddressingMode::ZeroPageY => Ok(self.registers.y.wrapping_add(value as u8) as u16),
            AddressingMode::Relative => {
                let value = value as i8; // cuz 2 should increment during processing this instruction

                if value < 0 {
                    Ok(self.registers.program_counter.wrapping_add(2).wrapping_sub(value.wrapping_abs() as u16))
                } else {
                    Ok(self.registers.program_counter.wrapping_add(2).wrapping_add(value as u16))
                }
            },
            AddressingMode::Absolute => Ok(value),
            AddressingMode::AbsoluteX => Ok(value.wrapping_add(self.registers.x as u16)),
            AddressingMode::AbsoluteY => Ok(value.wrapping_add(self.registers.y as u16)),
            AddressingMode::Indirect => {
                let lsb = self.memory[value as usize];
                let msb = self.memory[(value.wrapping_add(1)) as usize];

                Ok(((msb as u16) << 8) + (lsb as u16))
            }
            AddressingMode::IndexedIndirect => {
                let lsb = self.memory[((value as u8).wrapping_add(self.registers.x)) as usize];
                let msb = self.memory[((value as u8).wrapping_add(self.registers.x).wrapping_add(1)) as usize];


                Ok(((msb as u16) << 8) + (lsb as u16))
            }
            AddressingMode::IndirectIndexed => {
                let lsb = self.memory[(value) as usize];
                let msb = self.memory[(value.wrapping_add(1)) as usize];

                Ok((((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.registers.y as u16))
            }

            _ => Err(ExecutionError::InvalidAddressingMode)
        }
    }

    pub fn get_value(&self, mode: AddressingMode, value: u16) -> Result<u8, Box<dyn Error>> {
        if let Ok(addr) = self.get_address(mode, value) {
            Ok(self.memory[addr as usize])
        } else {
            match mode {
                AddressingMode::Accumulator => Ok(self.registers.accumulator),
                AddressingMode::Immediate => Ok(value as u8),

                _ => unimplemented!("Failed to get address and {mode:?} is not meant to get value")
            }
        }
    }

    pub fn set_value(&mut self, mode: AddressingMode, inst_value: u16, value: u8) -> Result<(), Box<dyn Error>> {
        match mode {
            AddressingMode::Absolute |
            AddressingMode::ZeroPage => {
                self.memory[inst_value as usize] = value;
            }

            AddressingMode::Accumulator => {
                self.registers.accumulator = value;
            }

            AddressingMode::ZeroPageX => {
                self.memory[(inst_value as u8).wrapping_add(self.registers.x) as usize] = value;
            }

            AddressingMode::ZeroPageY => {
                self.memory[(inst_value as u8).wrapping_add(self.registers.y) as usize] = value;
            }

            AddressingMode::AbsoluteX => {
                self.memory[(inst_value.wrapping_add(self.registers.x as u16)) as usize] = value;
            }

            AddressingMode::AbsoluteY => {
                self.memory[(inst_value.wrapping_add(self.registers.y as u16)) as usize] = value;
            }

            AddressingMode::IndexedIndirect => {
                let lsb = self.memory[((inst_value as u8).wrapping_add(self.registers.x)) as usize];
                let msb = self.memory[((inst_value as u8).wrapping_add(self.registers.x).wrapping_add(1)) as usize];

                let address = ((msb as u16) << 8) + (lsb as u16);

                self.memory[address as usize] = value
            }

            AddressingMode::IndirectIndexed => {
                let lsb = self.memory[inst_value as u8 as usize];
                let msb = self.memory[(inst_value as u8).wrapping_add(1) as usize];

                let address = (((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.registers.y as u16);

                self.memory[address as usize] = value;
            }

            _ => todo!("{:#06x} set_value {mode:?}", self.registers.program_counter),
        }

        Ok(())
    }

    pub fn set_flags(&mut self, flags: ProcessorStatus, mask: ProcessorStatus) {
        self.registers.processor_status = (self.registers.processor_status & !mask) | flags;
    }

    pub fn get_flags(prev_value: Option<u8>, operand: Option<u8>, result_value: &u8, mask: Option<ProcessorStatus>) -> ProcessorStatus {
        let mut result_flag = ProcessorStatus::empty();

        if let Some(prev_value) = prev_value {
            // result_flag |= if prev_value & 0b10000000 != result_value & 0b10000000 {ProcessorStatus::OverflowFlag} else {ProcessorStatus::empty()};

            if let Some(operand) = operand {
                result_flag |= if !(prev_value ^ operand) & (prev_value ^ result_value) & 0b10000000 != 0 {ProcessorStatus::OverflowFlag} else {ProcessorStatus::empty()};
            }
        }

        if *result_value == 0 {
            result_flag |= ProcessorStatus::ZeroFlag;
        }

        if result_value & 0b10000000 != 0 {
            result_flag |= ProcessorStatus::NegativeFlag
        }

        // Interupt Disable flag is ignored cuz it's set by specific instructions
        // Decimal Mode flag is ignored cuz it's set by specific instructions
        // Break Command flag is ignored cuz it's set by specific instructions

        result_flag & mask.unwrap_or(ProcessorStatus::all())
    }

    pub fn stack_push(&mut self, value: u8) {
        self.memory[0x100 + self.registers.stack_pointer as usize] = value;

        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
    }

    pub fn stack_pull(&mut self) -> u8 {
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);

        let address = 0x100 + self.registers.stack_pointer as u16;

        self.memory[address as usize]
    }

    pub fn reset(&mut self) {
        self.registers.program_counter = (self.memory[0xFFFD] as u16) << 8 | self.memory[0xFFFC] as u16;
    }

    pub fn once(&mut self) -> Result<(), Box<dyn Error>> {
        let (offset, instruction) = parse_instruction(self.registers.program_counter, &self.memory).inspect_err(|_x| eprintln!("Error during parsing"))?;

        if self.run_instruction(instruction)? {
            self.registers.program_counter = self.registers.program_counter.wrapping_add(offset as u16);
        }

        Ok(())
    }
}
