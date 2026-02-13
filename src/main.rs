use core::panic;
use std::{env, error::Error, fmt::{Binary, Debug, Display, LowerHex}, fs::{self}, io::{self, Read}, path::Path, rc::Rc};
use bitflags::{bitflags};

use crate::{errors::ExecutionError, instructions::{AddressingMode, Instruction, OpCode}, parser::parse_instruction, tracer::{Tracer, TracerError}};

mod instructions;
mod parser;
mod errors;
mod tracer;

// Currently only used when running https://github.com/Klaus2m5/6502_65C02_functional_tests only because there some thing we need to do, to run tests properly
const RUNNING_TEST: bool = true;
const DEBUG: bool = false;
const STOP_EACH_INSTRUCTION: bool = false;
const STOP_ON_JUMP_INSTRUCTION: bool = false;


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

struct Rom {
    data: [u8; u16::MAX as usize + 1]
}

impl From<Vec<u8>> for Rom {
    fn from(value: Vec<u8>) -> Self {
        if value.len() - 1 > u16::MAX as usize {
            panic!("Trying to create rom from vector with len > u16::MAX")
        }

        let mut data: [u8; u16::MAX as usize + 1] = [0; u16::MAX as usize + 1];

        value.iter().enumerate().for_each(|(i, x)| data[i] = *x);

        Self { data }
    }
}


impl LowerHex for Rom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line: String = String::new();

        let total_lines = self.data.len() / VALUES_PER_LINE;
        for i in 0..total_lines {
            let position = i * VALUES_PER_LINE;
            line.push_str(format!("{position:#x}\t").as_str());
            for j in 0..VALUES_PER_LINE {
                let value = self.data[(VALUES_PER_LINE * i) + j];
                line.push_str(format!("{value:#x} ").as_str());
            }
            line.push('\n');
        }

        write!(f, "{line}")
    }
}

impl Binary for Rom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line: String = String::new();

        let total_lines = self.data.len() / VALUES_PER_LINE;
        for i in 0..total_lines {
            let position = i * VALUES_PER_LINE;
            line.push_str(format!("{position:#x}\t").as_str());
            for j in 0..VALUES_PER_LINE {
                let value = self.data[(VALUES_PER_LINE * i) + j];
                line.push_str(format!("{value:#10b} ").as_str());
            }
            line.push('\n');
        }

        write!(f, "{line}")
    }
}

struct Memory {
    data: [u8; u16::MAX as usize + 1]
}

const VALUES_PER_LINE: usize = 16;

impl LowerHex for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line: String = String::new();

        let total_lines = self.data.len() / VALUES_PER_LINE;
        for i in 0..total_lines {
            let position = i * VALUES_PER_LINE;
            line.push_str(format!("{position:#x}\t").as_str());
            for j in 0..VALUES_PER_LINE {
                let value = self.data[(VALUES_PER_LINE * i) + j];
                line.push_str(format!("{value:#x} ").as_str());
            }
            line.push('\n');
        }

        write!(f, "{line}")
    }
}

impl Binary for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line: String = String::new();

        let total_lines = self.data.len() / VALUES_PER_LINE;
        for i in 0..total_lines {
            let position = i * VALUES_PER_LINE;
            line.push_str(format!("{position:#x}\t").as_str());
            for j in 0..VALUES_PER_LINE {
                let value = self.data[(VALUES_PER_LINE * i) + j];
                line.push_str(format!("{value:#10b} ").as_str());
            }
            line.push('\n');
        }

        write!(f, "{line}")
    }
}

impl Debug for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line: String = String::new();

        let total_lines = self.data.len() / VALUES_PER_LINE;
        for i in 0..total_lines {
            let position = i * VALUES_PER_LINE;
            line.push_str(format!("{position:#x}\t").as_str());

            for j in 0..VALUES_PER_LINE {
                let value = self.data[(VALUES_PER_LINE * i) + j];
                line.push_str(format!("{value} ").as_str());
            }
            line.push('\n');
        }

        write!(f, "{line}")
    }
}

struct Registers {
    program_counter: u16,
    stack_pointer: u8,

    accumulator: u8,
    x: u8,
    y: u8,

    processor_status: ProcessorStatus
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
struct CPU {
    memory: Memory,
    registers: Registers,
}

impl Debug for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_args!("Memory:\n{:x}\nRegisters:\n{:?}", self.memory, self.registers))
    }
}

impl Default for CPU {
    fn default() -> Self {
        CPU {
            memory: Memory { data: [0; u16::MAX as usize + 1] },
            registers: Registers { program_counter: 0xFFFC, stack_pointer: 0x0, accumulator: 0x0, x: 0x0, y: 0x0, processor_status: ProcessorStatus::empty() }
        }
    }
}


impl CPU {
    pub fn run_instruction(&mut self, instruction: Instruction) -> Result<bool, Box<dyn Error>> {
        let mut can_add_offset = true;

        let mode = instruction.addressing_mode;
        let value = instruction.value;

        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag;

        match instruction.opcode {
            OpCode::Load(op) => {
                match op {
                    instructions::LoadOp::LDA => {
                        let value = self.get_value(mode, value)?;

                        let flags = CPU::get_flags(
                            Some(self.registers.accumulator),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.accumulator = value;
                    }

                    instructions::LoadOp::LDX => {
                        let value = self.get_value(mode, value)?;

                        let flags = CPU::get_flags(
                            Some(self.registers.accumulator),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.x = value;
                    }

                    instructions::LoadOp::LDY => {
                        let value = self.get_value(mode, value)?;

                        let flags = CPU::get_flags(
                            Some(self.registers.accumulator),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.y = value;
                    }

                    instructions::LoadOp::STA => {
                        let _ = self.set_value(
                                    mode,
                                    value,
                                    self.registers.accumulator)
                                    .inspect_err(|_e| panic!("Failed to set value"));
                    }

                    instructions::LoadOp::STX => {
                        let _ = self.set_value(
                                    mode,
                                    value,
                                    self.registers.x)
                                    .inspect_err(|_e| panic!("Failed to set value"));
                    }

                    instructions::LoadOp::STY => {
                        let _ = self.set_value(
                                    mode,
                                    value,
                                    self.registers.y)
                                    .inspect_err(|_e| panic!("Failed to set value"));
                    }
                }
            }
            OpCode::Arithmetic(op) => {
                match op {
                    instructions::ArithmeticOp::ADC => {
                        let mask = mask_zn | ProcessorStatus::CarryFlag | ProcessorStatus::OverflowFlag;

                        let old_accumulator = self.registers.accumulator;

                        if (self.registers.processor_status & ProcessorStatus::DecimalMode).is_empty() {
                            let value = self.get_value(mode, value)?;
                            let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {0} else {1};

                            let (operand, operand_carry) = value.overflowing_add(carry);
                            let (final_result, result_carry) = self.registers.accumulator.overflowing_add(operand);

                            let flags = CPU::get_flags(
                                    Some(old_accumulator),
                                    Some(operand.wrapping_sub(carry)),
                                    &final_result,
                                    Some(mask & !ProcessorStatus::CarryFlag)
                                ) | if result_carry || operand_carry {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()};

                            self.set_flags(flags, mask);
                            self.registers.accumulator = final_result;
                        } else {
                            let value = CPU::hex2bcd(self.get_value(mode, value)?)?;
                            // println!("adc test {value}");
                            let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {0} else {1};

                            let (operand, _operand_carry) = value.overflowing_add(carry);
                            let (mut final_result, _result_carry) = CPU::hex2bcd(self.registers.accumulator)?.overflowing_add(operand);

                            let mut flags = CPU::get_flags(
                                    Some(old_accumulator),
                                    Some(operand.wrapping_sub(carry)),
                                    &final_result,
                                    Some(mask & !ProcessorStatus::CarryFlag)
                                );

                            // println!("adc test2 {final_result}");
                            if final_result > 99 {
                                flags |= ProcessorStatus::CarryFlag;
                                final_result -= 100;
                            }

                            self.set_flags(flags, mask);

                            // println!("adc test3 {final_result} {:X}", CPU::bcd2hex(final_result)?);
                            self.registers.accumulator = CPU::bcd2hex(final_result)?;
                        };

                    }

                    instructions::ArithmeticOp::SBC => {
                        let mask = mask_zn | ProcessorStatus::CarryFlag | ProcessorStatus::OverflowFlag;

                        let old_accumulator = self.registers.accumulator;

                        if (self.registers.processor_status & ProcessorStatus::DecimalMode).is_empty() {
                            let value: u8 = self.get_value(mode, value)?;
                            let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {0} else {1};

                            let (operand, operand_carry) = (!value).overflowing_add(carry);
                            let (final_result, new_carry) = self.registers.accumulator.overflowing_add(operand);

                            let flags = CPU::get_flags(
                                    Some(old_accumulator),
                                    Some(!value),
                                    &final_result,
                                    Some(mask & !ProcessorStatus::CarryFlag)
                                ) | if new_carry || operand_carry {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()};

                            self.set_flags(flags, mask);
                            self.registers.accumulator = final_result;
                        } else {
                            let value = CPU::hex2bcd(self.get_value(mode, value)?)?;
                            let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {0} else {1};

                            let operand = value + 1;
                            let acc = CPU::hex2bcd(self.registers.accumulator)?;

                            let (mut final_result, result_carry) = (acc + carry).overflowing_sub(operand);

                            // println!("{operand} {acc} {final_result} {result_carry}");
                            if result_carry {
                                // println!("{}", (100 - (u8::MAX - final_result)));
                                final_result = 100 - (u8::MAX - final_result);
                            }

                            let mut flags = CPU::get_flags(
                                    Some(old_accumulator),
                                    Some(!value),
                                    &final_result,
                                    Some(mask & !ProcessorStatus::CarryFlag)
                                );

                            if result_carry {
                                flags |= ProcessorStatus::CarryFlag;
                            }

                            if final_result > 99 {
                                flags |= ProcessorStatus::CarryFlag;
                                final_result -= 100;
                            }

                            self.set_flags(flags, mask);
                            self.registers.accumulator = CPU::bcd2hex(final_result)?;
                        };
                    }

                    instructions::ArithmeticOp::CMP => {
                        let old_value = self.registers.accumulator;

                        let value =self.get_value(mode, value)?;

                        let new_value = old_value.wrapping_sub(value);

                        self.set_flags(CPU::get_flags(
                                Some(old_value),
                                Some(value),
                                &new_value,
                                Some(mask_zn)
                            ) | if old_value >= value {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()},
                            mask_zn | ProcessorStatus::CarryFlag
                        );
                    }
                    instructions::ArithmeticOp::CPX => {
                        let old_value = self.registers.x;

                        let value = self.get_value(mode, value)?;

                        let new_value = old_value.wrapping_sub(value);

                        self.set_flags(CPU::get_flags(
                            Some(old_value),
                            Some(value),
                            &new_value,
                            Some(mask_zn)
                        ) | if old_value >= value {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()},
                        mask_zn | ProcessorStatus::CarryFlag
                    );
                    }

                    instructions::ArithmeticOp::CPY => {
                        let old_value = self.registers.y;

                        let value = self.get_value(mode, value)?;

                        let new_value = old_value.wrapping_sub(value);

                        self.set_flags(CPU::get_flags(
                            Some(old_value),
                            Some(value),
                            &new_value,
                            Some(mask_zn)
                        ) | if old_value >= value {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()},
                            mask_zn | ProcessorStatus::CarryFlag
                        );
                    }
                }
            }
            OpCode::JumpCall(op) => {
                match op {
                    instructions::JumpCallOp::JMP => {
                        let value = self.get_address(mode, value)?;
                        self.registers.program_counter = value;

                        can_add_offset = false;
                    }

                    instructions::JumpCallOp::JSR => {
                        let value = self.get_address(mode, value)?;

                        let target = self.registers.program_counter.wrapping_add(2);

                        self.stack_push(((target >> 8) & 0xFF) as u8);
                        self.stack_push((target & 0xFF) as u8);

                        self.registers.program_counter = value;

                        can_add_offset = false;
                    }

                    instructions::JumpCallOp::RTS => {
                        self.registers.program_counter = ((self.stack_pull() as u16) + ((self.stack_pull() as u16) << 8)).wrapping_add(1);
                        can_add_offset = false;
                    }
                }
            }
            OpCode::Stack(op) => {
                match op {
                    instructions::StackOp::TXS => {
                        self.registers.stack_pointer = self.registers.x;
                    }

                    instructions::StackOp::TSX => {
                        self.registers.x = self.registers.stack_pointer;

                        let flags = CPU::get_flags(None, None, &self.registers.x, Some(mask_zn));

                        self.set_flags(flags, mask_zn);
                    }

                    instructions::StackOp::PHA => {
                        self.stack_push(self.registers.accumulator);
                    }

                    instructions::StackOp::PLA => {
                        self.registers.accumulator = self.stack_pull();

                        let flags = CPU::get_flags(None, None, &self.registers.accumulator, Some(mask_zn));

                        self.set_flags(flags, mask_zn);
                    }

                    instructions::StackOp::PHP => {
                        // 1. It should always should return B flag. AFAIK it's not really a flag but some internal signal that isn't stored anywhere.
                        // 2. 5th bit is unused cuz status registry is actually just 6 1bit registries, and it's 1 just because it simplifies saving in stack or smt
                        self.stack_push((self.registers.processor_status | ProcessorStatus::BreakCommand).bits() | 1 << 5);
                    }

                    instructions::StackOp::PLP => {
                        let flags = ProcessorStatus::from_bits_truncate(self.stack_pull());
                        self.set_flags(flags, ProcessorStatus::all());
                    }
                }
            }
            OpCode::SystemFunc(op) => {
                match op {
                    instructions::SystemFuncOp::BRK => {
                        // there's bug in 6502 that makes "BRK" instruction "2 byte" long, when 2nd byte is ignored in any way.
                        // It was actually cool that some programs used this
                        self.stack_push((self.registers.program_counter.wrapping_add(2) >> 8) as u8);
                        self.stack_push((self.registers.program_counter.wrapping_add(2) & 0xFF) as u8);
                        self.stack_push((self.registers.processor_status | ProcessorStatus::BreakCommand).bits() | 1 << 5);

                        self.registers.program_counter = (self.memory.data[0xFFFE] as u16) + ((self.memory.data[0xFFFF] as u16) << 8);

                        self.set_flags(ProcessorStatus::InterruptDisable, ProcessorStatus::InterruptDisable);

                        can_add_offset = false;
                    }

                    instructions::SystemFuncOp::RTI => {
                        self.registers.processor_status = ProcessorStatus::from_bits_truncate(self.stack_pull());
                        self.registers.program_counter = (self.stack_pull() as u16) + ((self.stack_pull() as u16) << 8);
                        can_add_offset = false;
                    }

                    instructions::SystemFuncOp::NOP => {}
                }
            }
            OpCode::StatusFlag(op) => {
                match op {
                    instructions::StatusFlagOp::CLD => {
                        self.registers.processor_status &= !ProcessorStatus::DecimalMode
                    }

                    instructions::StatusFlagOp::CLC => {
                        self.registers.processor_status &= !ProcessorStatus::CarryFlag
                    }

                    instructions::StatusFlagOp::CLI => {
                        self.registers.processor_status &= !ProcessorStatus::InterruptDisable
                    }

                    instructions::StatusFlagOp::CLV => {
                        self.registers.processor_status &= !ProcessorStatus::OverflowFlag
                    }

                    instructions::StatusFlagOp::SEC => {
                        self.registers.processor_status |= ProcessorStatus::CarryFlag
                    }

                    instructions::StatusFlagOp::SEI => {
                        self.registers.processor_status |= ProcessorStatus::InterruptDisable
                    }

                    instructions::StatusFlagOp::SED => {
                        self.registers.processor_status |= ProcessorStatus::DecimalMode
                    }
                }
            }
            OpCode::Branch(op) => {
                match op {
                    instructions::BranchOp::BNE => {
                        if (self.registers.processor_status & ProcessorStatus::ZeroFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BEQ => {
                        if !(self.registers.processor_status & ProcessorStatus::ZeroFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BPL => {
                        if (self.registers.processor_status & ProcessorStatus::NegativeFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BCC => {
                        if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BCS => {
                        if !(self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BMI => {
                        if !(self.registers.processor_status & ProcessorStatus::NegativeFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BVC => {
                        if (self.registers.processor_status & ProcessorStatus::OverflowFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BVS => {
                        if !(self.registers.processor_status & ProcessorStatus::OverflowFlag).is_empty() {
                            self.registers.program_counter = self.get_address(mode, value)?;
                            can_add_offset = false;
                        }
                    }
                }
            }
            OpCode::IncDec(op) => {
                match op {
                    instructions::IncDecOp::DEX => {
                        let value = self.registers.x.wrapping_sub(1);

                        let flags = CPU::get_flags(
                            Some(self.registers.x),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.x = value;
                    },
                    instructions::IncDecOp::DEY => {
                        let value = self.registers.y.wrapping_sub(1);

                        let flags = CPU::get_flags(
                            Some(self.registers.y),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.y = value;
                    },
                    instructions::IncDecOp::INX => {
                        let value = self.registers.x.wrapping_add(1);

                        let flags = CPU::get_flags(
                            Some(self.registers.x),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.x = value;
                    },
                    instructions::IncDecOp::INY => {
                        let value = self.registers.y.wrapping_add(1);

                        let flags = CPU::get_flags(
                            Some(self.registers.y),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.y = value;
                    }

                    instructions::IncDecOp::INC => {
                        let mem_value = self.get_value(mode, value)?;

                        let result = mem_value.wrapping_add(1);

                        let flags = CPU::get_flags(
                            Some(mem_value),
                            None,
                            &result,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.set_value(mode, value, result)?;
                    }

                    instructions::IncDecOp::DEC => {
                        let mem_value = self.get_value(mode, value)?;

                        let result = mem_value.wrapping_sub(1);

                        let flags = CPU::get_flags(
                            Some(mem_value),
                            None,
                            &result,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.set_value(mode, value, result)?;
                    }
                }
            }
            OpCode::RegTrans(op) => {
                match op {
                    instructions::RegTransOp::TYA => {
                        let value = self.registers.y;

                        let flags = CPU::get_flags(
                            Some(self.registers.accumulator),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.accumulator = value;
                    }

                    instructions::RegTransOp::TAY => {
                        let value = self.registers.accumulator;

                        let flags = CPU::get_flags(
                            Some(self.registers.y),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.y = value;
                    }

                    instructions::RegTransOp::TAX => {
                        let value = self.registers.accumulator;

                        let flags = CPU::get_flags(
                            Some(self.registers.x),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.x = value;
                    }

                    instructions::RegTransOp::TXA => {
                        let value = self.registers.x;

                        let flags = CPU::get_flags(
                            Some(self.registers.accumulator),
                            None,
                            &value,
                            Some(mask_zn)
                        );

                        self.set_flags(flags, mask_zn);
                        self.registers.accumulator = value;
                    }
                }
            }
            OpCode::Logical(op) => {
                match op {
                    instructions::LogicOp::EOR => {
                        let value = self.get_value(mode, value)?;

                        let result = self.registers.accumulator ^ value;

                        let flags = CPU::get_flags(
                                Some(self.registers.accumulator),
                                Some(value),
                                &result,
                                Some(mask_zn)
                            );

                        self.set_flags(flags, mask_zn);
                        self.registers.accumulator = result;
                    }

                    instructions::LogicOp::ORA => {
                        let value = self.get_value(mode, value)?;

                        let result = self.registers.accumulator | value;

                        let flags = CPU::get_flags(
                                Some(self.registers.accumulator),
                                Some(value),
                                &result,
                                Some(mask_zn)
                            );

                        self.set_flags(flags, mask_zn);
                        self.registers.accumulator = result;
                    }

                    instructions::LogicOp::BIT => {
                        let mask = mask_zn | ProcessorStatus::OverflowFlag;
                        let a = self.registers.accumulator;
                        let value = self.get_value(mode, value)?;

                        let mut flags = ProcessorStatus::empty();

                        if a & value == 0 {
                            flags |= ProcessorStatus::ZeroFlag;
                        }

                        if value & 0b01000000 != 0 {
                            flags |= ProcessorStatus::OverflowFlag
                        }

                        if value & 0b10000000 != 0 {
                            flags |= ProcessorStatus::NegativeFlag
                        }


                        self.set_flags(flags, mask);
                    }

                    instructions::LogicOp::AND => {
                        let value = self.get_value(mode, value)?;

                        let result = self.registers.accumulator & value;

                        let flags = CPU::get_flags(
                                Some(self.registers.accumulator),
                                Some(value),
                                &result,
                                Some(mask_zn)
                            );

                        self.set_flags(flags, mask_zn);
                        self.registers.accumulator = result;
                    }
                }
            }
            OpCode::Shift(op) => {
                match op {
                    instructions::ShiftOp::ASL => {
                        let mask = mask_zn | ProcessorStatus::CarryFlag;

                        let op_value = self.get_value(mode, value)?;

                        let (result, _carry) = op_value.overflowing_shl(1);

                        let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask)) | if op_value & 0b10000000 != 0 {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()} ;

                        if mode == AddressingMode::Accumulator {
                            self.set_value(mode, 0, result)?;
                        } else {
                            self.set_value(mode, value, result)?;
                        }

                        self.set_flags(flags, mask);
                    }

                    instructions::ShiftOp::LSR => {
                        let mask = mask_zn | ProcessorStatus::CarryFlag;

                        let op_value = self.get_value(mode, value)?;

                        let (result, _carry) = op_value.overflowing_shr(1);

                        let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask)) | if op_value & 0b00000001 != 0 {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()} ;

                        if mode == AddressingMode::Accumulator {
                            self.set_value(mode, 0, result)?;
                        } else {
                            self.set_value(mode, value, result)?;
                        }

                        self.set_flags(flags, mask);
                    }

                    instructions::ShiftOp::ROL => {
                        let mask = mask_zn | ProcessorStatus::CarryFlag;

                        let op_value = self.get_value(mode, value)?;

                        let (mut result, _carry) = op_value.overflowing_shl(1);

                        result |= if (self.registers.processor_status & ProcessorStatus::CarryFlag).bits() != 0 {
                            1
                        } else {
                            0
                        };

                        let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask)) | if op_value & 0b10000000 != 0 {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()} ;

                        if mode == AddressingMode::Accumulator {
                            self.set_value(mode, 0, result)?;
                        } else {
                            self.set_value(mode, value, result)?;
                        }

                        self.set_flags(flags, mask);
                    }

                    instructions::ShiftOp::ROR => {
                        let mask = mask_zn | ProcessorStatus::CarryFlag;

                        let op_value = self.get_value(mode, value)?;

                        let (mut result, _carry) = op_value.overflowing_shr(1);

                        result |= if (self.registers.processor_status & ProcessorStatus::CarryFlag).bits() != 0 {
                            1 << 7
                        } else {
                            0 << 7
                        };

                        let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask)) | if op_value & 0b00000001 != 0 {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()} ;

                        if mode == AddressingMode::Accumulator {
                            self.set_value(mode, 0, result)?;
                        } else {
                            self.set_value(mode, value, result)?;
                        }

                        self.set_flags(flags, mask);
                    }
                }
            }
            _ => {
                todo!("WIP2, {instruction:?}")
            }
        };

        Ok(can_add_offset)
    }

    fn get_address(&self, mode: AddressingMode, value: u16) -> Result<u16, ExecutionError> {
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
                let lsb = self.memory.data[value as usize];
                let msb = self.memory.data[(value.wrapping_add(1)) as usize];

                Ok(((msb as u16) << 8) + (lsb as u16))
            }
            AddressingMode::IndexedIndirect => {
                let lsb = self.memory.data[((value as u8).wrapping_add(self.registers.x)) as usize];
                let msb = self.memory.data[((value as u8).wrapping_add(self.registers.x).wrapping_add(1)) as usize];


                Ok(((msb as u16) << 8) + (lsb as u16))
            }
            AddressingMode::IndirectIndexed => {
                let lsb = self.memory.data[(value) as usize];
                let msb = self.memory.data[(value.wrapping_add(1)) as usize];

                Ok((((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.registers.y as u16))
            }

            _ => Err(ExecutionError::InvalidAddressingMode)
        }
    }

    fn get_value(&self, mode: AddressingMode, value: u16) -> Result<u8, Box<dyn Error>> {
        if let Ok(addr) = self.get_address(mode, value) {
            Ok(self.memory.data[addr as usize])
        } else {
            match mode {
                AddressingMode::Accumulator => Ok(self.registers.accumulator),
                AddressingMode::Immediate => Ok(value as u8),

                _ => unimplemented!("Failed to get address and {mode:?} is not meant to get value")
            }
        }
    }

    fn set_value(&mut self, mode: AddressingMode, inst_value: u16, value: u8) -> Result<(), Box<dyn Error>> {
        match mode {
            AddressingMode::Absolute |
            AddressingMode::ZeroPage => {
                self.memory.data[inst_value as usize] = value;
            }

            AddressingMode::Accumulator => {
                self.registers.accumulator = value;
            }

            AddressingMode::ZeroPageX => {
                self.memory.data[(inst_value as u8).wrapping_add(self.registers.x) as usize] = value;
            }

            AddressingMode::ZeroPageY => {
                self.memory.data[(inst_value as u8).wrapping_add(self.registers.y) as usize] = value;
            }

            AddressingMode::AbsoluteX => {
                self.memory.data[(inst_value.wrapping_add(self.registers.x as u16)) as usize] = value;
            }

            AddressingMode::AbsoluteY => {
                self.memory.data[(inst_value.wrapping_add(self.registers.y as u16)) as usize] = value;
            }

            AddressingMode::IndexedIndirect => {
                let lsb = self.memory.data[((inst_value as u8).wrapping_add(self.registers.x)) as usize];
                let msb = self.memory.data[((inst_value as u8).wrapping_add(self.registers.x).wrapping_add(1)) as usize];

                let address = ((msb as u16) << 8) + (lsb as u16);

                self.memory.data[address as usize] = value
            }

            AddressingMode::IndirectIndexed => {
                let lsb = self.memory.data[inst_value as u8 as usize];
                let msb = self.memory.data[(inst_value as u8).wrapping_add(1) as usize];

                let address = (((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.registers.y as u16);

                self.memory.data[address as usize] = value;
            }

            _ => todo!("{:#06x} set_value {mode:?}", self.registers.program_counter),
        }

        Ok(())
    }

    fn set_flags(&mut self, flags: ProcessorStatus, mask: ProcessorStatus) {
        self.registers.processor_status = (self.registers.processor_status & !mask) | flags;
    }

    fn get_flags(prev_value: Option<u8>, operand: Option<u8>, result_value: &u8, mask: Option<ProcessorStatus>) -> ProcessorStatus {
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

    fn load_into_ram(&mut self, rom: &Rom) {
        self.memory.data = rom.data;
    }

    fn stack_push(&mut self, value: u8) {
        self.memory.data[0x100 + self.registers.stack_pointer as usize] = value;

        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
    }

    fn stack_pull(&mut self) -> u8 {
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);

        let address = 0x100 + self.registers.stack_pointer as u16;

        self.memory.data[address as usize]
    }

    fn reset(&mut self) {
        self.registers.program_counter = (self.memory.data[0xFFFD] as u16) << 8 | self.memory.data[0xFFFC] as u16;
    }

    fn once(&mut self) -> Result<(), Box<dyn Error>> {
        let (offset, instruction) = parse_instruction(self.registers.program_counter, &self.memory.data).inspect_err(|_x| eprintln!("Error during parsing"))?;

        if self.run_instruction(instruction)? {
            self.registers.program_counter = self.registers.program_counter.wrapping_add(offset as u16);
        }

        Ok(())
    }

    fn hex2bcd(num: u8) -> Result<u8, ExecutionError> {
        // println!("hex2bcd {num:#X}");
        if (num & 0xF0) <= 0x99 &&
           (num & 0x0F) <= 0x09{
            Ok(num - 6 * (num >> 4))
        } else {
            Err(ExecutionError::InvalidBcdValue(num))
        }
    }

    fn bcd2hex(num: u8) -> Result<u8, ExecutionError> {
        // println!("bcd2hex {num}");
        if num <= 99 {
            // Ok(num + 6 * (num >> 4))
            Ok(u8::from_str_radix(format!("{num}").as_str(), 16).unwrap())
        } else {
            Err(ExecutionError::InvalidBcdValue(num))
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        let executable_path = Path::new(args.first().unwrap());
        println!("Usage: {} [-t] path/to/bin", executable_path.file_name().unwrap().to_str().unwrap());
        return Ok(());
    }

    let run_tracer = args.contains(&"-t".to_string());

    let mut cpu = CPU::default();

    let data = fs::read(args.last().unwrap())?;

    let rom = Rom::from(data);

    let mut total_ran: u32 = 0;

    cpu.load_into_ram(&rom);

    cpu.reset();

    if RUNNING_TEST {
        cpu.registers.program_counter = 0x0400
    }

    if run_tracer {
        let mut tracer = Tracer::new(Rc::new(cpu));

        tracer.init()?;

        tracer.force_redraw()?;


        let mut error: Option<TracerError> = None;

        loop {
            let result = tracer.update();
            if let Ok(stop) = result {
                if !stop { break };
            } else {
                error = Some(result.unwrap_err());
                break;
            }
        }

        if let Some(e) = error {
            return Err(Box::new(e));
        }

        tracer.restore()?;

        return Ok(());
    }

    let mut prev_address = 0u16;

    loop {
        let (offset, instruction);
        let res = parse_instruction(cpu.registers.program_counter, &cpu.memory.data).inspect_err(|_x| eprintln!("Error during parsing"));

        if let Ok(values) = res {
            (offset, instruction) = values
        } else {
            println!("----------------------------------");

            let reg = &cpu.registers;
            println!("Registry:\n{reg:?}");

            // Some junk :)
            res?;
            return Ok(());
        }

        if DEBUG {
            println!("----------------------------------");
            println!("{instruction:?}");

            let reg = &cpu.registers;
            println!("Before:\n{reg:?}");
        }

        let res = cpu.run_instruction(instruction);
        total_ran += 1;

        if let Ok(increase) = res {
            if increase {
                cpu.registers.program_counter = cpu.registers.program_counter.wrapping_add(offset as u16);
            }
        } else {
            println!("----------------------------------");
            println!("{instruction:?}");

            let reg = &cpu.registers;
            println!("Registry:\n{reg:?}");

            res?;
        }

        if DEBUG {
            let _ = fs::create_dir("memdump");
            let reg = &cpu.registers;
            println!("After:\n{reg:?}");

            if STOP_EACH_INSTRUCTION {
                let _ = io::stdin().read(&mut [0u8]);
            }
            if STOP_ON_JUMP_INSTRUCTION
                && instruction.opcode == OpCode::JumpCall(instructions::JumpCallOp::JMP) {
                    let _ = io::stdin().read(&mut [0u8]);
                }
        }

        if prev_address == cpu.registers.program_counter {
            println!("Stuck at address {prev_address:#x} after {total_ran}");
            panic!("Stuck at address {prev_address:#x} after {total_ran}");
        }

        prev_address = cpu.registers.program_counter;
    }
}
