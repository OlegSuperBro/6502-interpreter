use core::panic;
use std::{env, error::Error, fmt::{Binary, Debug, Display, LowerHex}, fs, io::{self, Read}, path::Path};
use bitflags::{bitflags};

use crate::{instructions::OpCode, parser::{AddressingMode, Instruction, parse_instruction}};

mod instructions;
mod parser;
mod errors;

// Currently only used when running https://github.com/Klaus2m5/6502_65C02_functional_tests only because there some thing we need to do, to run tests properly
const RUNNING_TEST: bool = true;

bitflags! {
    #[derive(Clone, Copy)]
    pub struct ProcessorStatus: u8 {
        const CarryFlag = 1 << 0;
        const ZeroFlag = 1 << 1;
        const InterruptDisable = 1 << 2;
        const DecimalMode = 1 << 3;
        const BreakCommand = 1 << 4;
        const OverflowFlag = 1 << 5;
        const NegativeFlag = 1 << 6;
    }
}

impl Display for ProcessorStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#010b}", self.bits())
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

        write!(f, "{}", line)
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

        write!(f, "{}", line)
    }
}

struct Memory {
    data: [u8; u16::MAX as usize]
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

        write!(f, "{}", line)
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

        write!(f, "{}", line)
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

        write!(f, "{}", line)
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
        write!(f, "{}", format_args!("PC: {} SP: {}\nA: {} X: {} Y: {}\n{}", self.program_counter, self.stack_pointer, self.accumulator, self.x, self.y, self.processor_status))
    }
}
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
            memory: Memory { data: [0; u16::MAX as usize] },
            registers: Registers { program_counter: 0xFFFC, stack_pointer: 0x0, accumulator: 0x0, x: 0x0, y: 0x0, processor_status: ProcessorStatus::empty() }
        }
    }
}

enum SingleOrDoubleValue {
    Single(u8),
    Double(u16),
}

impl CPU {
    pub fn run_instruction(&mut self, instruction: Instruction) -> bool {
        let mut can_add_offset = true;

        match instruction.opcode {
            OpCode::Load(op) => {
                match op {
                    instructions::LoadOp::LDA => {
                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(x) => self.memory.data[x as usize],
                        };

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.accumulator = value;
                    }

                    instructions::LoadOp::LDX => {
                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(x) => self.memory.data[x as usize],
                        };

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.x = value;
                    }

                    instructions::LoadOp::LDY => {
                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(x) => self.memory.data[x as usize],
                        };

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.y = value;
                    }

                    instructions::LoadOp::STA => {
                        let _ = self.set_value(
                                    instruction.addressing_mode,
                                    instruction.value,
                                    self.registers.accumulator)
                                    .inspect_err(|_e| panic!("Failed to set value"));
                    }
                    _ => {
                        todo!("WIP1 {op:?}")
                    }
                }
            }
            OpCode::Arithmetic(op) => {
                match op {
                    instructions::ArithmeticOp::ADC => {
                        let old_accumulator = self.registers.accumulator;

                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(_x) => todo!("invalid return value type"),
                        };
                        let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {0} else {1};

                        let operand = value.wrapping_add(carry);
                        let final_result = self.registers.accumulator.wrapping_add(operand);

                        self.registers.processor_status = CPU::get_flags(Some(old_accumulator), Some(operand), &final_result) & (ProcessorStatus::CarryFlag | ProcessorStatus::ZeroFlag | ProcessorStatus::OverflowFlag | ProcessorStatus::NegativeFlag);
                        self.registers.accumulator = final_result;
                    }

                    instructions::ArithmeticOp::CMP => {
                        let old_value = self.registers.accumulator;

                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(_x) => todo!("invalid return value type"),
                        };

                        let new_value = old_value.wrapping_sub(value);

                        self.registers.processor_status = CPU::get_flags(Some(old_value), Some(value), &new_value) & (ProcessorStatus::CarryFlag | ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag)
                    }
                    _ => todo!("Arithmetic {op:?}")
                }
            }
            OpCode::JumpCall(op) => {
                match op {
                    instructions::JumpCallOp::JMP => {
                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(_x) => todo!("invalid return value type"),
                            SingleOrDoubleValue::Double(x) => x,
                        };
                        self.registers.program_counter = value;

                        can_add_offset = false;
                    }

                    _ => todo!("JumpCall")
                }
            }
            OpCode::Stack(op) => {
                match op {
                    instructions::StackOp::TXS => {
                        self.registers.stack_pointer = self.registers.x;
                    }

                    instructions::StackOp::TSX => {
                        self.registers.x = self.registers.stack_pointer
                    }

                    _ => todo!("run stack {op:?}")
                }
            }
            OpCode::SystemFunc(op) => {
                match op {
                    instructions::SystemFuncOp::BRK => {
                        self.registers.processor_status |= ProcessorStatus::BreakCommand;

                        let _ = self.set_value(AddressingMode::Absolute,
                                0x0100 + (self.registers.stack_pointer + 0) as u16,
                                self.registers.processor_status.bits());

                        let _ = self.set_value(AddressingMode::Absolute,
                                0x0100 + (self.registers.stack_pointer + 1) as u16,
                                (self.registers.program_counter & 0xFF) as u8);

                        let _ = self.set_value(AddressingMode::Absolute,
                            0x0100 + (self.registers.stack_pointer + 2) as u16,
                            (self.registers.program_counter >> 8) as u8);
                    }

                    instructions::SystemFuncOp::NOP => {}

                    _ => todo!("run system {op:?}")
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

                    _ => todo!("run status flag {op:?}")
                }
            }
            OpCode::Branch(op) => {
                match op {
                    instructions::BranchOp::BNE => {
                        if (self.registers.processor_status & ProcessorStatus::ZeroFlag).is_empty() {
                            let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                                SingleOrDoubleValue::Single(x) => x as i8,
                                SingleOrDoubleValue::Double(_x) => todo!("invalid return value type"),
                            } + 2; // cuz 2 should increment during processing this instruction
                            if value < 0 {
                                self.registers.program_counter = self.registers.program_counter.wrapping_sub(value.abs() as u16);
                            } else {
                                self.registers.program_counter = self.registers.program_counter.wrapping_add(value as u16);
                            }

                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BEQ => {
                        if !(self.registers.processor_status & ProcessorStatus::ZeroFlag).is_empty() {
                            let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                                SingleOrDoubleValue::Single(x) => x as i8,
                                SingleOrDoubleValue::Double(_x) => todo!("invalid return value type"),
                            } + 2; // cuz 2 should increment during processing this instruction

                            if value < 0 {
                                self.registers.program_counter = self.registers.program_counter.wrapping_sub(value.abs() as u16);
                            } else {
                                self.registers.program_counter = self.registers.program_counter.wrapping_add(value as u16);
                            }

                            can_add_offset = false;
                        }
                    }

                    instructions::BranchOp::BPL => {
                        if (self.registers.processor_status & ProcessorStatus::NegativeFlag).is_empty() {
                            let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                                SingleOrDoubleValue::Single(x) => x as i8,
                                SingleOrDoubleValue::Double(_x) => todo!("invalid return value type"),
                            } + 2; // cuz 2 should increment during processing this instruction

                            if value < 0 {
                                self.registers.program_counter = self.registers.program_counter.wrapping_sub(value.abs() as u16);
                            } else {
                                self.registers.program_counter = self.registers.program_counter.wrapping_add(value as u16);
                            }

                            can_add_offset = false;
                        }
                    }
                    _ => todo!("run branch {op:?}")
                }
            }
            OpCode::IncDec(op) => {
                match op {
                    instructions::IncDecOp::DEX => {
                        let value = self.registers.x.wrapping_sub(1);

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.x = value;
                    },
                    instructions::IncDecOp::DEY => {
                        let value = self.registers.y.wrapping_sub(1);

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.y = value;
                    },
                    _ => todo!("run IncDec {op:?}")
                }
            }
            OpCode::RegTrans(op) => {
                match op {
                    instructions::RegTransOp::TYA => {
                        let value = self.registers.y;

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.accumulator = value;
                    }
                    instructions::RegTransOp::TAX => {
                        let value = self.registers.accumulator;

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.x = value;
                    }
                    _ => todo!("run regtrans {op:?}")
                }
            }
            OpCode::Logical(op) => {
                match op {
                    instructions::LogicOp::EOR => {
                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(_x) => todo!("invalid return value type")
                        };

                        let result = self.registers.accumulator ^ value;

                        let flags = CPU::get_flags(Some(self.registers.accumulator), None, &value) & (ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag);

                        self.registers.processor_status = flags;
                        self.registers.accumulator = result;
                    }
                    _ => todo!("run logical {op:?}")
                }
            }
            _ => {
                todo!("WIP2, {instruction:?}")
            }
        };

        return can_add_offset;
    }

    fn get_value(&mut self, mode: AddressingMode, value: u16) -> SingleOrDoubleValue {
        match mode {
            AddressingMode::Accumulator => SingleOrDoubleValue::Single(self.registers.accumulator),
            AddressingMode::Immediate => SingleOrDoubleValue::Single(value as u8),
            AddressingMode::ZeroPage => SingleOrDoubleValue::Single(self.memory.data[value as usize]),
            AddressingMode::ZeroPageX => SingleOrDoubleValue::Single(self.memory.data[self.registers.x.wrapping_add(value as u8) as usize]),
            AddressingMode::ZeroPageY => SingleOrDoubleValue::Single(self.memory.data[self.registers.y.wrapping_add(value as u8) as usize]),
            AddressingMode::Relative => SingleOrDoubleValue::Single(value as u8),
            AddressingMode::Absolute => SingleOrDoubleValue::Double(value),
            AddressingMode::AbsoluteX => SingleOrDoubleValue::Double(value + self.registers.x as u16),
            AddressingMode::AbsoluteY => SingleOrDoubleValue::Double(value + self.registers.y as u16),
            AddressingMode::Indirect => {
                let lsb = self.memory.data[value as usize];
                let msb = self.memory.data[(value + 1) as usize];

                SingleOrDoubleValue::Double(((msb as u16) << 8) + (lsb as u16))
            }
            AddressingMode::IndexedIndirect => {
                let lsb = self.memory.data[(value + self.registers.x as u16) as usize];
                let msb = self.memory.data[(value + self.registers.x as u16 + 1) as usize];

                let address = ((msb as u16) << 8) + (lsb as u16);

                SingleOrDoubleValue::Single(self.memory.data[address as usize])
            }
            AddressingMode::IndirectIndexed => {
                let lsb = self.memory.data[(value) as usize];
                let msb = self.memory.data[(value + 1) as usize];

                let address = ((msb as u16) << 8) + (lsb as u16) + self.registers.y as u16;

                SingleOrDoubleValue::Single(self.memory.data[address as usize])
            }

            _ => todo!("get_value")
        }
    }

    fn set_value(&mut self, mode: AddressingMode, address: u16, value: u8) -> Result<(), Box<dyn Error>> {
        match mode {
            AddressingMode::Absolute => {
                self.memory.data[address as usize] = value;
            }
            _ => todo!("{mode:?}"),
        }

        Ok(())
    }

    fn get_flags(prev_value: Option<u8>, operand: Option<u8>, result_value: &u8) -> ProcessorStatus {
        let mut result_flag = ProcessorStatus::empty();

        if let Some(prev_value) = prev_value {
            result_flag |= if prev_value & 0b10000000 != result_value & 0b10000000 {ProcessorStatus::OverflowFlag} else {ProcessorStatus::empty()};

            if let Some(operand) = operand {
                result_flag |= if !(prev_value ^ operand) & (prev_value ^ result_value) & 0b10000000 != 0 {ProcessorStatus::CarryFlag} else {ProcessorStatus::empty()};
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

        result_flag
    }

    fn reset(&mut self, rom: &Rom) {
        self.registers.program_counter = (rom.data[0xFFFD] as u16) << 8 | rom.data[0xFFFC] as u16
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        let executable_path = Path::new(args.first().unwrap());
        println!("Usage: {} path/to/bin", executable_path.file_name().unwrap().to_str().unwrap());
        return Ok(());
    }

    let mut cpu = CPU::default();

    let data = fs::read(args.get(1).unwrap())?;

    let rom = Rom::from(data);

    cpu.reset(&rom);

    if RUNNING_TEST {
        cpu.registers.program_counter = 0x0400
    }

    loop {
        let (offset, instruction) = parse_instruction(cpu.registers.program_counter, &rom.data).inspect_err(|_x| eprintln!("Error during parsing"))?;
        println!("{instruction:?}");

        let reg = &cpu.registers;
        println!("Before:\n{reg:?}");

        if cpu.run_instruction(instruction) {
            cpu.registers.program_counter = cpu.registers.program_counter.wrapping_add(offset as u16);
        }

        let reg = &cpu.registers;
        println!("After:\n{reg:?}");

        let _ = io::stdin().read(&mut [0u8]);
        // if instruction.opcode == OpCode::JumpCall(instructions::JumpCallOp::JMP) {
        //     let _ = io::stdin().read(&mut [0u8]);
        // }
    }

    // let instructions = parser::parse_bin(data.as_slice())?;

    // for instruction in instructions {
    //     println!("{instruction:?}");

    //     cpu.run_instruction(instruction);

    //     let reg = &cpu.registers;
    //     println!("{reg:?}");
    //     let _ = io::stdin().read(&mut [0u8]);
    // }

    Ok(())
}
