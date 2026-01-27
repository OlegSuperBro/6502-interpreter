use std::{cell::RefCell, env, error::Error, fmt::{Binary, Debug, LowerHex}, fs, io::{self, Read}, path::Path, rc::Rc};
use bitflags::bitflags;

use crate::{instructions::OpCode, parser::{AddressingMode, Instruction, parse_instruction}};

mod instructions;
mod parser;
mod errors;

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
        write!(f, "{}", format_args!("PC: {} SP: {}\nA: {} X: {} Y: {}", self.program_counter, self.stack_pointer, self.accumulator, self.x, self.y))
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
    pub fn run_instruction(&mut self, instruction: Instruction) {
        match instruction.opcode {
            OpCode::Load(op) => {
                match op {
                    instructions::LoadOp::LDA => {
                        self.registers.accumulator = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(x) => todo!("invalid return value type"),
                        };
                    }
                    _ => {
                        todo!("WIP1")
                    }
                }
            }
            OpCode::Arithmetic(op) => {
                match op {
                    instructions::ArithmeticOp::ADC => {
                        let old_accumulator = self.registers.accumulator;

                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => x,
                            SingleOrDoubleValue::Double(x) => todo!("invalid return value type"),
                        };

                        let carry = (self.registers.processor_status & ProcessorStatus::CarryFlag).bits();

                        let sum_value = value.wrapping_add(carry);
                        
                        let sum_result = self.registers.accumulator.checked_add(value).and_then(|x| x.checked_add(carry));

                        if let Some(result) = sum_result {
                            self.registers.accumulator = result;
                            self.registers.processor_status &= !ProcessorStatus::CarryFlag;
                        } else {
                            self.registers.accumulator = self.registers.accumulator.wrapping_add(sum_value);
                            self.registers.processor_status |= ProcessorStatus::CarryFlag;
                        }

                        let final_result = self.registers.accumulator;

                        if final_result == 0 {
                            self.registers.processor_status |= ProcessorStatus::ZeroFlag;
                        } else {
                            self.registers.processor_status &= !ProcessorStatus::ZeroFlag;
                        }

                        if (old_accumulator & 0b01000000 != sum_value & 0b01000000) ||
                            (old_accumulator & 0b10000000 != sum_value & 0b10000000) {
                            self.registers.processor_status |= ProcessorStatus::OverflowFlag;    
                        } else {
                            self.registers.processor_status &= !ProcessorStatus::OverflowFlag;
                        }

                        if final_result & 0b10000000 != 0 {
                            self.registers.processor_status |= ProcessorStatus::NegativeFlag;
                        } else {
                            self.registers.processor_status &= !ProcessorStatus::NegativeFlag;
                        }
                    }
                    _ => todo!("Arithmetic")
                }
            }
            OpCode::JumpCall(op) => {
                match op {
                    instructions::JumpCallOp::JMP => {
                        let value = match self.get_value(instruction.addressing_mode, instruction.value) {
                            SingleOrDoubleValue::Single(x) => todo!("invalid return value type"),
                            SingleOrDoubleValue::Double(x) => x,
                        };
                        self.registers.program_counter = value;
                    }

                    _ => todo!("JumpCall")
                }
            }

            _ => {
                todo!("WIP2, {instruction:?}")
            }
        };
    }

    fn get_value(&mut self, mode: AddressingMode, value: u16) -> SingleOrDoubleValue {
        match mode {
            AddressingMode::Accumulator => SingleOrDoubleValue::Single(self.registers.accumulator),
            AddressingMode::Immediate => SingleOrDoubleValue::Single(value as u8),
            AddressingMode::ZeroPage => SingleOrDoubleValue::Single(self.memory.data[value as usize]),
            AddressingMode::ZeroPageX => SingleOrDoubleValue::Single(self.memory.data[self.registers.x.wrapping_add(value as u8) as usize]),
            AddressingMode::ZeroPageY => SingleOrDoubleValue::Single(self.memory.data[self.registers.y.wrapping_add(value as u8) as usize]),
            AddressingMode::Relative => SingleOrDoubleValue::Double(self.registers.program_counter.wrapping_add_signed(value as i16 & 0xFF)),
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
        Ok(())
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

    loop {
        let (offset, instruction) = parse_instruction(cpu.registers.program_counter, &rom.data).inspect_err(|_x| eprintln!("Error during parsing"))?;
        cpu.run_instruction(instruction);

        cpu.registers.program_counter = cpu.registers.program_counter.wrapping_add(offset as u16);
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
