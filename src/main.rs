use std::{error::Error, fmt::{Binary, Debug, LowerHex}, fs, io::{self, Read}};
use bitflags::bitflags;

use crate::{instructions::OpCode, parser::{AddressingMode, Instruction}};

mod instructions;
mod parser;
mod errors;

bitflags! {
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
        return CPU {
            memory: Memory { data: [0; u16::MAX as usize] },
            registers: Registers { program_counter: 0x0000_0000, stack_pointer: 0x0000, accumulator: 0x0000, x: 0x0000, y: 0x0000 }
        }
    }
}

impl CPU {
    pub fn run_instruction(&mut self, instruction: Instruction) {
        match instruction.opcode {
            OpCode::Load(op) => {
                match op {
                    instructions::LoadOp::LDA => {
                        self.registers.accumulator = self.get_value(instruction.addressing_mode, instruction.value);
                    }
                    _ => {
                        todo!()
                    }
                }
            }

            _ => {
                todo!()
            }
        };
    }

    fn get_value(&mut self, mode: AddressingMode, value: u16) -> u8 {
        return value as u8 // TODO: this can lead to breaking for 16 bit operations
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut cpu = CPU::default();

    let data = fs::read("/home/od.rakov@cit-sk.local/Рабочий стол/cc65/bin/test.bin")?;

    let instructions = parser::parse_bin(data.as_slice())?;

    for instruction in instructions {
        println!("{instruction:?}");

        cpu.run_instruction(instruction);

        let reg = &cpu.registers;
        println!("{reg:?}");
        let _ = io::stdin().read(&mut [0u8]);
    }

    return Ok(())
}
