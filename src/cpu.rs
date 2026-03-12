use bitflags::bitflags;
use std::{
    cell::RefCell, error::Error, fmt::{Debug, Display}, rc::Rc
};

use crate::{
    errors::ExecutionError,
    instructions::{AddressingMode, Instruction, JumpCallOp, OpCode, StackOp, SystemFuncOp},
    parser::{parse_instruction, parse_opcode_byte},
};

macro_rules! check_states {
    ($current_state:expr, $($states:expr => $code:block)+) => {
        $(
            if ($current_state ^ $states).is_empty() $code
        )+
    };
}

const STACK_OFFSET: u16 = 0x0100;

bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct State: u16 {
        const T0 = 1 << 0;
        const Tplus = 1 << 1;
        const T2 = 1 << 2;
        const T3 = 1 << 3;
        const T4 = 1 << 4;
        const T5 = 1 << 5;

        const T1 = 1 << 6;
        const V0 = 1 << 7;
        const T6 = 1 << 8;

        // RMW
        const SD1 = 1 << 9;
        const SD2 = 1 << 10;
    }
}

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
        write!(
            f,
            "N: {} V: {} B: {} D: {} I: {} Z: {} C: {} \n({:#010b} - {})\nWith B:\n({:#010b} - {})",
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

    pub processor_status: ProcessorStatus,

    pub predecode: (OpCode, AddressingMode),
    pub instruction: InstructionRegistry,
}

impl Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format_args!(
                "PC: {:#X} SP: {:#X} \nA: {} ({}) X: {} ({}) Y: {} ({})\n{}",
                self.program_counter,
                self.stack_pointer,
                self.accumulator,
                self.accumulator as i8,
                self.x,
                self.x as i8,
                self.y,
                self.y as i8,
                self.processor_status
            )
        )
    }
}

struct InstructionRegistry {
    operation: Option<(OpCode, AddressingMode)>,

    operand: Option<u16>,
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU {
    pub memory: [u8; u16::MAX as usize + 1],
    pub registers: Registers,
    state: State,

    #[allow(non_snake_case)]
    pub NMI: bool,
    #[allow(non_snake_case)]
    pub IRQ: bool,

    pub memory_address_bus: Rc<RefCell<u16>>,
    pub data_bus: Rc<RefCell<u8>>,
}

impl Default for CPU {
    fn default() -> Self {
        CPU {
            memory: [0; u16::MAX as usize + 1],
            registers: Registers {
                program_counter: 0xFFFC,
                stack_pointer: 0x0,
                accumulator: 0x0,
                x: 0x0,
                y: 0x0,
                processor_status: ProcessorStatus::empty(),
                predecode: (OpCode::Unknown, AddressingMode::Unknown),
                instruction: InstructionRegistry {
                    operation: None,
                    operand: None,
                },
            },
            state: State::T0,


            NMI: false,
            IRQ: false,

            memory_address_bus: Rc::new(RefCell::new(0)),
            data_bus: Rc::new(RefCell::new(0)),
        }
    }
}

pub trait ProcessInstruction<T> {
    fn process(&mut self, instruction: Instruction, op: T) -> Result<bool, Box<dyn Error>>;
}

impl CPU {
    pub fn run_instruction(&mut self, instruction: Instruction) -> Result<(), Box<dyn Error>> {
        match instruction.opcode {
            OpCode::Load(op) => self.process(instruction, op),
            OpCode::Arithmetic(op) => self.process(instruction, op),
            OpCode::JumpCall(op) => self.process(instruction, op),
            OpCode::Stack(op) => self.process(instruction, op),
            OpCode::SystemFunc(op) => self.process(instruction, op),
            OpCode::StatusFlag(op) => self.process(instruction, op),
            OpCode::Branch(op) => self.process(instruction, op),
            OpCode::IncDec(op) => self.process(instruction, op),
            OpCode::RegTrans(op) => self.process(instruction, op),
            OpCode::Logical(op) => self.process(instruction, op),
            OpCode::Shift(op) => self.process(instruction, op),
            _ => {
                todo!("Not implemented group, {instruction:?}")
            }
        };

        Ok(())
    }

    pub fn get_address(&self, mode: AddressingMode, value: u16) -> Result<u16, ExecutionError> {
        match mode {
            AddressingMode::ZeroPage => Ok(value),
            AddressingMode::ZeroPageX => Ok(self.registers.x.wrapping_add(value as u8) as u16),
            AddressingMode::ZeroPageY => Ok(self.registers.y.wrapping_add(value as u8) as u16),
            AddressingMode::Relative => {
                let value = value as i8; // cuz 2 should increment during processing this instruction

                if value < 0 {
                    Ok(self
                        .registers
                        .program_counter
                        .wrapping_add(2)
                        .wrapping_sub(value.wrapping_abs() as u16))
                } else {
                    Ok(self
                        .registers
                        .program_counter
                        .wrapping_add(2)
                        .wrapping_add(value as u16))
                }
            }
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
                let msb = self.memory
                    [((value as u8).wrapping_add(self.registers.x).wrapping_add(1)) as usize];

                Ok(((msb as u16) << 8) + (lsb as u16))
            }
            AddressingMode::IndirectIndexed => {
                let lsb = self.memory[(value) as usize];
                let msb = self.memory[(value.wrapping_add(1)) as usize];

                Ok((((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.registers.y as u16))
            }

            _ => Err(ExecutionError::InvalidAddressingMode),
        }
    }

    pub fn get_value(&self, mode: AddressingMode, value: u16) -> Result<u8, Box<dyn Error>> {
        if let Ok(addr) = self.get_address(mode, value) {
            Ok(self.memory[addr as usize])
        } else {
            match mode {
                AddressingMode::Accumulator => Ok(self.registers.accumulator),
                AddressingMode::Immediate => Ok(value as u8),

                _ => unimplemented!("Failed to get address and {mode:?} is not meant to get value"),
            }
        }
    }

    pub fn set_value(
        &mut self,
        mode: AddressingMode,
        inst_value: u16,
        value: u8,
    ) -> Result<(), Box<dyn Error>> {
        match mode {
            AddressingMode::Absolute | AddressingMode::ZeroPage => {
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
                let msb = self.memory[((inst_value as u8)
                    .wrapping_add(self.registers.x)
                    .wrapping_add(1)) as usize];

                let address = ((msb as u16) << 8) + (lsb as u16);

                self.memory[address as usize] = value
            }

            AddressingMode::IndirectIndexed => {
                let lsb = self.memory[inst_value as u8 as usize];
                let msb = self.memory[(inst_value as u8).wrapping_add(1) as usize];

                let address =
                    (((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.registers.y as u16);

                self.memory[address as usize] = value;
            }

            _ => todo!("{:#06x} set_value {mode:?}", self.registers.program_counter),
        }

        Ok(())
    }

    pub fn set_flags(&mut self, flags: ProcessorStatus, mask: ProcessorStatus) {
        self.registers.processor_status = (self.registers.processor_status & !mask) | flags;
    }

    pub fn get_flags(
        prev_value: Option<u8>,
        operand: Option<u8>,
        result_value: &u8,
        mask: Option<ProcessorStatus>,
    ) -> ProcessorStatus {
        let mut result_flag = ProcessorStatus::empty();

        if let (Some(prev_value), Some(operand)) = (prev_value, operand) {
            result_flag |=
                if !(prev_value ^ operand) & (prev_value ^ result_value) & 0b10000000 != 0 {
                    ProcessorStatus::OverflowFlag
                } else {
                    ProcessorStatus::empty()
                };
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
        self.registers.program_counter =
            (self.memory[0xFFFD] as u16) << 8 | self.memory[0xFFFC] as u16;
    }

    pub fn once(&mut self) -> Result<(), Box<dyn Error>> {
        let (offset, instruction) = parse_instruction(self.registers.program_counter, &self.memory)
            .inspect_err(|_x| eprintln!("Error during parsing"))?;

        self.registers.program_counter = self.registers.program_counter.wrapping_add(offset as u16);

        Ok(())
    }

    pub fn run_full_cycle(&mut self) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    pub fn run_phi_1(&mut self) -> Result<(), Box<dyn Error>> {
        check_states! {
            self.state,
            // 1st cycle of 2 cycle opcodes
            State::T0 | State::T2 => {
                self.registers.instruction.operation = Some(self.registers.predecode);
            }
            // 1st cycle of 3+ cycle opcodes
            State::T2 => {
                self.registers.instruction.operation = Some(self.registers.predecode);
                let bus = Rc::clone(&self.memory_address_bus);

                *bus.borrow_mut() = self.registers.program_counter;

            }
            // 2nd cycle of 4+ cycle opcodes
            State::T3 => {
                todo!("phi1")
            }
            // 2nd cycle of RMW zp
            State::T3 | State::SD1 => {
                todo!("phi1")
            }
            // 3rd cycle of 5+ cycle opcodes
            State::T4 => {
                todo!("phi1")
            }
            // 3rd cycle of RMW zp,X & abs
            State::T4 | State::SD1 => {
                todo!("phi1")
            }
            // 3rd cycle of RMW zp
            State::T4 | State::SD2 => {
                todo!("phi1")
            }
            // 4th cycle of 6+ cycle opcodes
            State::T5 => {
                todo!("phi1")
            }
            // 4th cycle of RMW abs,X & abs,Y (latter illegal)
            State::T5 | State::SD1 => {
                todo!("phi1")
            }
            // 4th cycle of RMW zp,X & abs
            State::T5 | State::SD2 => {
                todo!("phi1")
            }
            // 4th cycle of BRK
            State::T5 | State::V0 => {
                todo!("phi1")
            }
            // 5th cycle of BRK
            State::T6 => {
                todo!("phi1")
            }
            // 5th cycle of RMW (zp,X) & (zp),Y (illegal)
            State::SD1 => {
                todo!("phi1")
            }
            // 5th of RMW abs,X&Y, 6th of RMW (zp,X) & (zp),Y
            State::SD2 => {
                todo!("phi1")
            }
            // Terminal state of opcodes that run forever
            State::empty() => {
                todo!("phi1")
            }
            // 2nd-to-last cycle of all(*) opcodes
            State::T0 => {
                todo!("phi1")
            }
            // Last cycle of all(*) opcodes
            State::Tplus | State::T1 => {
                todo!("phi1")
            }
            // Last cycle of branches not taken & no page cross
            State::T1 => {
                todo!("phi1")
            }
            // Clock long-term hold by RES
            State::T0 | State::Tplus => {
                todo!("phi1")
            }
            // 1 cycle after RES of BRK during T5 [V0] cycle
            State::T0 | State::T6 => {
                todo!("phi1")
            }
            // 1 cycle after RES of RMW during SD cycle
            State::T0 | State::SD1 => {
                todo!("phi1")
            }
            // 1 cycle after RES of RMW just after SD cycle
            State::T0 | State::SD2 => {
                todo!("phi1")
            }
            // 2 cycles after RES (still held) of RMW during SD cycle
            State::T0 | State::Tplus | State::SD2 => {
                todo!("phi1")
            }
            // 2 cycles after RES (released) of RMW during SD cycle
            State::Tplus | State::T1 | State::SD2 => {
                todo!("phi1")
            }
        };

        Ok(())
    }

    pub fn run_phi_2(&mut self) -> Result<(), Box<dyn Error>> {
        check_states! {
            self.state,
            // 1st cycle of 2 cycle opcodes
            State::T0 | State::T2 => {
                let (opcode, addressing_mode) = self.registers.instruction.operation.unwrap();
                self.run_instruction(Instruction { opcode, addressing_mode, value: 0 });

                self.state = State::Tplus | State::T1;
            }
            // 1st cycle of 3+ cycle opcodes
            State::T2 => {
                let (opcode, addressing_mode) = self.registers.instruction.operation.unwrap();

                self.registers.instruction.operand = Some(Rc::clone(&self.data_bus).borrow().clone() as u16);

                match get_byte_size(&addressing_mode) {
                    1 | 2 => {
                        self.state = State::T3;
                    }

                    _ => unimplemented!("Invalid instruction byte size")
                }
            }
            // 2nd cycle of 4+ cycle opcodes
            State::T3 => {
                todo!("phi2")
            }
            // 2nd cycle of RMW zp
            State::T3 | State::SD1 => {
                todo!("phi2")
            }
            // 3rd cycle of 5+ cycle opcodes
            State::T4 => {
                todo!("phi2")
            }
            // 3rd cycle of RMW zp,X & abs
            State::T4 | State::SD1 => {
                todo!("phi2")
            }
            // 3rd cycle of RMW zp
            State::T4 | State::SD2 => {
                todo!("phi2")
            }
            // 4th cycle of 6+ cycle opcodes
            State::T5 => {
                todo!("phi2")
            }
            // 4th cycle of RMW abs,X & abs,Y (latter illegal)
            State::T5 | State::SD1 => {
                todo!("phi2")
            }
            // 4th cycle of RMW zp,X & abs
            State::T5 | State::SD2 => {
                todo!("phi2")
            }
            // 4th cycle of BRK
            State::T5 | State::V0 => {
                todo!("phi2")
            }
            // 5th cycle of BRK
            State::T6 => {
                todo!("phi2")
            }
            // 5th cycle of RMW (zp,X) & (zp),Y (illegal)
            State::SD1 => {
                todo!("phi2")
            }
            // 5th of RMW abs,X&Y, 6th of RMW (zp,X) & (zp),Y
            State::SD2 => {
                todo!("phi2")
            }
            // Terminal state of opcodes that run forever
            State::empty() => {
                todo!("phi2")
            }
            // 2nd-to-last cycle of all(*) opcodes
            State::T0 => {
                todo!("phi2")
            }
            // Last cycle of all(*) opcodes
            State::Tplus | State::T1 => {
                let operation = (OpCode::Load(crate::instructions::LoadOp::LDA), AddressingMode::Immediate);
                self.registers.predecode = operation;

                let is_two_cycle = match self.registers.predecode.1 {
                    AddressingMode::Immediate |
                    AddressingMode::Accumulator => true,

                    AddressingMode::Implicit =>
                        match self.registers.predecode.0 {
                            OpCode::SystemFunc(op) =>
                                match op {
                                    SystemFuncOp::BRK | // 7 cycles
                                    SystemFuncOp::RTI   // 6 cycles
                                        => false,

                                    // <2 cycles
                                    _ => true
                                }

                            OpCode::Stack(op) => {
                                match op {
                                    StackOp::PHA | // 3 cycles
                                    StackOp::PHP | // 3 cycles
                                    StackOp::PLA | // 4 cycles
                                    StackOp::PLP   // 4 cycles
                                        => false,

                                    // <= 2 cycles
                                    _ => true
                                }
                            }

                            OpCode::JumpCall(op) => {
                                match op {
                                    // 6 cycles
                                    JumpCallOp::RTS => false,

                                    // <= 2 cycles
                                    _ => true
                                }
                            }

                            // <=2 cycles
                            _ => true
                        }

                    // >2 cycles or branch
                    _ => false
                };

                if is_two_cycle {
                    self.state = State::T0 | State::T2
                } else {
                    self.state = State::T2
                }
            }
            // Last cycle of branches not taken & no page cross
            State::T1 => {
                todo!("phi2")
            }
            // Clock long-term hold by RES
            State::T0 | State::Tplus => {
                todo!("phi2")
            }
            // 1 cycle after RES of BRK during T5 [V0] cycle
            State::T0 | State::T6 => {
                todo!("phi2")
            }
            // 1 cycle after RES of RMW during SD cycle
            State::T0 | State::SD1 => {
                todo!("phi2")
            }
            // 1 cycle after RES of RMW just after SD cycle
            State::T0 | State::SD2 => {
                todo!("phi2")
            }
            // 2 cycles after RES (still held) of RMW during SD cycle
            State::T0 | State::Tplus | State::SD2 => {
                todo!("phi2")
            }
            // 2 cycles after RES (released) of RMW during SD cycle
            State::Tplus | State::T1 | State::SD2 => {
                todo!("phi2")
            }
        };
        Ok(())
    }
}


fn get_byte_size(addressing_mode: &AddressingMode) -> usize {
    match addressing_mode {
            // Read 0 more bytes
            AddressingMode::Implicit |
            AddressingMode::Accumulator => 0,

            // Read 1 more byte
            AddressingMode::Immediate |
            AddressingMode::ZeroPage |
            AddressingMode::ZeroPageX |
            AddressingMode::ZeroPageY |
            AddressingMode::IndexedIndirect |
            AddressingMode::IndirectIndexed |
            AddressingMode::Relative => 1,

            // Read 2 more bytes
            AddressingMode::Absolute |
            AddressingMode::AbsoluteX |
            AddressingMode::AbsoluteY |
            AddressingMode::Indirect => 2,

            AddressingMode::Unknown => 1,
    }
}