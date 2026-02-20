use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, BranchOp},
};

impl ProcessOpcode<BranchOp> for CPU {
    fn process(
        &mut self,
        instruction: crate::instructions::Instruction,
        op: BranchOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        match op {
            instructions::BranchOp::BNE => {
                if (self.registers.processor_status & ProcessorStatus::ZeroFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BEQ => {
                if !(self.registers.processor_status & ProcessorStatus::ZeroFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BPL => {
                if (self.registers.processor_status & ProcessorStatus::NegativeFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BCC => {
                if (self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BCS => {
                if !(self.registers.processor_status & ProcessorStatus::CarryFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BMI => {
                if !(self.registers.processor_status & ProcessorStatus::NegativeFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BVC => {
                if (self.registers.processor_status & ProcessorStatus::OverflowFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }

            instructions::BranchOp::BVS => {
                if !(self.registers.processor_status & ProcessorStatus::OverflowFlag).is_empty() {
                    self.registers.program_counter = self.get_address(mode, value)?;
                    return Ok(false);
                }
                Ok(true)
            }
        }
    }
}
