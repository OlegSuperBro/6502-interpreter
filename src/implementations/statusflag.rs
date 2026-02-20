use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, StatusFlagOp},
};

impl ProcessOpcode<StatusFlagOp> for CPU {
    fn process(
        &mut self,
        _instruction: crate::instructions::Instruction,
        op: StatusFlagOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
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
        Ok(true)
    }
}
