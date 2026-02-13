use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, LogicOp},
};

impl ProcessOpcode<LogicOp> for CPU {
    fn process(
        &mut self,
        instruction: crate::instructions::Instruction,
        op: LogicOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag;
        match op {
            instructions::LogicOp::EOR => {
                let value = self.get_value(mode, value)?;

                let result = self.registers.accumulator ^ value;

                let flags = CPU::get_flags(
                    Some(self.registers.accumulator),
                    Some(value),
                    &result,
                    Some(mask_zn),
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
                    Some(mask_zn),
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
                    Some(mask_zn),
                );

                self.set_flags(flags, mask_zn);
                self.registers.accumulator = result;
            }
        }

        Ok(true)
    }
}
