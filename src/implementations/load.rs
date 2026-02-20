use crate::{cpu::{CPU, ProcessOpcode, ProcessorStatus}, instructions::{self, LoadOp}};

impl ProcessOpcode<LoadOp> for CPU {
    fn process(&mut self, instruction: crate::instructions::Instruction, op: LoadOp) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus:: NegativeFlag;
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

        Ok(true)
    }
}