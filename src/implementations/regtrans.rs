use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, RegTransOp},
};

impl ProcessOpcode<RegTransOp> for CPU {
    fn process(
        &mut self,
        _instruction: crate::instructions::Instruction,
        op: RegTransOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag;
        match op {
            instructions::RegTransOp::TYA => {
                let value = self.registers.y;

                let flags = CPU::get_flags(
                    Some(self.registers.accumulator),
                    None,
                    &value,
                    Some(mask_zn),
                );

                self.set_flags(flags, mask_zn);
                self.registers.accumulator = value;
            }

            instructions::RegTransOp::TAY => {
                let value = self.registers.accumulator;

                let flags = CPU::get_flags(Some(self.registers.y), None, &value, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.registers.y = value;
            }

            instructions::RegTransOp::TAX => {
                let value = self.registers.accumulator;

                let flags = CPU::get_flags(Some(self.registers.x), None, &value, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.registers.x = value;
            }

            instructions::RegTransOp::TXA => {
                let value = self.registers.x;

                let flags = CPU::get_flags(
                    Some(self.registers.accumulator),
                    None,
                    &value,
                    Some(mask_zn),
                );

                self.set_flags(flags, mask_zn);
                self.registers.accumulator = value;
            }
        }

        Ok(true)
    }
}
