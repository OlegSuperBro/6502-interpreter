use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, IncDecOp},
};

impl ProcessOpcode<IncDecOp> for CPU {
    fn process(
        &mut self,
        instruction: crate::instructions::Instruction,
        op: IncDecOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag;
        match op {
            instructions::IncDecOp::DEX => {
                let value = self.registers.x.wrapping_sub(1);

                let flags = CPU::get_flags(Some(self.registers.x), None, &value, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.registers.x = value;
            }
            instructions::IncDecOp::DEY => {
                let value = self.registers.y.wrapping_sub(1);

                let flags = CPU::get_flags(Some(self.registers.y), None, &value, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.registers.y = value;
            }
            instructions::IncDecOp::INX => {
                let value = self.registers.x.wrapping_add(1);

                let flags = CPU::get_flags(Some(self.registers.x), None, &value, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.registers.x = value;
            }
            instructions::IncDecOp::INY => {
                let value = self.registers.y.wrapping_add(1);

                let flags = CPU::get_flags(Some(self.registers.y), None, &value, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.registers.y = value;
            }

            instructions::IncDecOp::INC => {
                let mem_value = self.get_value(mode, value)?;

                let result = mem_value.wrapping_add(1);

                let flags = CPU::get_flags(Some(mem_value), None, &result, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.set_value(mode, value, result)?;
            }

            instructions::IncDecOp::DEC => {
                let mem_value = self.get_value(mode, value)?;

                let result = mem_value.wrapping_sub(1);

                let flags = CPU::get_flags(Some(mem_value), None, &result, Some(mask_zn));

                self.set_flags(flags, mask_zn);
                self.set_value(mode, value, result)?;
            }
        }
        Ok(true)
    }
}
