use crate::{
    cpu::{CPU, ProcessOpcode},
    instructions::{self, JumpCallOp},
};

impl ProcessOpcode<JumpCallOp> for CPU {
    fn process(
        &mut self,
        instruction: crate::instructions::Instruction,
        op: JumpCallOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        match op {
            instructions::JumpCallOp::JMP => {
                let value = self.get_address(mode, value)?;
                self.registers.program_counter = value;
            }

            instructions::JumpCallOp::JSR => {
                let value = self.get_address(mode, value)?;

                let target = self.registers.program_counter.wrapping_add(2);

                self.stack_push(((target >> 8) & 0xFF) as u8);
                self.stack_push((target & 0xFF) as u8);

                self.registers.program_counter = value;

            }

            instructions::JumpCallOp::RTS => {
                self.registers.program_counter = ((self.stack_pull() as u16)
                    + ((self.stack_pull() as u16) << 8))
                    .wrapping_add(1);
            }
        }
        Ok(false)
    }
}
