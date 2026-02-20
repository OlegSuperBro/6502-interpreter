use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, SystemFuncOp},
};

impl ProcessOpcode<SystemFuncOp> for CPU {
    fn process(
        &mut self,
        _instruction: crate::instructions::Instruction,
        op: SystemFuncOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        match op {
            instructions::SystemFuncOp::BRK => {
                // there's bug in 6502 that makes "BRK" instruction "2 byte" long, when 2nd byte is ignored in any way.
                // It was actually cool that some programs used this
                self.stack_push((self.registers.program_counter.wrapping_add(2) >> 8) as u8);
                self.stack_push((self.registers.program_counter.wrapping_add(2) & 0xFF) as u8);
                self.stack_push(
                    (self.registers.processor_status | ProcessorStatus::BreakCommand).bits()
                        | 1 << 5,
                );

                self.registers.program_counter =
                    (self.memory[0xFFFE] as u16) + ((self.memory[0xFFFF] as u16) << 8);

                self.set_flags(
                    ProcessorStatus::InterruptDisable,
                    ProcessorStatus::InterruptDisable,
                );

                Ok(false)
            }

            instructions::SystemFuncOp::RTI => {
                self.registers.processor_status =
                    ProcessorStatus::from_bits_truncate(self.stack_pull());
                self.registers.program_counter =
                    (self.stack_pull() as u16) + ((self.stack_pull() as u16) << 8);
                Ok(false)
            }

            instructions::SystemFuncOp::NOP => Ok(true),
        }
    }
}
