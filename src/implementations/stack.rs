use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, StackOp},
};

impl ProcessOpcode<StackOp> for CPU {
    fn process(
        &mut self,
        _instruction: crate::instructions::Instruction,
        op: StackOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag;

        match op {
            instructions::StackOp::TXS => {
                self.registers.stack_pointer = self.registers.x;
            }

            instructions::StackOp::TSX => {
                self.registers.x = self.registers.stack_pointer;

                let flags = CPU::get_flags(None, None, &self.registers.x, Some(mask_zn));

                self.set_flags(flags, mask_zn);
            }

            instructions::StackOp::PHA => {
                self.stack_push(self.registers.accumulator);
            }

            instructions::StackOp::PLA => {
                self.registers.accumulator = self.stack_pull();

                let flags = CPU::get_flags(None, None, &self.registers.accumulator, Some(mask_zn));

                self.set_flags(flags, mask_zn);
            }

            instructions::StackOp::PHP => {
                // 1. It should always should return B flag. AFAIK it's not really a flag but some internal signal that isn't stored anywhere.
                // 2. 5th bit is unused cuz status registry is actually just 6 1bit registries, and it's 1 just because it simplifies saving in stack or smt
                self.stack_push(
                    (self.registers.processor_status | ProcessorStatus::BreakCommand).bits()
                        | 1 << 5,
                );
            }

            instructions::StackOp::PLP => {
                let flags = ProcessorStatus::from_bits_truncate(self.stack_pull());
                self.set_flags(flags, ProcessorStatus::all());
            }
        }
        Ok(true)
    }
}
