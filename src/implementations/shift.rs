use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus},
    instructions::{self, AddressingMode, ShiftOp},
};

impl ProcessOpcode<ShiftOp> for CPU {
    fn process(
        &mut self,
        instruction: crate::instructions::Instruction,
        op: ShiftOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag;
        match op {
            instructions::ShiftOp::ASL => {
                let mask = mask_zn | ProcessorStatus::CarryFlag;

                let op_value = self.get_value(mode, value)?;

                let (result, _carry) = op_value.overflowing_shl(1);

                let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask))
                    | if op_value & 0b10000000 != 0 {
                        ProcessorStatus::CarryFlag
                    } else {
                        ProcessorStatus::empty()
                    };

                if mode == AddressingMode::Accumulator {
                    self.set_value(mode, 0, result)?;
                } else {
                    self.set_value(mode, value, result)?;
                }

                self.set_flags(flags, mask);
            }

            instructions::ShiftOp::LSR => {
                let mask = mask_zn | ProcessorStatus::CarryFlag;

                let op_value = self.get_value(mode, value)?;

                let (result, _carry) = op_value.overflowing_shr(1);

                let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask))
                    | if op_value & 0b00000001 != 0 {
                        ProcessorStatus::CarryFlag
                    } else {
                        ProcessorStatus::empty()
                    };

                if mode == AddressingMode::Accumulator {
                    self.set_value(mode, 0, result)?;
                } else {
                    self.set_value(mode, value, result)?;
                }

                self.set_flags(flags, mask);
            }

            instructions::ShiftOp::ROL => {
                let mask = mask_zn | ProcessorStatus::CarryFlag;

                let op_value = self.get_value(mode, value)?;

                let (mut result, _carry) = op_value.overflowing_shl(1);

                result |=
                    if (self.registers.processor_status & ProcessorStatus::CarryFlag).bits() != 0 {
                        1
                    } else {
                        0
                    };

                let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask))
                    | if op_value & 0b10000000 != 0 {
                        ProcessorStatus::CarryFlag
                    } else {
                        ProcessorStatus::empty()
                    };

                if mode == AddressingMode::Accumulator {
                    self.set_value(mode, 0, result)?;
                } else {
                    self.set_value(mode, value, result)?;
                }

                self.set_flags(flags, mask);
            }

            instructions::ShiftOp::ROR => {
                let mask = mask_zn | ProcessorStatus::CarryFlag;

                let op_value = self.get_value(mode, value)?;

                let (mut result, _carry) = op_value.overflowing_shr(1);

                result |=
                    if (self.registers.processor_status & ProcessorStatus::CarryFlag).bits() != 0 {
                        1 << 7
                    } else {
                        0 << 7
                    };

                let flags = CPU::get_flags(Some(op_value), None, &result, Some(mask))
                    | if op_value & 0b00000001 != 0 {
                        ProcessorStatus::CarryFlag
                    } else {
                        ProcessorStatus::empty()
                    };

                if mode == AddressingMode::Accumulator {
                    self.set_value(mode, 0, result)?;
                } else {
                    self.set_value(mode, value, result)?;
                }

                self.set_flags(flags, mask);
            }
        }
        Ok(true)
    }
}
