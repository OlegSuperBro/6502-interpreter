use crate::{
    cpu::{CPU, ProcessOpcode, ProcessorStatus}, errors::ExecutionError, instructions::{self, ArithmeticOp}
};

impl ProcessOpcode<ArithmeticOp> for CPU {
    fn process(
        &mut self,
        instruction: crate::instructions::Instruction,
        op: ArithmeticOp,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mode = instruction.addressing_mode;
        let value = instruction.value;

        let mask_zn = ProcessorStatus::ZeroFlag | ProcessorStatus::NegativeFlag;

        match op {
            instructions::ArithmeticOp::ADC => {
                let mask = mask_zn | ProcessorStatus::CarryFlag | ProcessorStatus::OverflowFlag;

                let old_accumulator = self.registers.accumulator;

                if (self.registers.processor_status & ProcessorStatus::DecimalMode).is_empty() {
                    let value = self.get_value(mode, value)?;
                    let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag)
                        .is_empty()
                    {
                        0
                    } else {
                        1
                    };

                    let (operand, operand_carry) = value.overflowing_add(carry);
                    let (final_result, result_carry) =
                        self.registers.accumulator.overflowing_add(operand);

                    let flags = CPU::get_flags(
                        Some(old_accumulator),
                        Some(operand.wrapping_sub(carry)),
                        &final_result,
                        Some(mask & !ProcessorStatus::CarryFlag),
                    ) | if result_carry || operand_carry {
                        ProcessorStatus::CarryFlag
                    } else {
                        ProcessorStatus::empty()
                    };

                    self.set_flags(flags, mask);
                    self.registers.accumulator = final_result;
                } else {
                    let value = CPU::hex2bcd(self.get_value(mode, value)?)?;
                    // println!("adc test {value}");
                    let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag)
                        .is_empty()
                    {
                        0
                    } else {
                        1
                    };

                    let (operand, _operand_carry) = value.overflowing_add(carry);
                    let (mut final_result, _result_carry) =
                        CPU::hex2bcd(self.registers.accumulator)?.overflowing_add(operand);

                    let mut flags = CPU::get_flags(
                        Some(old_accumulator),
                        Some(operand.wrapping_sub(carry)),
                        &final_result,
                        Some(mask & !ProcessorStatus::CarryFlag),
                    );

                    // println!("adc test2 {final_result}");
                    if final_result > 99 {
                        flags |= ProcessorStatus::CarryFlag;
                        final_result -= 100;
                    }

                    self.set_flags(flags, mask);

                    // println!("adc test3 {final_result} {:X}", CPU::bcd2hex(final_result)?);
                    self.registers.accumulator = CPU::bcd2hex(final_result)?;
                };
            }

            instructions::ArithmeticOp::SBC => {
                let mask = mask_zn | ProcessorStatus::CarryFlag | ProcessorStatus::OverflowFlag;

                let old_accumulator = self.registers.accumulator;

                if (self.registers.processor_status & ProcessorStatus::DecimalMode).is_empty() {
                    let value: u8 = self.get_value(mode, value)?;
                    let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag)
                        .is_empty()
                    {
                        0
                    } else {
                        1
                    };

                    let (operand, operand_carry) = (!value).overflowing_add(carry);
                    let (final_result, new_carry) =
                        self.registers.accumulator.overflowing_add(operand);

                    let flags = CPU::get_flags(
                        Some(old_accumulator),
                        Some(!value),
                        &final_result,
                        Some(mask & !ProcessorStatus::CarryFlag),
                    ) | if new_carry || operand_carry {
                        ProcessorStatus::CarryFlag
                    } else {
                        ProcessorStatus::empty()
                    };

                    self.set_flags(flags, mask);
                    self.registers.accumulator = final_result;
                } else {
                    let value = CPU::hex2bcd(self.get_value(mode, value)?)?;
                    let carry = if (self.registers.processor_status & ProcessorStatus::CarryFlag)
                        .is_empty()
                    {
                        0
                    } else {
                        1
                    };

                    let operand = CPU::bcd_not(value)? + carry;

                    let acc = CPU::hex2bcd(self.registers.accumulator)?;

                    let (final_result, result_carry) = CPU::bcd_overflowing_add(acc, operand)?;

                    let mut flags = CPU::get_flags(
                        Some(old_accumulator),
                        Some(!value),
                        &final_result,
                        Some(mask & !ProcessorStatus::CarryFlag),
                    );

                    if result_carry {
                        flags |= ProcessorStatus::CarryFlag;
                    }

                    self.set_flags(flags, mask);
                    self.registers.accumulator = CPU::bcd2hex(final_result)?;
                };
            }

            instructions::ArithmeticOp::CMP => {
                let old_value = self.registers.accumulator;

                let value = self.get_value(mode, value)?;

                let new_value = old_value.wrapping_sub(value);

                self.set_flags(
                    CPU::get_flags(Some(old_value), Some(value), &new_value, Some(mask_zn))
                        | if old_value >= value {
                            ProcessorStatus::CarryFlag
                        } else {
                            ProcessorStatus::empty()
                        },
                    mask_zn | ProcessorStatus::CarryFlag,
                );
            }
            instructions::ArithmeticOp::CPX => {
                let old_value = self.registers.x;

                let value = self.get_value(mode, value)?;

                let new_value = old_value.wrapping_sub(value);

                self.set_flags(
                    CPU::get_flags(Some(old_value), Some(value), &new_value, Some(mask_zn))
                        | if old_value >= value {
                            ProcessorStatus::CarryFlag
                        } else {
                            ProcessorStatus::empty()
                        },
                    mask_zn | ProcessorStatus::CarryFlag,
                );
            }
            instructions::ArithmeticOp::CPY => {
                let old_value = self.registers.y;

                let value = self.get_value(mode, value)?;

                let new_value = old_value.wrapping_sub(value);

                self.set_flags(
                    CPU::get_flags(Some(old_value), Some(value), &new_value, Some(mask_zn))
                        | if old_value >= value {
                            ProcessorStatus::CarryFlag
                        } else {
                            ProcessorStatus::empty()
                        },
                    mask_zn | ProcessorStatus::CarryFlag,
                );
            }
        }
        Ok(true)
    }
}

impl CPU {
    fn hex2bcd(num: u8) -> Result<u8, ExecutionError> {
        // println!("hex2bcd {num:#X}");
        if (num & 0xF0) <= 0x99 &&
           (num & 0x0F) <= 0x09{
            Ok(num - 6 * (num >> 4))
        } else {
            Err(ExecutionError::InvalidBcdValue(num))
        }
    }

    fn bcd2hex(num: u8) -> Result<u8, ExecutionError> {
        // println!("bcd2hex {num}");
        if num <= 99 {
            // Ok(num + 6 * (num >> 4))
            Ok(u8::from_str_radix(format!("{num}").as_str(), 16).unwrap())
        } else {
            Err(ExecutionError::InvalidBcdValue(num))
        }
    }

    fn bcd_not(num: u8) -> Result<u8, ExecutionError> {
        if num > 99 {
            return Err(ExecutionError::InvalidBcdValue(num));
        }

        Ok(100 - num)
    }

    fn bcd_overflowing_add(value: u8, operand: u8) -> Result<(u8, bool), ExecutionError> {
        if value > 99 {
            return Err(ExecutionError::InvalidBcdValue(value));
        }
        if operand > 99 {
            return Err(ExecutionError::InvalidBcdValue(operand));
        }

        let mut result = value + operand;
        let mut carry = false;

        if result > 99 {
            carry = true;
            result -= 100;
        }

        Ok((result, carry))
    }
}