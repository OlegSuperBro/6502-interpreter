use core::panic;
use std::{env, error::Error, fs::{self}, io::{self, Read}, path::Path, rc::Rc};

use crate::{instructions::{OpCode}, parser::parse_instruction, tracer::{Tracer, TracerError}};

mod instructions;
mod parser;
mod errors;
mod tracer;
mod cpu;
mod implementations;

// Currently only used when running https://github.com/Klaus2m5/6502_65C02_functional_tests only because there some thing we need to do, to run tests properly
const RUNNING_TEST: bool = true;
const DEBUG: bool = false;
const STOP_EACH_INSTRUCTION: bool = false;
const STOP_ON_JUMP_INSTRUCTION: bool = false;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        let executable_path = Path::new(args.first().unwrap());
        println!("Usage: {} [-t] path/to/bin", executable_path.file_name().unwrap().to_str().unwrap());
        return Ok(());
    }

    let run_tracer = args.contains(&"-t".to_string());

    let mut cpu = cpu::CPU::default();

    let data = fs::read(args.last().unwrap())?;

    let mut total_ran: u32 = 0;

    cpu.memory = data.as_slice().try_into()?;

    cpu.reset();

    if RUNNING_TEST {
        cpu.registers.program_counter = 0x0400
    }

    if run_tracer {
        let mut tracer = Tracer::new(Rc::new(cpu));

        tracer.init()?;

        tracer.force_redraw()?;


        let mut error: Option<TracerError> = None;

        loop {
            let result = tracer.update();
            if let Ok(stop) = result {
                if !stop { break };
            } else {
                error = Some(result.unwrap_err());
                break;
            }
        }

        if let Some(e) = error {
            return Err(Box::new(e));
        }

        tracer.restore()?;

        return Ok(());
    }

    let mut prev_address = 0u16;

    loop {
        let (offset, instruction);
        let res = parse_instruction(cpu.registers.program_counter, &cpu.memory).inspect_err(|_x| eprintln!("Error during parsing"));

        if let Ok(values) = res {
            (offset, instruction) = values
        } else {
            println!("----------------------------------");

            let reg = &cpu.registers;
            println!("Registry:\n{reg:?}");

            // Some junk :)
            res?;
            return Ok(());
        }

        if DEBUG {
            println!("----------------------------------");
            println!("{instruction:?}");

            let reg = &cpu.registers;
            println!("Before:\n{reg:?}");
        }

        let res = cpu.run_instruction(instruction);
        total_ran += 1;

        if let Ok(increase) = res {
            if increase {
                cpu.registers.program_counter = cpu.registers.program_counter.wrapping_add(offset as u16);
            }
        } else {
            println!("----------------------------------");
            println!("{instruction:?}");

            let reg = &cpu.registers;
            println!("Registry:\n{reg:?}");

            res?;
        }

        if DEBUG {
            let _ = fs::create_dir("memdump");
            let reg = &cpu.registers;
            println!("After:\n{reg:?}");

            if STOP_EACH_INSTRUCTION {
                let _ = io::stdin().read(&mut [0u8]);
            }
            if STOP_ON_JUMP_INSTRUCTION
                && instruction.opcode == OpCode::JumpCall(instructions::JumpCallOp::JMP) {
                    let _ = io::stdin().read(&mut [0u8]);
                }
        }

        if prev_address == cpu.registers.program_counter {
            println!("Stuck at address {prev_address:#x} after {total_ran}");
            panic!("Stuck at address {prev_address:#x} after {total_ran}");
        }

        prev_address = cpu.registers.program_counter;
    }
}
