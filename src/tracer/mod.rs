use std::{cell::RefCell, error::Error, io::Stdout, rc::Rc, time::Duration, vec};

use crossterm::{
    event::{self, KeyCode, KeyEvent},
    terminal::size,
};
use ratatui::{
    Terminal,
    layout::{Layout, Rect},
    prelude::CrosstermBackend,
    style::{Color, Style, Stylize},
    text::{Line, Span, Text},
    widgets::{Block, Padding, Paragraph},
};

use crate::{
    CPU, ProcessorStatus,
    instructions::{self, AddressingMode, Instruction, JumpCallOp, OpCode},
    parser::parse_instruction,
    tracer::errors::{CommandError, HotkeyError},
};

use macros::{add_find_by, add_info, add_parse_executor};

pub use errors::TracerError;

mod errors;
pub mod macros;

type InitializedTerminal = Terminal<CrosstermBackend<Stdout>>;

enum Command {
    SetMem,
    SetReg,
    Reset,
    Run,
    RunUntil,
    RunSilent,
    RunUntilSilent,
    MoveCursorTo,

    ExecAsm,

    FollowCursor,
    Quit,
}

add_find_by! {
    Command Result<Command, CommandError>{
        name<&str> => {
            "setmem" => Ok(Command::SetMem),
            "setreg" => Ok(Command::SetReg),
            "reset" => Ok(Command::Reset),

            "run" => Ok(Command::Run),
            "run_until" => Ok(Command::RunUntil),
            "run_silent" => Ok(Command::RunSilent),
            "run_until_silent" => Ok(Command::RunUntilSilent),
            "follow" => Ok(Command::FollowCursor),
            "move" | "mov" => Ok(Command::MoveCursorTo),
            "exec" | "asm" => Ok(Command::ExecAsm),

            "q" | "quit" => Ok(Command::Quit)
        }
    } => Err(CommandError::InvalidCommand)
}

add_parse_executor! {
    Command (args &[&str], tracer &mut Tracer) Result<(), CommandError> {
        Command::SetMem => {
            if args.len() > 2 {
                let target_addr_arg = args.first().unwrap();
                let data_args = &args[1..];

                let target_addr = parse_addr_argument(tracer, target_addr_arg).map_err(|_e| CommandError::InvalidArgument)?;

                // that's some junk, but it works
                return data_args.iter().enumerate().try_fold((), |_, (index, data)| {
                    if target_addr as usize + index > u16::MAX as usize {
                        return Err(CommandError::InvalidRange);
                    }

                    if let Ok(value) = parse_value_argument(data) &&
                        let Some(cpu) = Rc::get_mut(&mut tracer.cpu)
                        && value <= u8::MAX as usize {
                            cpu.memory.data[target_addr as usize + index] = value as u8;
                        }

                    Ok(())
                })
            }
            Ok(())
        },

        Command::SetReg => {
            if args.len() == 2 {
                let value = parse_value_argument(args[1]).map_err(|_e| CommandError::InvalidArgument)?;

                if let Some(cpu) = Rc::get_mut(&mut tracer.cpu) {
                    match args[0] {
                        "pc" => {
                            cpu.registers.program_counter = value.try_into().map_err(|_e| CommandError::InvalidArgument)?;
                        }
                        "sp" => {
                            cpu.registers.stack_pointer = value.try_into().map_err(|_e| CommandError::InvalidArgument)?;
                        }
                        "a" => {
                            cpu.registers.accumulator = value.try_into().map_err(|_e| CommandError::InvalidArgument)?;
                        }
                        "x" => {
                            cpu.registers.x = value.try_into().map_err(|_e| CommandError::InvalidArgument)?;
                        }
                        "y" => {
                            cpu.registers.y = value.try_into().map_err(|_e| CommandError::InvalidArgument)?;
                        }

                        _ => todo!("invalid registry")
                    }
                }
            }

            Ok(())
        },

        Command::Reset => {
            if let Some(cpu) = Rc::get_mut(&mut tracer.cpu) {
                cpu.reset();
            }

            Ok(())
        },

        Command::Run => {
            if !args.is_empty() {
                let target_address_arg = args.first().unwrap();
                let target_address = parse_addr_argument(tracer, target_address_arg).map_err(|_e| CommandError::InvalidArgument)?;

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;

                    let pc = tracer.cpu.registers.program_counter;

                    if pc >= target_address {
                        return false;
                    }

                    true
                }));
            }


            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::RunUntil => {
            if !args.is_empty() {
                let target_address_arg = args.first().unwrap();
                let target_address = parse_addr_argument(tracer, target_address_arg).map_err(|_e| CommandError::InvalidArgument)?;

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;

                    let pc = tracer.cpu.registers.program_counter;

                    if pc >= target_address {
                        return false;
                    }

                    true
                }));
            }


            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::RunSilent => {
            if !args.is_empty() {
                let target_address: u16 = parse_addr_argument(tracer, args.first().unwrap()).map_err(|_e| CommandError::InvalidArgument)?;

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let pc = tracer.cpu.registers.program_counter;

                    let target_address = pc + target_address;

                    if pc >= target_address {
                        return false;
                    }

                    true
                }));
            }

            tracer.current_state = States::RunningSilent;
            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::RunUntilSilent => {
            if !args.is_empty() {
                let target_address: u16 = parse_addr_argument(tracer, args.first().unwrap()).map_err(|_e| CommandError::InvalidArgument)?;

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;

                    let pc = tracer.cpu.registers.program_counter;

                    if pc == target_address {
                        return false;
                    }

                    true
                }));
                tracer.current_state = States::RunningSilent;
                tracer.is_cpu_running = true;

                return Ok(());
            }


            Err(CommandError::InvalidArgument)
        },

        Command::FollowCursor => {
            if !args.is_empty() {
                match *args.first().unwrap() {
                    "c" => {
                        tracer.memory_follow_mode = MemoryCursorTracking::Cursor;
                    }
                    "pc" => {
                        tracer.memory_follow_mode = MemoryCursorTracking::ProgramCounter;
                    }
                    "sp" => {
                        tracer.memory_follow_mode = MemoryCursorTracking::StackPointer;
                    }
                    _ => {
                        todo!("invalid argument")
                    }
                }
            }

            Ok(())
        },

        Command::MoveCursorTo => {
            if !args.is_empty() {
                let target_address: u16 = parse_addr_argument(tracer, args.first().unwrap()).map_err(|_e| CommandError::InvalidArgument)?;

                tracer.memory_cursor_pos = target_address as usize;
            }
            Ok(())
        },

        Command::ExecAsm => {
            if !args.is_empty() {
                let strings = args.iter().fold(String::new(), |mut acc, x| {
                    acc.push_str(x);
                    acc.push(' ');
                    acc
                });

                let instructions_strings = strings.split(";").collect::<Vec<&str>>();

                let mut instructions: Vec<Instruction> = Vec::new();

                for string in instructions_strings {
                    let instruction = Instruction::try_from(string).map_err(|_e| CommandError::InvalidArgument)?;
                    instructions.push(instruction);
                }

                if let Some(cpu) = Rc::get_mut(&mut tracer.cpu) {
                    instructions.iter().try_for_each(|x| {
                        cpu.run_instruction(*x).map(|_| ()).map_err(CommandError::FailedToExecute)?;

                        if !tracer.registry_history.is_empty() {
                            let last_entry = tracer.registry_history.last().unwrap();

                            if (last_entry.status.bits() != cpu.registers.processor_status.bits())
                                || (last_entry.a != cpu.registers.accumulator)
                                || (last_entry.x != cpu.registers.x)
                                || (last_entry.y != cpu.registers.y)
                            {
                                tracer.registry_history.push(ProcessorStatusHistoryEntry {
                                    addr: 0xFFFF,
                                    a: cpu.registers.accumulator,
                                    x: cpu.registers.x,
                                    y: cpu.registers.y,
                                    status: cpu.registers.processor_status,
                                });
                            }
                        } else {
                            tracer.registry_history.push(ProcessorStatusHistoryEntry {
                                addr: 0xFFFF,
                                a: cpu.registers.accumulator,
                                x: cpu.registers.x,
                                y: cpu.registers.y,
                                status: cpu.registers.processor_status,
                            });
                        }

                        Ok(())
                    })?
                }
            }

            Ok(())
        },

        Command::Quit => {
            tracer.current_state = States::Exit;
            Ok(())
        }
    } => Err(CommandError::InvalidCommand)
}

fn parse_addr_argument(tracer: &Tracer, arg: &str) -> Result<u16, Box<dyn Error>> {
    let target_address_arg = arg;

    if target_address_arg.starts_with("0x") {
        if let Some(stripped) = target_address_arg.strip_prefix("0x") {
            // TODO replace .unwrap() with check
            Ok(u16::from_str_radix(stripped, 16).unwrap())
        } else {
            todo!("Couldn't parse argument")
        }
    } else if target_address_arg == "$$" {
        Ok(tracer.memory_cursor_pos as u16)
    } else if target_address_arg == "pc" {
        Ok(tracer.cpu.registers.program_counter)
    } else if target_address_arg == "sp" {
        Ok(0x0100 + tracer.cpu.registers.stack_pointer as u16)
    } else {
        // TODO replace .unwrap() with check
        Ok(target_address_arg.parse::<u16>().unwrap())
    }
}

fn parse_value_argument(arg: &str) -> Result<usize, Box<dyn Error>> {
    let target_address_arg = arg;

    if target_address_arg.starts_with("0x") {
        if let Some(stripped) = target_address_arg.strip_prefix("0x") {
            // TODO replace .unwrap() with check
            Ok(usize::from_str_radix(stripped, 16).unwrap())
        } else {
            todo!("Couldn't parse argument")
        }
    } else {
        // TODO replace .unwrap() with check
        Ok(target_address_arg.parse::<usize>().unwrap())
    }
}

impl Command {
    pub fn execute(input: &str, tracer: &mut Tracer) -> Result<(), CommandError> {
        let splitted_input: Vec<&str> = input.split(" ").collect();

        let command = Command::find_by_name(splitted_input.first().unwrap())?;

        let args = &splitted_input[1..];

        Command::parse_and_run(&command, args, tracer)
    }
}

enum Hotkeys {
    MoveCursorUp,
    MoveCursorDown,
    MoveCursorLeft,
    MoveCursorRight,

    RunOnce,
    Stop,
    Pause,

    InputCommand,
    Quit,
}

add_find_by! {
    Hotkeys Result<Hotkeys, HotkeyError> {
        code<&KeyCode> => {
            KeyCode::Char(':') => Ok(Hotkeys::InputCommand),
            KeyCode::Char('q') => Ok(Hotkeys::Quit),
            KeyCode::Left => Ok(Hotkeys::MoveCursorLeft),
            KeyCode::Right => Ok(Hotkeys::MoveCursorRight),
            KeyCode::Down => Ok(Hotkeys::MoveCursorDown),
            KeyCode::Up => Ok(Hotkeys::MoveCursorUp),
            KeyCode::Enter => Ok(Hotkeys::RunOnce),
            KeyCode::Char(' ') => Ok(Hotkeys::RunOnce),
            KeyCode::Char('s') => Ok(Hotkeys::Stop),
            KeyCode::Char('p') => Ok(Hotkeys::Pause)
        }
    } => Err(HotkeyError::InvalidHotkey)
}

add_parse_executor! {
    Hotkeys (tracer &mut Tracer) Result<(), HotkeyError> {
        Hotkeys::InputCommand => {
            tracer.is_inputting_command = true;
            tracer.command_input = String::from(":");
            Ok(())
        },
        Hotkeys::Quit => {
            tracer.current_state = States::Exit;
            Ok(())
        },
        Hotkeys::MoveCursorLeft => {
            match tracer.active_area {
                Areas::Memory => {
                    if tracer.memory_cursor_pos != 0 {
                        tracer.memory_cursor_pos -= 1;
                    }
                }
                _ => todo!("move cursor left")
            }
            Ok(())
        },
        Hotkeys::MoveCursorRight => {
            match tracer.active_area {
                Areas::Memory => {
                    if tracer.memory_cursor_pos != 0xFFFF {
                        tracer.memory_cursor_pos += 1;
                    }
                }
                _ => todo!("move cursor right")
            }
            Ok(())
        },
        Hotkeys::MoveCursorUp => {
            match tracer.active_area {
                Areas::Memory => {
                    if tracer.memory_cursor_pos > 0x000F {
                        tracer.memory_cursor_pos -= 0x10;
                    }
                }
                _ => todo!("move cursor up")
            }
            Ok(())
        },
        Hotkeys::MoveCursorDown => {
            match tracer.active_area {
                Areas::Memory => {
                    if tracer.memory_cursor_pos < 0xFFF0 {
                        tracer.memory_cursor_pos += 0x10;
                    }
                }
                _ => todo!("move cursor down")
            }
            Ok(())
        },
        Hotkeys::RunOnce => {
            if let Some(cpu) = Rc::get_mut(&mut tracer.cpu) {
                let _ = cpu.once();
            } else {
                todo!("Couldn't get cpu from rc pointer")
            }
            Ok(())
        },
        Hotkeys::Stop => {
            tracer.is_cpu_running = false;
            tracer.running_predicate = None;
            Ok(())
        },
        Hotkeys::Pause => {
            tracer.is_cpu_paused = !tracer.is_cpu_paused;
            Ok(())
        }
    } => todo!("hotkey")
}

pub enum Areas {
    Dissasembly,
    Memory,
    Registry,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum States {
    Waiting,
    RunningSilent,
    Exit,
}

pub struct ProcessorStatusHistoryEntry {
    addr: u16,
    a: u8,
    x: u8,
    y: u8,
    status: ProcessorStatus,
}

type CpuRunningPredicate = Option<Box<dyn Fn(&Tracer) -> bool>>;

pub struct Tracer {
    terminal: Option<Rc<RefCell<InitializedTerminal>>>,

    parsed_instructions: Vec<(usize, Instruction)>,

    pub cpu: Rc<CPU>,
    pub is_cpu_running: bool,
    pub is_cpu_paused: bool,
    pub running_predicate: CpuRunningPredicate,

    pub commands_history: Vec<String>,
    pub command_history_index_offset: usize,
    pub command_input: String,
    pub command_error: Option<Box<dyn Error>>,
    pub is_inputting_command: bool,

    pub active_area: Areas,

    pub current_state: States,

    pub registry_history: Vec<ProcessorStatusHistoryEntry>,

    pub memory_cursor_pos: usize,
    pub memory_follow_mode: MemoryCursorTracking,
}

pub enum MemoryCursorTracking {
    Cursor,
    ProgramCounter,
    StackPointer,
    Static(u16),
}

enum Layouts {
    Compact,
    Full,
}

impl Tracer {
    pub fn new(cpu: Rc<CPU>) -> Self {
        Tracer {
            cpu,
            parsed_instructions: vec![],
            is_cpu_running: false,
            is_cpu_paused: false,
            running_predicate: None,
            commands_history: vec![],
            command_history_index_offset: 0,
            command_error: None,
            is_inputting_command: false,

            registry_history: Vec::new(),
            active_area: Areas::Memory,
            current_state: States::Waiting,
            command_input: String::new(),

            memory_cursor_pos: 0x00,
            memory_follow_mode: MemoryCursorTracking::Cursor,
            terminal: None,
        }
    }

    pub fn update_assembly(&mut self) {
        self.parsed_instructions = vec![];

        let mut index = 0usize;

        loop {
            if index >= 0xFFFF {
                break;
            }

            if let Ok((offset, instruction)) =
                parse_instruction(index as u16, &self.cpu.memory.data)
            {
                self.parsed_instructions.push((index, instruction));
                index += offset;
            } else {
                self.parsed_instructions.push((
                    index,
                    Instruction {
                        opcode: crate::instructions::OpCode::Unknown,
                        addressing_mode: instructions::AddressingMode::Unknown,
                        value: 0,
                    },
                ));
                index += 1;
            }
        }
    }

    pub fn init(&mut self) -> Result<(), TracerError> {
        self.terminal = Some(Rc::new(RefCell::new(ratatui::init())));
        self.update_assembly();
        Ok(())
    }

    pub fn restore(&mut self) -> Result<(), TracerError> {
        ratatui::restore();
        Ok(())
    }

    pub fn update(&mut self) -> Result<bool, TracerError> {
        if self.current_state == States::Exit {
            return Ok(false);
        }

        let had_inputs = self.process_inputs()?;

        if had_inputs || (self.current_state != States::RunningSilent) {
            self.draw()?;
        }

        if self.is_cpu_running && !self.is_cpu_paused {
            if let Some(cpu) = Rc::get_mut(&mut self.cpu) {
                if let Err(e) = cpu.once() {
                    todo!("Error occured during cpu run {e}")
                }
            }

            if let Some(predicate) = &self.running_predicate
                && !predicate(self)
            {
                self.is_cpu_running = false;
                self.running_predicate = None;

                if self.current_state == States::RunningSilent {
                    self.current_state = States::Waiting;
                }
            }
        }

        if let Some(cpu) = Rc::get_mut(&mut self.cpu) {
            if !self.registry_history.is_empty() {
                let last_entry = self.registry_history.last().unwrap();

                if (last_entry.status.bits() != cpu.registers.processor_status.bits())
                    || (last_entry.a != cpu.registers.accumulator)
                    || (last_entry.x != cpu.registers.x)
                    || (last_entry.y != cpu.registers.y)
                {
                    self.registry_history.push(ProcessorStatusHistoryEntry {
                        addr: cpu.registers.program_counter,
                        a: cpu.registers.accumulator,
                        x: cpu.registers.x,
                        y: cpu.registers.y,
                        status: cpu.registers.processor_status,
                    });
                }
            } else {
                self.registry_history.push(ProcessorStatusHistoryEntry {
                    addr: cpu.registers.program_counter,
                    a: cpu.registers.accumulator,
                    x: cpu.registers.x,
                    y: cpu.registers.y,
                    status: cpu.registers.processor_status,
                });
            }
        }

        Ok(true)
    }

    pub fn force_redraw(&mut self) -> Result<(), TracerError> {
        self.draw()
    }

    fn process_inputs(&mut self) -> Result<bool, TracerError> {
        if let Ok(res) = crossterm::event::poll(Duration::from_nanos(1))
            && !res
        {
            return Ok(false);
        }

        if self.is_inputting_command {
            self.process_command_input().map(|_v| true)
        } else {
            self.process_hotkey().map(|_v| true)
        }
    }

    fn process_hotkey(&mut self) -> Result<(), TracerError> {
        if let event::Event::Key(key_event) =
            crossterm::event::read().map_err(TracerError::IOError)?
        {
            let result = self.process_key(key_event);

            if let Err(e) = result {
                if let TracerError::HotkeyError(HotkeyError::InvalidHotkey) = e {
                    return Ok(());
                } else {
                    return Err(e);
                }
            }
        };

        Ok(())
    }

    fn process_key(&mut self, key_event: KeyEvent) -> Result<(), TracerError> {
        let hotkey = Hotkeys::find_by_code(&key_event.code).map_err(TracerError::HotkeyError)?;

        Hotkeys::parse_and_run(&hotkey, self).map_err(TracerError::HotkeyError)?;
        Ok(())
    }

    fn process_command_input(&mut self) -> Result<(), TracerError> {
        match crossterm::event::read().map_err(TracerError::IOError)? {
            event::Event::Key(key_event) => {
                match key_event.code {
                    KeyCode::Backspace => {
                        self.command_input =
                            self.command_input[0..self.command_input.len() - 1].to_string();

                        if self.command_input.is_empty() {
                            self.is_inputting_command = false;
                        }

                        Ok(())
                    }

                    KeyCode::Enter => {
                        if !self.commands_history.is_empty() {
                            if self.commands_history.last().unwrap() != &self.command_input {
                                self.commands_history.push(self.command_input.clone());
                            }
                        } else {
                            self.commands_history.push(self.command_input.clone());
                        }

                        let command = &self.command_input.clone()[1..]; // first is always ":"

                        if let Err(e) = self.process_command(command) {
                            self.command_input = String::from(":");
                            self.command_error = Some(Box::new(e));
                        } else {
                            self.command_input = String::from("");

                            self.is_inputting_command = false;
                        }

                        self.command_history_index_offset = 0;
                        Ok(())
                    }

                    KeyCode::Up => {
                        if self.commands_history.is_empty() {
                            return Ok(());
                        }

                        let max_index = self.commands_history.len() - 1;
                        let checked_index = self.command_history_index_offset.checked_add(1);

                        if let Some(mut offset) = checked_index {
                            if offset > max_index + 1 {
                                offset = max_index + 1;
                            }

                            self.command_input = self.commands_history.get(max_index - (offset - 1)).unwrap().clone();

                            self.command_history_index_offset = offset;
                        }

                        Ok(())
                    }

                    KeyCode::Down => {
                        if self.commands_history.is_empty() {
                            return Ok(());
                        }

                        let max_index = self.commands_history.len() - 1;
                        let checked_index = self.command_history_index_offset.checked_sub(1);

                        if let Some(offset) = checked_index {
                            if offset == 0 {
                                self.command_input = ":".to_string();
                                return Ok(());
                            }

                            self.command_input = self.commands_history.get(max_index - (offset - 1)).unwrap().clone();

                            self.command_history_index_offset = offset;
                        }

                        Ok(())
                    }

                    KeyCode::Char(c) => {
                        self.command_input.push(c);
                        Ok(())
                    }

                    _ => Ok(()),
                }
            }
            event::Event::Paste(_) => todo!("command paste"),

            _ => Ok(()),
        }
    }

    fn process_command(&mut self, input: &str) -> Result<(), TracerError> {
        Command::execute(input, self).map_err(TracerError::CommandError)?;

        Ok(())
    }

    fn draw(&mut self) -> Result<(), TracerError> {
        if let Some(pointer) = self.terminal.clone() {
            let mut terminal = pointer.borrow_mut();
            let size = size().map_err(TracerError::IOError)?;
            match Tracer::get_layout_type(size) {
                Layouts::Compact => todo!(),
                Layouts::Full => {
                    self.draw_full(&mut terminal)?;
                }
            }

            return Ok(());
        }

        Err(TracerError::TerminalError(
            "Failed to retrieve pointer for terminal during draw",
        ))
    }

    fn draw_full(&self, terminal: &mut InitializedTerminal) -> Result<(), TracerError> {
        terminal
            .draw(|f| {
                use ratatui::layout::Constraint::{Fill, Length};

                let parent: Rect = if self.is_inputting_command {
                    let vertical_command = Layout::vertical([Fill(1), Length(3)]);

                    let [panels, command] = vertical_command.areas(f.area());

                    let mut bottom_title_text = String::from("");

                    if let Some(e) = &self.command_error {
                        bottom_title_text = format!("{e}");
                    }

                    let command_block = Block::bordered()
                        .padding(Padding::horizontal(1))
                        .title("Command")
                        .title_bottom(bottom_title_text);
                    let command_content = Text::from(Span::from(&self.command_input));
                    f.render_widget(
                        Paragraph::new(command_content).block(command_block),
                        command,
                    );

                    panels
                } else { f.area() };

                let horizontal = Layout::horizontal([Fill(2), Fill(5)]);
                let [left_area, right_area] = horizontal.areas(parent);

                let vertical = Layout::vertical([Length(5), Fill(1)]);
                let [states, disassembly] = vertical.areas(left_area);

                let vertical = Layout::vertical([Fill(1), Fill(1)]);
                let [registry, bottom_right] = vertical.areas(right_area);

                let states_block = Block::bordered()
                    .padding(Padding::horizontal(1))
                    .title("State");
                let states_content = Text::from(self.build_states_lines());
                f.render_widget(Paragraph::new(states_content).block(states_block), states);

                let disassembly_block = Block::bordered()
                    .padding(Padding::horizontal(1))
                    .title("Disassembly");
                let disassembly_content =
                    Text::from(self.build_disassembly_lines(
                        disassembly_block.inner(disassembly).height as usize,
                    ));
                f.render_widget(
                    Paragraph::new(disassembly_content).block(disassembly_block),
                    disassembly,
                );

                let registry_block = Block::bordered()
                    .padding(Padding::horizontal(1))
                    .title("Registry");
                let registry_content = Text::from(
                    self.build_registry_lines(registry_block.inner(registry).height as usize),
                );
                f.render_widget(
                    Paragraph::new(registry_content).block(registry_block),
                    registry,
                );

                let bottom_right_vertical = Layout::horizontal([Fill(1), Length(14)]);

                let [memory, stack] = bottom_right_vertical.areas(bottom_right);

                let memory_title_top = format!(
                    "SP: {0} ({0:#06X}) PC: {1} ({1:#06X})",
                    self.cpu.registers.stack_pointer, self.cpu.registers.program_counter
                );
                let memory_title_bottom = format!(
                    "Bin: {0:#010b} Dec: {0:03} Addr:{1:#06X}",
                    self.cpu.memory.data[self.memory_cursor_pos], self.memory_cursor_pos
                );
                let memory_block = Block::bordered()
                    .padding(Padding::horizontal(1))
                    .title("Memory")
                    .title_top(Line::from(memory_title_top).right_aligned())
                    .title_bottom(Line::from(memory_title_bottom).right_aligned());
                let memory_content =
                    Text::from(self.build_memory_lines(memory_block.inner(memory).height as usize));
                f.render_widget(Paragraph::new(memory_content).block(memory_block), memory);

                let stack_block = Block::bordered()
                    .padding(Padding::horizontal(1))
                    .title("Stack");
                let stack_content = Text::from(self.build_stack_lines(stack_block.inner(stack).height as usize));
                f.render_widget(Paragraph::new(stack_content).block(stack_block), stack);

            })
            .map_err(TracerError::IOError)
            .map(|_x| ())
    }

    fn get_layout_type(size: (u16, u16)) -> Layouts {
        Layouts::Full
    }

    fn build_states_lines(&'_ self) -> Vec<Line<'_>> {
        let mut res: Vec<Line> = Vec::new();

        let cpu_state_span: Span;

        if self.is_cpu_paused {
            cpu_state_span = Span::from("Paused").fg(Color::LightBlue)
        } else if self.is_cpu_running {
            cpu_state_span = Span::from("Running").fg(Color::Green)
        } else {
            cpu_state_span = Span::from("Waiting")
        }

        res.push(Line::from(vec![Span::from("CPU State: "), cpu_state_span]));

        res.push(Line::from(
            format!("PC: {0:#06X} ({0:05}) SP: {1:#04x} ({1:03})",
                self.cpu.registers.program_counter,
                self.cpu.registers.stack_pointer,
            )
        ));

        res.push(Line::from(
            format!("A: {0:#04X} ({0:03}) X: {1:#04X} ({1:03}) Y: {2:#04X} ({2:03})",
                self.cpu.registers.accumulator,
                self.cpu.registers.x,
                self.cpu.registers.y,
            )
        ));

        res
    }

    fn build_disassembly_lines(&'_ self, count: usize) -> Vec<Line<'_>> {
        let pos = match self.memory_follow_mode {
            MemoryCursorTracking::Cursor => self.memory_cursor_pos,
            MemoryCursorTracking::ProgramCounter => self.cpu.registers.program_counter as usize,
            MemoryCursorTracking::StackPointer => self.cpu.registers.stack_pointer as usize,
            MemoryCursorTracking::Static(x) => x as usize,
        };

        let mut lines: Vec<Line> = vec![];
        let mut page = 0;

        for chunk in self.parsed_instructions.chunks(count) {
            let (first_index, _first_instruction) = chunk.first().unwrap();
            let (last_index, last_instruction) = chunk.last().unwrap();

            if !(first_index..&(last_index + last_instruction.get_code_size())).contains(&&pos) {
                page += 1;
                continue;
            }

            lines = chunk
                .iter()
                .enumerate()
                .fold(Vec::new(), |mut acc, (index, (addr, inst))| {
                    let to;

                    if let Some(val) = self.parsed_instructions.get(page * count + index + 1) {
                        to = val.0;
                    } else {
                        to = usize::MAX;
                    }

                    let is_pos_at_cursor = (*addr..to).contains(&self.memory_cursor_pos);
                    let is_pos_at_pc =
                        (*addr..to).contains(&(self.cpu.registers.program_counter as usize));
                    let is_pos_at_stack =
                        (*addr..to).contains(&(0x100 + self.cpu.registers.stack_pointer as usize));

                    let optional_text = match inst.addressing_mode {
                        AddressingMode::Relative => {
                            let result_address= if inst.value as i8 > 0 {
                                addr.wrapping_add(2).wrapping_add(inst.value as usize)
                            } else {
                                addr.wrapping_add(2).wrapping_sub((inst.value as i8).unsigned_abs() as usize)
                            };

                            if &result_address == addr {
                                String::from("(loop)")
                            } else {
                                format!("({result_address:#06X})")
                            }
                        }

                        AddressingMode::Indirect => {
                            format!("({:#04X}{:02X})", self.cpu.memory.data[inst.value.wrapping_add(1) as usize], self.cpu.memory.data[inst.value as usize])
                        }

                        AddressingMode::IndexedIndirect => {
                            let lsb = self.cpu.memory.data[((inst.value as u8).wrapping_add(self.cpu.registers.x)) as usize];
                            let msb = self.cpu.memory.data[((inst.value as u8).wrapping_add(self.cpu.registers.x).wrapping_add(1)) as usize];

                            let address = ((msb as u16) << 8) + (lsb as u16);

                            format!("({address:#06X}) [{0:#04X} ({0})]",
                                self.cpu.memory.data[address as usize]
                            )
                        }

                        AddressingMode::IndirectIndexed => {
                            let lsb = self.cpu.memory.data[inst.value as u8 as usize];
                            let msb = self.cpu.memory.data[(inst.value as u8).wrapping_add(1) as usize];

                            let address = (((msb as u16) << 8) + (lsb as u16)).wrapping_add(self.cpu.registers.y as u16);

                            format!("({address:#06X}) [{0:#04X} ({0})]",
                                self.cpu.memory.data[address as usize]
                            )
                        }

                        AddressingMode::AbsoluteY => {
                            format!("({0:#06X}) [{1:#04X} ({1})]",
                                inst.value.wrapping_add(self.cpu.registers.y as u16),
                                self.cpu.memory.data[inst.value.wrapping_add(self.cpu.registers.y as u16) as usize]
                            )
                        }

                        AddressingMode::ZeroPageX => {
                            format!("({0:#04X}) [{1:#04X} ({1})]",
                                inst.value.wrapping_add(self.cpu.registers.x as u16),
                                self.cpu.memory.data[inst.value.wrapping_add(self.cpu.registers.x as u16) as usize]
                            )
                        }

                        AddressingMode::AbsoluteX => {
                            format!("({0:#06X}) [{1:#04X} ({1})]",
                                inst.value.wrapping_add(self.cpu.registers.x as u16),
                                self.cpu.memory.data[inst.value.wrapping_add(self.cpu.registers.x as u16) as usize]
                            )
                        }

                        _ => String::from("")
                    };

                    let span: Span = Span::styled(
                        format!("{addr:#06X}    {inst} {optional_text}"),
                        self.get_cursors_style(is_pos_at_cursor, is_pos_at_pc, is_pos_at_stack),
                    );

                    acc.push(Line::from(span));

                    acc
                });

            break;
        }

        lines
    }

    fn build_registry_lines(&'_ self, count: usize) -> Vec<Line<'_>> {
        if let Some(stripped) = self.registry_history.get(
                self.registry_history.len() - count.min(self.registry_history.len())..self.registry_history.len()
            ) {
                return stripped.iter().fold(Vec::new(), |mut acc, reg| {
                    let string = format!("{:#06X} A: {:03} X: {:03} Y: {:03} N: {} V: {} B: {} D: {} I: {} Z: {} C: {} ({:#010b} - {:03}) With B: ({:#010b} - {:03})",
                        reg.addr,
                        reg.a,
                        reg.x,
                        reg.y,
                        reg.status.contains(ProcessorStatus::NegativeFlag) as u8,
                        reg.status.contains(ProcessorStatus::OverflowFlag) as u8,
                        reg.status.contains(ProcessorStatus::BreakCommand) as u8,
                        reg.status.contains(ProcessorStatus::DecimalMode) as u8,
                        reg.status.contains(ProcessorStatus::InterruptDisable) as u8,
                        reg.status.contains(ProcessorStatus::ZeroFlag) as u8,
                        reg.status.contains(ProcessorStatus::CarryFlag) as u8,
                        reg.status.bits() | 1 << 5,
                        reg.status.bits() | 1 << 5,
                        reg.status.bits() | 1 << 5 | ProcessorStatus::BreakCommand.bits(),
                        reg.status.bits() | 1 << 5 | ProcessorStatus::BreakCommand.bits(),
                    );

                    acc.push(Line::from(Span::from(string)));

                    acc
                });
        }

        vec![]
    }

    fn build_memory_lines(&'_ self, count: usize) -> Vec<Line<'_>> {
        let from_addr: usize = match self.memory_follow_mode {
            MemoryCursorTracking::Cursor => 16 * (self.memory_cursor_pos / 16),

            MemoryCursorTracking::ProgramCounter => {
                16 * (self.cpu.registers.program_counter / 16) as usize
            }

            MemoryCursorTracking::StackPointer => {
                16 * (self.cpu.registers.stack_pointer / 16) as usize
            }

            MemoryCursorTracking::Static(x) => 16 * (x / 16) as usize,
        };

        self.cpu.memory.data[from_addr..]
            .chunks_exact(16)
            .enumerate()
            .fold(Vec::new(), |mut acc, (chunk_index, values)| {
                if chunk_index >= count {
                    return acc;
                }

                let mut line = Line::from(format!("{0:#06x}    ", (chunk_index * 16) + from_addr));

                values.iter().enumerate().for_each(|(index, val)| {
                    let address = chunk_index * 16 + index + from_addr;

                    let is_pos_at_cursor = address == self.memory_cursor_pos;
                    let is_pos_at_pc = address == self.cpu.registers.program_counter as usize;
                    let is_pos_at_stack =
                        address == 0x100 + self.cpu.registers.stack_pointer as usize;

                    let span: Span = Span::styled(
                        format!("{val:#04X}"),
                        self.get_cursors_style(is_pos_at_cursor, is_pos_at_pc, is_pos_at_stack),
                    );

                    line.push_span(span);

                    line.push_span(Span::from(" ")); // needed to make highlight show only addres, not the space after it.
                });

                acc.push(line);

                acc
            })
    }

    fn build_stack_lines(&'_ self, count: usize) -> Vec<Line<'_>> {
        let range;
        let addr_offset;

        if (self.cpu.registers.stack_pointer as usize) < 0x100 - count {
            // TODO i think i'll need to make it scroll in case it will use more than "count"
            range = 0x100..=0x1FF;
            addr_offset = 0x0;
        } else {
            range = 0x100..=0x1FF;
            addr_offset = 0x0;
        }


        self.cpu.memory.data[range]
            .iter()
            .rev()
            .enumerate()
            .fold(Vec::new(), |mut acc, (index, value)| {
                let address = 0x200 - index - 1 + addr_offset;

                let is_pos_at_cursor = address == self.memory_cursor_pos;
                let is_pos_at_pc = address == self.cpu.registers.program_counter as usize;
                let is_pos_at_stack = address == 0x100 + self.cpu.registers.stack_pointer as usize;

                let mut line = Line::default().style(self.get_cursors_style(is_pos_at_cursor, is_pos_at_pc, is_pos_at_stack));

                let addr_span = Span::from(format!("{address:#05X} "));
                line.push_span(addr_span);
                
                let value_span = Span::from(format!("{value:#04X}"));
                line.push_span(value_span);

                acc.push(line);

                acc
            })
    }

    fn get_cursors_style(&self, on_cursor: bool, on_pc: bool, on_stack: bool) -> Style {
        if on_stack && on_cursor && on_pc {
            Style::default().bg(Color::White).fg(Color::Magenta)
        } else if on_cursor && on_pc {
            Style::default().bg(Color::White).fg(Color::Blue)
        } else if on_stack && on_cursor {
            Style::default().bg(Color::White).fg(Color::Red)
        } else if on_stack && on_pc {
            Style::default().fg(Color::Magenta)
        } else if on_cursor {
            Style::default().bg(Color::White).fg(Color::Black)
        } else if on_pc {
            Style::default().fg(Color::Blue)
        } else if on_stack {
            Style::default().fg(Color::Red)
        } else {
            Style::default()
        }
    }
}
