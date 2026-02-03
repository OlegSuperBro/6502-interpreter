use std::{
    cell::RefCell, convert::identity, fmt::format, io::Stdout, rc::{Rc, Weak}, time::Duration, vec
};

use crossterm::{event::{self, KeyCode, KeyEvent}, execute, terminal::{self, EnterAlternateScreen, LeaveAlternateScreen, size}};
use ratatui::{Frame, Terminal, layout::{Layout, Rect}, prelude::CrosstermBackend, style::{Color, Style, Stylize}, text::{Line, Span, Text}, widgets::{self, Block, Padding, Paragraph, Widget}};

use crate::{CPU, ProcessorStatus, instructions::{self, Instruction}, parser::parse_instruction, tracer::errors::{CommandError, HotkeyError}};

use macros::{
    add_find_by,
    add_info,
    add_parse_executor,
};

pub use errors::{
    TracerError
};

pub mod macros;
mod errors;

type InitializedTerminal = Terminal<CrosstermBackend<Stdout>>;


enum Command {
    SetMem,
    DumpMem,
    Run,
    RunUntil,
    RunSilent,
    RunSilentUntil,
    SetRunSpeed,

    FollowCursor,
    Quit,
}

add_info!{
    Command Result<&'static str, CommandError> {
        name => {
            Command::SetMem => Ok("set"),
            Command::DumpMem => Ok("dump")
        }
        description => {
            Command::SetMem => Ok("Desc")
        }
    } => Err(CommandError::InvalidCommand)
}

add_find_by!{
    Command Result<Command, CommandError>{
        name<&str> => {
            "setmem" => Ok(Command::SetMem),
            "dumpmem" => Ok(Command::DumpMem),
            "run" => Ok(Command::Run),
            "run_until" => Ok(Command::RunUntil),
            "run_silent" => Ok(Command::RunSilent),
            "run_silent_until" => Ok(Command::RunSilentUntil),
            "set_run_speed" => Ok(Command::SetRunSpeed),
            "follow" => Ok(Command::FollowCursor),

            "q" | "quit" => Ok(Command::Quit)
        }
    } => Err(CommandError::InvalidCommand)
}

add_parse_executor!{
    Command (args &[&str], tracer &mut Tracer) Result<(), CommandError> {
        Command::SetMem => {
            Ok(())
        },

        Command::Run => {
            if args.len() > 0 {
                let target_address: usize;

                let target_address_arg = args.first().unwrap();

                if target_address_arg.starts_with("0x") {
                    // TODO replace .unwrap() with check
                    target_address = tracer.cpu.registers.program_counter as usize + usize::from_str_radix(&target_address_arg[2..], 16).unwrap();
                } else {
                    // TODO replace .unwrap() with check
                    target_address = tracer.cpu.registers.program_counter as usize + target_address_arg.parse::<usize>().unwrap();
                }

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;

                    let pc = tracer.cpu.registers.program_counter as usize;

                    if pc >= identity(target_address) {
                        return false;
                    }
    
                    return true;
                }));
            }


            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::RunUntil => {
            if args.len() > 0 {
                let target_address: usize;

                let target_address_arg = args.first().unwrap();

                if target_address_arg.starts_with("0x") {
                    // TODO replace .unwrap() with check
                    target_address = usize::from_str_radix(&target_address_arg[2..], 16).unwrap();
                } else {
                    // TODO replace .unwrap() with check
                    target_address = target_address_arg.parse::<usize>().unwrap();
                }

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;

                    let pc = tracer.cpu.registers.program_counter as usize;

                    if pc >= identity(target_address) {
                        return false;
                    }
    
                    return true;
                }));
            }


            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::RunSilent => {
            if args.len() > 0 {
                let target_address: usize;

                let target_address_arg = args.first().unwrap();

                if target_address_arg.starts_with("0x") {
                    // TODO replace .unwrap() with check
                    target_address = tracer.cpu.registers.program_counter as usize + usize::from_str_radix(&target_address_arg[2..], 16).unwrap();
                } else {
                    // TODO replace .unwrap() with check
                    target_address = tracer.cpu.registers.program_counter as usize + target_address_arg.parse::<usize>().unwrap();
                }

                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;

                    let pc = tracer.cpu.registers.program_counter as usize;

                    if pc >= identity(target_address) {
                        return false;
                    }
    
                    return true;
                }));

                tracer.current_state = States::RunningSilent;
            }
            

            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::RunSilentUntil => {
            if args.len() > 0 {
                let target_address: usize;
                
                let target_address_arg = args.first().unwrap();
                
                if target_address_arg.starts_with("0x") {
                    // TODO replace .unwrap() with check
                    target_address = usize::from_str_radix(&target_address_arg[2..], 16).unwrap();
                } else {
                    // TODO replace .unwrap() with check
                    target_address = target_address_arg.parse::<usize>().unwrap();
                }
                
                tracer.running_predicate = Some(Box::new(move |tracer| {
                    let target_address = target_address;
                    
                    let pc = tracer.cpu.registers.program_counter as usize;
                    
                    if pc >= identity(target_address) {
                        return false;
                    }
    
                    return true;
                }));
                tracer.current_state = States::RunningSilent;
            }


            tracer.is_cpu_running = true;
            Ok(())
        },

        Command::FollowCursor => {
            if args.len() > 0 {
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

        Command::Quit => {
            tracer.current_state = States::Exit;
            Ok(())
        }
    } => Err(CommandError::InvalidCommand)
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
    Quit
}

add_find_by!{
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

add_parse_executor!{
    Hotkeys (tracer &mut Tracer) Result<(), HotkeyError> {
        Hotkeys::InputCommand => {
            tracer.current_state = States::CommandInput;
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
    CommandInput,
    RunningSilent,
    Exit,
}

pub struct ProcessorStatusHistoryEntry(u16, ProcessorStatus);

pub struct Tracer {
    terminal: Option<Rc<RefCell<InitializedTerminal>>>,

    parsed_instructions: Vec<(usize, Instruction)>,

    pub cpu: Rc<CPU>,
    pub is_cpu_running: bool,
    pub is_cpu_paused: bool,
    pub running_predicate: Option<Box<dyn Fn(&Tracer) -> bool>>,

    pub commands_history: Vec<String>,
    pub command_input: String,

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
    Full
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
            registry_history: Vec::new(),
            active_area: Areas::Memory,
            current_state: States::Waiting,
            command_input: String::new(),

            memory_cursor_pos: 0x00,
            memory_follow_mode: MemoryCursorTracking::Static(0),
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

            if let Ok((offset, instruction)) = parse_instruction(index as u16, &self.cpu.memory.data){
                self.parsed_instructions.push((index, instruction));
                index += offset;
            } else {
                self.parsed_instructions.push(
                    (
                        index,
                        Instruction {
                            opcode: crate::instructions::OpCode::Unknown,
                            addressing_mode: instructions::AddressingMode::Unknown,
                            value: 0
                        }
                    )
                );
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
        if self.current_state != States::RunningSilent {
            self.process_inputs()?;
            self.draw()?;
        }

        if self.is_cpu_running && !self.is_cpu_paused {
            if let Some(cpu) = Rc::get_mut(&mut self.cpu) {

                if let Err(e) = cpu.once() {
                    todo!("Error occured during cpu run {e}")
                } else {
                    if (self.registry_history.len() == 0) || (
                        self.registry_history.last().unwrap().1.bits() != self.cpu.registers.processor_status.bits()
                    ) {
                        self.registry_history.push(ProcessorStatusHistoryEntry(self.cpu.registers.program_counter, self.cpu.registers.processor_status.clone()));
                    }
                }
            }

            if let Some(predicate) = &self.running_predicate {
                if !predicate(self) {
                    self.is_cpu_running = false;

                    if self.current_state == States::RunningSilent {
                        self.current_state = States::Waiting;
                    }
                }
            }
        }
        Ok(true)
    }

    pub fn force_redraw(&mut self) -> Result<(), TracerError> {
        self.draw()
    }

    fn process_inputs(&mut self) -> Result<(), TracerError> {
        if let Ok(res) = crossterm::event::poll(Duration::from_nanos(1)) {
            if !res {
                return Ok(());
            }
        }

        match self.current_state {
            States::Waiting => {
                self.process_hotkey()
            }

            States::CommandInput => {
                self.process_command_input()
            }

            _ => Ok(())
        }
    }

    fn process_hotkey(&mut self) -> Result<(), TracerError> {
        match crossterm::event::read().map_err(TracerError::IOError)? {
            event::Event::Key(key_event) => {
                let result = self.process_key(key_event);

                if let Err(e) = result {
                    if let TracerError::HotkeyError(HotkeyError::InvalidHotkey) = e {
                        return Ok(());
                    } else {
                        return Err(e);
                    }
                }
            },

            _ => {}
        };

        Ok(())
    }

    fn process_key(&mut self, key_event: KeyEvent) -> Result<(), TracerError> {
        match self.current_state {
            States::Waiting => {
                let hotkey = Hotkeys::find_by_code(&key_event.code).map_err(|e| TracerError::HotkeyError(e))?;

                Hotkeys::parse_and_run(&hotkey, self).map_err(|e| TracerError::HotkeyError(e))?;
            }

            States::CommandInput => {
                if self.command_input.is_empty() {
                    self.current_state = States::Waiting;
                    return Ok(());
                }
            },
            _ => todo!()
        }

        Ok(())
    }

    fn process_command_input(&mut self) ->  Result<(), TracerError> {
        match crossterm::event::read().map_err(TracerError::IOError)? {
            event::Event::Key(key_event) => {
                match key_event.code {
                    KeyCode::Backspace => {
                        self.command_input = self.command_input[0..self.command_input.len() - 1].to_string();

                        if self.command_input.is_empty() {
                            self.current_state = States::Waiting;
                        }

                        Ok(())
                    }

                    KeyCode::Enter => {
                        let command = &self.command_input.clone()[1..]; // first is always ":"

                        self.command_input = String::from("");
                        self.current_state = States::Waiting;

                        self.process_command(command)?;
                        Ok(())
                    }

                    KeyCode::Char(c) => {
                        self.command_input.push(c);
                        Ok(())
                    }

                    _ => Ok(())
                }
            },
            event::Event::Paste(_) => todo!("command paste"),

            _ => Ok(())
        }
    }

    fn process_command(&mut self, input: &str) -> Result<(), TracerError> {
        Command::execute(input, self).map_err(TracerError::CommandError)?;
        
        Ok(())
    }

    fn draw(&mut self) -> Result<(), TracerError> {
        if let Some(pointer) = self.terminal.clone() {
            let mut terminal = pointer.borrow_mut();
            let size = size().map_err(|e| TracerError::IOError(e))?;
            match Tracer::get_layout_type(size) {
                Layouts::Compact => todo!(),
                Layouts::Full => {
                    self.draw_full(&mut terminal)?;
                }
            }

            return Ok(());
        }

        Err(TracerError::TerminalError("Failed to retrieve pointer for terminal during draw"))
    }

    fn draw_full(&self, terminal: &mut InitializedTerminal) -> Result<(), TracerError> {
        terminal.draw(|f| {
            use ratatui::layout::Constraint::{Fill, Length};

            let parent: Rect;
            match self.current_state {
                States::CommandInput => {
                    let vertical_command = Layout::vertical([Fill(1), Length(3)]);

                    let [panels, command] = vertical_command.areas(f.area());

                    let command_block = Block::bordered().padding(Padding::horizontal(1)).title("Command");
                    let command_content = Text::from(Span::from(&self.command_input));
                    f.render_widget(Paragraph::new(command_content).block(command_block), command);

                    parent = panels;
                }
                _ => {
                    parent = f.area();
                }
            }

            let horizontal = Layout::horizontal([Fill(1), Fill(3)]);
            let [left_area, right_area] = horizontal.areas(parent);

            let vertical = Layout::vertical([Length(5), Fill(1)]);
            let [states, disassembly] = vertical.areas(left_area);

            let vertical = Layout::vertical([Fill(1), Fill(1)]);
            let [registry, memory] = vertical.areas(right_area);

            let states_block = Block::bordered().padding(Padding::horizontal(1)).title("State");
            let states_content = Text::from(self.build_states_lines());
            f.render_widget(Paragraph::new(states_content).block(states_block), states);

            let disassembly_block = Block::bordered().padding(Padding::horizontal(1)).title("Disassembly");
            let disassembly_content = Text::from(self.build_disassembly_lines(disassembly_block.inner(parent).height as usize));
            f.render_widget(Paragraph::new(disassembly_content).block(disassembly_block), disassembly);

            let registry_block = Block::bordered()
                                                .padding(Padding::horizontal(1))
                                                .title("Registry");
            let registry_content = Text::from(self.build_registry_lines(registry_block.inner(parent).height as usize));
            f.render_widget(Paragraph::new(registry_content).block(registry_block), registry);

            let memory_title_top = format!("SP: {0} ({0:#06X}) PC: {1} ({1:#06X})", self.cpu.registers.stack_pointer, self.cpu.registers.program_counter);
            let memory_title_bottom = format!("{:#0X}", self.memory_cursor_pos);
            let memory_block = Block::bordered()
                                            .padding(Padding::horizontal(1))
                                            .title("Memory")
                                            .title_top(Line::from(memory_title_top).right_aligned())
                                            .title_bottom(Line::from(memory_title_bottom).right_aligned());
            let memory_content = Text::from(self.build_memory_lines(memory_block.inner(parent).height as usize));
            f.render_widget(Paragraph::new(memory_content).block(memory_block), memory);
        }).map_err(TracerError::IOError).map(|_x| ())
    }

    fn get_layout_type(size: (u16, u16)) -> Layouts {
        return Layouts::Full;
    }

    fn build_states_lines(&self) -> Vec<Line> {
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


        res
    }

    fn build_disassembly_lines(&self, count: usize) -> Vec<Line> {
        self.parsed_instructions.iter().enumerate().fold(Vec::new(), |mut acc, (index, (addr, inst))| {
            let to;

            if let Some(val) = self.parsed_instructions.get(index + 1) {
                to = val.0;
            } else {
                to = usize::MAX;
            }

            let is_pos_at_cursor = (*addr..to).contains(&self.memory_cursor_pos);
            let is_pos_at_pc = (self.cpu.registers.program_counter as usize..to).contains(addr);
            let is_pos_at_stack = (0x100 + self.cpu.registers.stack_pointer as usize..to).contains(addr);

            let span: Span = Span::styled(
                            format!("{0:#06X}    {1}", addr, inst),
                            self.get_cursors_style(is_pos_at_cursor, is_pos_at_pc, is_pos_at_stack)
                        );

            acc.push(Line::from(span));

            acc
        })
        // self.cpu.memory.data[from_addr..].enumerate().fold(Vec::new(), |mut acc, (chunk_index, values)| {
        //     if chunk_index >= count {
        //         return acc;
        //     }

        //     let mut line = Line::from(format!("{0:#06x}    ", (chunk_index * 16) + from_addr));

        //     values.iter().enumerate().for_each(|(index, val)| {
        //         let address = chunk_index * 16 + index + from_addr;

        //         let is_pos_at_cursor = address == self.memory_cursor_pos;
        //         let is_pos_at_pc = address == self.cpu.registers.program_counter as usize;
        //         let is_pos_at_stack = address == 0x100 + self.cpu.registers.stack_pointer as usize;

        //         let span: Span = Span::styled(
        //                     format!("{:#04X}", val),
        //                     self.get_cursors_style(is_pos_at_cursor, is_pos_at_pc, is_pos_at_stack)
        //                 );

        //         line.push_span(span);

        //         line.push_span(Span::from(" ")); // needed to make highlight show only addres, not the space after it.
        //     });

        //     acc.push(line);

        //     acc
        // })
    }

    fn build_registry_lines(&self, count: usize) -> Vec<Line> {
        self.registry_history.iter().fold(Vec::new(), |mut acc, reg| {
            let string = format!("{:#06X} N: {} V: {} B: {} D: {} I: {} Z: {} C: {} ({:#010b} - {:03}) With B: ({:#010b} - {:03})",
                reg.0,
                reg.1.contains(ProcessorStatus::NegativeFlag) as u8,
                reg.1.contains(ProcessorStatus::OverflowFlag) as u8,
                reg.1.contains(ProcessorStatus::BreakCommand) as u8,
                reg.1.contains(ProcessorStatus::DecimalMode) as u8,
                reg.1.contains(ProcessorStatus::InterruptDisable) as u8,
                reg.1.contains(ProcessorStatus::ZeroFlag) as u8,
                reg.1.contains(ProcessorStatus::CarryFlag) as u8,
                reg.1.bits() | 1 << 5,
                reg.1.bits() | 1 << 5,
                reg.1.bits() | 1 << 5 | ProcessorStatus::BreakCommand.bits(),
                reg.1.bits() | 1 << 5 | ProcessorStatus::BreakCommand.bits(),
            );

            acc.push(Line::from(Span::from(string)));

            acc
        })
    }

    fn build_memory_lines(&self, count: usize) -> Vec<Line> {
        let from_addr: usize;
        match self.memory_follow_mode {
            MemoryCursorTracking::Cursor => {
                from_addr = 16 * (self.memory_cursor_pos / 16)
            }

            MemoryCursorTracking::ProgramCounter => {
                from_addr= 16 * (self.cpu.registers.program_counter / 16) as usize
            }

            MemoryCursorTracking::StackPointer => {
                from_addr= 16 * (self.cpu.registers.stack_pointer / 16) as usize
            }

            MemoryCursorTracking::Static(x) => {
                from_addr= 16 * (x / 16) as usize
            }
        }

        self.cpu.memory.data[from_addr..].chunks_exact(16).enumerate().fold(Vec::new(), |mut acc, (chunk_index, values)| {
            if chunk_index >= count {
                return acc;
            }

            let mut line = Line::from(format!("{0:#06x}    ", (chunk_index * 16) + from_addr));

            values.iter().enumerate().for_each(|(index, val)| {
                let address = chunk_index * 16 + index + from_addr;

                let is_pos_at_cursor = address == self.memory_cursor_pos;
                let is_pos_at_pc = address == self.cpu.registers.program_counter as usize;
                let is_pos_at_stack = address == 0x100 + self.cpu.registers.stack_pointer as usize;

                let span: Span = Span::styled(
                            format!("{:#04X}", val),
                            self.get_cursors_style(is_pos_at_cursor, is_pos_at_pc, is_pos_at_stack)
                        );

                line.push_span(span);

                line.push_span(Span::from(" ")); // needed to make highlight show only addres, not the space after it.
            });

            acc.push(line);

            acc
        })
    }

    fn get_cursors_style(&self, on_cursor: bool, on_pc: bool, on_stack: bool) -> Style {
        if on_stack && on_cursor && on_pc {
            return Style::default().bg(Color::White).fg(Color::Magenta);
        } else if on_cursor && on_pc {
            return Style::default().bg(Color::White).fg(Color::Blue);
        } else if on_stack && on_cursor {
            return Style::default().bg(Color::White).fg(Color::Red);
        } else if on_stack && on_pc {
            return Style::default().fg(Color::Magenta)
        } else if on_cursor {
            return Style::default().bg(Color::White).fg(Color::Black);
        } else if on_pc {
            return Style::default().fg(Color::Blue);
        } else {
            return Style::default();
        }
    }
}
