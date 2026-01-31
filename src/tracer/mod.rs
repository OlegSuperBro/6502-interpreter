use std::{cell::RefCell, error::Error, io::{self, Stdout}, rc::Rc};

use crossterm::{event::{self, KeyCode, KeyEvent}, execute, terminal::{self, EnterAlternateScreen, LeaveAlternateScreen, size}};
use ratatui::{Terminal, layout::Layout, prelude::CrosstermBackend, style::{Color, Style, Stylize}, text::{Line, Span, Text}, widgets::{self, Block, Padding, Paragraph, Widget}};

use crate::{CPU, ProcessorStatus, tracer::errors::{CommandError, HotkeyError}};

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
            "q" | "quit" => Ok(Command::Quit)
        }
    } => Err(CommandError::InvalidCommand)
}

add_parse_executor!{
    Command (args &[&str], tracer &mut Tracer) Result<(), CommandError> {
        Command::SetMem => {
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

    InputCommand,
    Quit
}

add_find_by!{
    Hotkeys Result<Hotkeys, HotkeyError> {
        code<&KeyCode> => {
            KeyCode::Char(':') => Ok(Hotkeys::InputCommand),
            KeyCode::Char('q') => Ok(Hotkeys::Quit)
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
        }
    } => todo!("hotkey")
}


enum Areas {
    Dissasembly,
    Memory,
    Registry,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum States {
    Waiting,
    CommandInput,
    Exit,
}

struct ProcessorStatusHistoryEntry(u16, ProcessorStatus);

pub struct Tracer {
    terminal: Option<Rc<RefCell<InitializedTerminal>>>,

    pub cpu: Rc<CPU>,

    pub commands_history: Vec<String>,
    pub command_input: String,

    pub active_area: Areas,

    pub current_state: States,

    pub registry_history: Vec<ProcessorStatusHistoryEntry>,
}

enum Layouts {
    Compact,
    Full
}

impl Tracer {
    pub fn new(cpu: Rc<CPU>) -> Self {
        Tracer {
            cpu,
            commands_history: vec![],
            registry_history: Vec::from([ProcessorStatusHistoryEntry(0x0000, ProcessorStatus::all()), ProcessorStatusHistoryEntry(0x0400, ProcessorStatus::ZeroFlag)]),
            active_area: Areas::Dissasembly,
            current_state: States::Waiting,
            command_input: String::new(),

            terminal: None,
        }
    }

    pub fn init(&mut self) -> Result<(), TracerError> {
        self.terminal = Some(Rc::new(RefCell::new(ratatui::init())));
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
        self.process_inputs()?;
        self.draw()?;
        Ok(true)
    }

    pub fn force_redraw(&mut self) -> Result<(), TracerError> {
        self.draw()
    }

    fn process_inputs(&mut self) -> Result<(), TracerError> {
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
            event::Event::Paste(_) => todo!(),

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
                    use ratatui::layout::Constraint::{Fill, Length, Min};

                    match self.current_state {
                        States::CommandInput => {
                            let vertical_command = Layout::vertical([Fill(1), Length(3)]);

                            let [panels, command] = vertical_command.areas(f.area());

                            let horizontal = Layout::horizontal([Fill(1); 2]);
                            let [disassembly, right_area] = horizontal.areas(panels);
                            let vertical = Layout::vertical([Fill(1), Fill(1)]);
                            let [registry, memory] = vertical.areas(right_area);
                            
                            let disassembly_block = Block::bordered().padding(Padding::horizontal(1)).title("Disassembly");
                            let disassembly_content = Text::from(self.build_disassembly_lines(disassembly_block.inner(panels).height as usize));
                            f.render_widget(Paragraph::new(disassembly_content).block(disassembly_block), disassembly);
                            
                            let registry_block = Block::bordered().padding(Padding::horizontal(1)).title("Registry");
                            let registry_content = Text::from(self.build_registry_lines(registry_block.inner(panels).height as usize));
                            f.render_widget(Paragraph::new(registry_content).block(registry_block), registry);
        
                            let memory_block = Block::bordered().padding(Padding::horizontal(1)).title("Memory");
                            let memory_content = Text::from(self.build_memory_lines(memory_block.inner(panels).height as usize));
                            f.render_widget(Paragraph::new(memory_content).block(memory_block), memory);

                            let command_block = Block::bordered().padding(Padding::horizontal(1)).title("Command");
                            let command_content = Text::from(Span::from(&self.command_input));
                            f.render_widget(Paragraph::new(command_content).block(command_block), command);
                        }
                        _ => {
                            let horizontal = Layout::horizontal([Fill(1); 2]);
                            let [disassembly, right_area] = horizontal.areas(f.area());
                            let vertical = Layout::vertical([Fill(1), Fill(1)]);
                            let [registry, memory] = vertical.areas(right_area);
        
                            f.render_widget(Block::bordered().title("Disassembly"), disassembly);
                            f.render_widget(Block::bordered().title("Registry"), registry);
                            f.render_widget(Block::bordered().title("Memory"), memory);
                        }
                    }
                }).map_err(TracerError::IOError)?;

        Ok(())
    }

    fn get_layout_type(size: (u16, u16)) -> Layouts {
        return Layouts::Full;
    }

    fn build_disassembly_lines(&self, count: usize) -> Vec<Line> {
        vec![
            Line::from(Span::from("WIP")
                .style(
                    Style::default()
                        .fg(Color::Green)
                )
            ); count
        ]
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
        self.cpu.memory.data.chunks_exact(16).enumerate().fold(Vec::new(), |mut acc, (chunk_index, values)| {
            if chunk_index >= count {
                return acc;
            }

            let mut line = format!("{0:#06x}    ", chunk_index * 16);

            values.iter().for_each(|val| {
                line += format!("{:#04X} ", val).as_str();
            });
        
            acc.push(Line::from(Span::from(line)));

            acc
        })
    }
}
