use std::{cell::RefCell, error::Error, io::{self, Stdout}, rc::Rc};

use crossterm::{event::{self, KeyCode, KeyEvent}, execute, terminal::{self, EnterAlternateScreen, LeaveAlternateScreen, size}};
use ratatui::{Terminal, layout::Layout, prelude::CrosstermBackend, widgets::{self, Block}};

use crate::{CPU, tracer::errors::{CommandError, HotkeyError}};

use macros::{
    add_find_by,
    add_info,
    add_parse_executor,
};

use errors::{
    TracerError
};

mod macros;
mod errors;

type InitializedTerminal = Terminal<CrosstermBackend<Stdout>>;


enum Command {
    SetMem,
    DumpMem,
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
    } => Err(CommandError::InvalidHotkey)
}

add_find_by!{
    Command Result<Command, CommandError>{
        name<&str> => {
            "setmem" => Ok(Command::SetMem),
            "dumpmem" => Ok(Command::DumpMem)
        }
    } => Err(CommandError::InvalidHotkey)
}

add_parse_executor!{
    Command (args &[&str], tracer &mut Tracer) Result<(), CommandError> {
        Command::SetMem => {
            Ok(())
        }
    } => Err(CommandError::InvalidHotkey)
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

pub struct Tracer {
    terminal: Option<Rc<RefCell<InitializedTerminal>>>,

    pub cpu: Rc<CPU>,

    pub commands_history: Vec<String>,
    pub command_input: String,

    pub selected_area: Areas,
    pub current_state: States,
}

enum Layouts {
    Compact,
    Full
}

impl Tracer {
    pub fn new(cpu: Rc<CPU>) -> Self {
        return Tracer {
            cpu,
            commands_history: vec![],
            selected_area: Areas::Dissasembly,
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
        match crossterm::event::read().map_err(|e| TracerError::IOError(e))? {
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
            event::Event::Paste(_) => todo!(),

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
        
                            f.render_widget(Block::bordered().title("Disassembly"), disassembly);
                            f.render_widget(Block::bordered().title("Registry"), registry);
                            f.render_widget(Block::bordered().title("Memory"), memory);
                            f.render_widget(Block::bordered().title("Command"), command);
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
                }).map_err(|e| TracerError::IOError(e))?;

        Ok(())
    }

    fn get_layout_type(size: (u16, u16)) -> Layouts {
        return Layouts::Full;
    }
}
