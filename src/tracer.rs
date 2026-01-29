use std::{error::Error, rc::Rc};

use crate::CPU;

struct Command
{
    name: &'static str,
    description: &'static str,

    action: Box<dyn Fn(CPU) -> Result<(), Box<dyn Error>>>
}

impl Command {
    pub fn new(name: &'static str, description: &'static str, action: impl Fn(CPU) -> Result<(), Box<dyn Error>> + 'static) -> Self {
        Command {
            name,
            description,
            action: Box::new(action)
        }
    }

    pub fn run(&self, cpu: CPU) -> Result<(), Box<dyn Error>> {
        (&self.action)(cpu)
    }
}

struct Shortcut {
    name: &'static str,
    key: usize,
    action: Box<dyn Fn(CPU) -> Result<(), Box<dyn Error>>>,
}

impl Shortcut {
    pub fn new(name: &'static str, key: usize, action: impl Fn(CPU) -> Result<(), Box<dyn Error>> + 'static) -> Self {
        Shortcut {
            name,
            key,
            action: Box::new(action)
        }
    }
}

pub struct Tracer {
    cpu: Rc<CPU>,

    commands: Vec<Command>,
    commands_history: Vec<String>
}

impl Tracer {
    pub fn new(cpu: Rc<CPU>) -> Self {
        return Tracer { cpu, commands: vec![], commands_history: vec![]}
    }

    pub fn create_command(&mut self, name: &'static str, description: &'static str, action: impl Fn(CPU) -> Result<(), Box<dyn Error>> + 'static) {
        self.add_command(Command::new(name, description, action));
    }

    pub fn add_command(&mut self, command: Command) {
        todo!()
    }
}
