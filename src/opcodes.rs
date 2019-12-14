use super::intcode;
use super::intcode::{Command, InstructionSet, ParamMode};
use super::util::wrapper::Instruction;
use std::{cell::RefCell, io};

#[derive(Debug)]
pub enum DefaultOpcodes {
    Add(ParamMode, ParamMode),
    Mul(ParamMode, ParamMode),
    Out(ParamMode),
    Jne(ParamMode, ParamMode),
    Je(ParamMode, ParamMode),
    Leq(ParamMode, ParamMode),
    Cmp(ParamMode, ParamMode),
    In,
    End,
    Data,
}

impl Default for DefaultOpcodes {
    fn default() -> Self {
        DefaultOpcodes::Data
    }
}

impl InstructionSet for DefaultOpcodes {
    fn get_stride(&self) -> usize {
        use DefaultOpcodes::*;
        match self {
            Add(_, _) | Mul(_, _) | Leq(_, _) | Cmp(_, _) => 3,
            Jne(_, _) | Je(_, _) => 2,
            Out(_) | In => 1,
            End | Data => 0,
        }
    }

    fn process_opcode(opcode: &[u8; 5], value: isize) -> Command<DefaultOpcodes> {
        use intcode::{parse_binary, parse_unary};
        type Cmd = Command<DefaultOpcodes>;
        // if the value is less than zero then it for sure isn't an instruction.
        if value < 0 {
            return Cmd::new(Self::default(), value);
        }

        use DefaultOpcodes::*;
        match opcode {
            [0, b, c, 0, 1] => parse_binary(*b, *c, value, |b, c, v| Cmd::new(Add(b, c), v)),
            [0, b, c, 0, 2] => parse_binary(*b, *c, value, |b, c, v| Cmd::new(Mul(b, c), v)),
            [0, 0, 0, 0, 3] => Cmd::new(In, value),
            [0, 0, c, 0, 4] => parse_unary(*c, value, |p, v| Cmd::new(Out(p), v)),
            [0, b, c, 0, 5] => parse_binary(*b, *c, value, |b, c, v| Cmd::new(Jne(b, c), v)),
            [0, b, c, 0, 6] => parse_binary(*b, *c, value, |b, c, v| Cmd::new(Je(b, c), v)),
            [0, b, c, 0, 7] => parse_binary(*b, *c, value, |b, c, v| Cmd::new(Leq(b, c), v)),
            [0, b, c, 0, 8] => parse_binary(*b, *c, value, |b, c, v| Cmd::new(Cmp(b, c), v)),
            [_, _, _, 9, 9] => Cmd::new(End, value),
            [_, _, _, _, _] => Cmd::new(Data, value),
        }
    }
}

pub fn default_runtime<F, G, R>(program_gen: F, handler: G, inputs: &[isize]) -> io::Result<R>
where
    F: FnOnce() -> Vec<RefCell<Instruction>>,
    G: FnOnce(Vec<isize>) -> io::Result<R>,
{
    let program = program_gen();
    let mut rip: usize = 0;
    let mut outputs = Vec::<isize>::new();
    let mut inputs = inputs.iter();

    loop {
        let command = {
            if rip < program.len() {
                let instruction = program[rip].borrow();
                DefaultOpcodes::process_opcode(&instruction.opcode, instruction.get_value())
            } else {
                Command::<DefaultOpcodes>::new(DefaultOpcodes::End, 99)
            }
        };

        let mut command_signature = program.iter().skip(rip + 1).take(command.stride);
        rip += command.stride + 1;

        match command.iset {
            DefaultOpcodes::End => break,
            DefaultOpcodes::Data => {
                let msg = format!(
                    "program halted unexpectedly on invalid instruction: intcode {}",
                    command.value
                );
                let error = io::Error::new(io::ErrorKind::InvalidData, msg);
                return Err(error);
            }
            DefaultOpcodes::Add(mode_a, mode_b) => intcode::execute_binary_op_nose(
                command_signature,
                (mode_a, mode_b),
                &program,
                |a, b| a + b,
            ),
            DefaultOpcodes::Mul(mode_a, mode_b) => intcode::execute_binary_op_nose(
                command_signature,
                (mode_a, mode_b),
                &program,
                |a, b| a * b,
            ),
            DefaultOpcodes::Leq(mode_a, mode_b) => intcode::execute_binary_op_nose(
                command_signature,
                (mode_a, mode_b),
                &program,
                |a, b| (a < b) as isize,
            ),
            DefaultOpcodes::Cmp(mode_a, mode_b) => intcode::execute_binary_op_nose(
                command_signature,
                (mode_a, mode_b),
                &program,
                |a, b| (a == b) as isize,
            ),
            DefaultOpcodes::Jne(mode_a, mode_b) => intcode::execute_binary_op_se(
                command_signature,
                (mode_a, mode_b),
                &program,
                |p, v| {
                    if p != 0 {
                        rip = v as usize;
                    }
                },
            ),
            DefaultOpcodes::Je(mode_a, mode_b) => intcode::execute_binary_op_se(
                command_signature,
                (mode_a, mode_b),
                &program,
                |p, v| {
                    if p == 0 {
                        rip = v as usize;
                    }
                },
            ),
            DefaultOpcodes::Out(p) => match command_signature.next() {
                Some(ptr) => {
                    let output = intcode::process_parameter(&(ptr.borrow()), p, &program);
                    outputs.push(output);
                }
                _ => unreachable!(),
            },
            DefaultOpcodes::In => {
                let input = {
                    match inputs.next() {
                        Some(input) => input,
                        None => {
                            let error = io::Error::new(
                                io::ErrorKind::InvalidData,
                                "program halted waiting for input",
                            );
                            return Err(error);
                        }
                    }
                };

                match command_signature.next() {
                    Some(ptr) => {
                        let addr = ptr.borrow().get_value() as usize;
                        let mut instruction = program[addr].borrow_mut();
                        instruction.update(*input);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    handler(outputs)
}
