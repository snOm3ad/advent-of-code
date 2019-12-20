#![allow(dead_code)]
use crate::intcode::{Command, InstructionSet, ParamMode};
use crate::util::wrapper::Instruction;
use std::{cell::RefCell, convert::TryFrom, fmt, fs, io, marker::PhantomData};

pub enum Part {
    One,
    Two,
    All,
}

#[derive(Debug)]
enum Opcodes {
    Add(ParamMode, ParamMode, ParamMode),
    Mul(ParamMode, ParamMode, ParamMode),
    Leq(ParamMode, ParamMode, ParamMode),
    Cmp(ParamMode, ParamMode, ParamMode),
    Jne(ParamMode, ParamMode),
    Je(ParamMode, ParamMode),
    Mov(ParamMode),
    Out(ParamMode),
    In(ParamMode),
    End,
    Data,
}

impl Default for Opcodes {
    fn default() -> Self {
        Opcodes::Data
    }
}

impl InstructionSet for Opcodes {
    fn get_stride(&self) -> usize {
        use Opcodes::*;
        match self {
            Add(_, _, _) | Mul(_, _, _) | Leq(_, _, _) | Cmp(_, _, _) => 3,
            Jne(_, _) | Je(_, _) => 2,
            Out(_) | In(_) | Mov(_) => 1,
            End | Data => 0,
        }
    }

    fn parse<F>(params: [u8; 3], rbase: &isize, value: isize, gen: F) -> Command<Self>
    where
        F: FnOnce([ParamMode; 3]) -> Command<Self>,
    {
        let map = [
            ParamMode::Address(0),
            ParamMode::Immediate,
            ParamMode::Address(*rbase),
        ];

        match params {
            [0, b, c] if b < 3 && c < 3 => gen([map[0], map[b as usize], map[c as usize]]),
            [2, b, c] if b < 3 && c < 3 => gen([map[2], map[b as usize], map[c as usize]]),
            [_, _, _] => Command::<Opcodes>::new(Self::default(), value),
        }
    }

    fn process_opcode(opcode: &[u8; 5], rbase: &isize, value: isize) -> Command<Self> {
        type Cmd = Command<Opcodes>;

        // if it's negative its clear it must be data.
        if value < 0 {
            return Cmd::new(Self::default(), value);
        }

        let params = <[u8; 3]>::try_from(&opcode[..3]).unwrap();
        let instruction = <[u8; 2]>::try_from(&opcode[3..]).unwrap();

        match instruction {
            [0, 1] => Self::parse(params, rbase, value, |p| match p {
                [a, b, c] => Cmd::new(Opcodes::Add(a, b, c), value),
            }),
            [0, 2] => Self::parse(params, rbase, value, |p| match p {
                [a, b, c] => Cmd::new(Opcodes::Mul(a, b, c), value),
            }),
            [0, 3] => Self::parse(params, rbase, value, |p| match p {
                [_, _, c] => Cmd::new(Opcodes::In(c), value),
            }),
            [0, 4] => Self::parse(params, rbase, value, |p| match p {
                [_, _, c] => Cmd::new(Opcodes::Out(c), value),
            }),
            [0, 5] => Self::parse(params, rbase, value, |p| match p {
                [_, b, c] => Cmd::new(Opcodes::Jne(b, c), value),
            }),
            [0, 6] => Self::parse(params, rbase, value, |p| match p {
                [_, b, c] => Cmd::new(Opcodes::Je(b, c), value),
            }),
            [0, 7] => Self::parse(params, rbase, value, |p| match p {
                [a, b, c] => Cmd::new(Opcodes::Leq(a, b, c), value),
            }),
            [0, 8] => Self::parse(params, rbase, value, |p| match p {
                [a, b, c] => Cmd::new(Opcodes::Cmp(a, b, c), value),
            }),
            [0, 9] => Self::parse(params, rbase, value, |p| match p {
                [_, _, c] => Cmd::new(Opcodes::Mov(c), value),
            }),
            [9, 9] => Cmd::new(Opcodes::End, value),
            [_, _] => Cmd::new(Opcodes::Data, value),
        }
    }
}

struct Process<T: InstructionSet> {
    mkr: PhantomData<T>,
    rip: usize,
    rbase: isize,
    halted: bool,
    outputs: Vec<isize>,
    program: Vec<RefCell<Instruction>>,
}

impl Process<Opcodes> {
    fn new(program: Vec<RefCell<Instruction>>) -> Self {
        Self {
            mkr: PhantomData,
            rip: 0,
            rbase: 0,
            halted: false,
            outputs: Vec::with_capacity(8),
            program,
        }
    }

    fn run<'a, T: Iterator<Item = &'a isize>>(&mut self, mut inputs: T) -> io::Result<isize> {
        use crate::intcode;
        type Cmd = Command<Opcodes>;
        // closures will modify these two values, avoid taking ref to self by having them as local
        // variables.
        let mut rip = self.rip;

        while !self.halted {
            let command = {
                if rip < self.program.len() {
                    let instruction = self.program[rip].borrow();
                    Opcodes::process_opcode(
                        &instruction.opcode,
                        &self.rbase,
                        instruction.get_value(),
                    )
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "program is ill-formed",
                    ));
                }
            };

            let mut command_signature = self.program.iter().skip(rip + 1).take(command.stride);
            rip += command.stride + 1;

            match command.iset {
                Opcodes::End => {
                    self.halted = true;
                    break;
                }
                Opcodes::Data => {
                    let msg = format!(
                        "program halted on illegal instruction, opcode: {}",
                        command.value
                    );
                    let error = io::Error::new(io::ErrorKind::InvalidData, msg);
                    return Err(error);
                }
                Opcodes::Add(a, b, c) => intcode::execute_binary_op_nose(
                    command_signature,
                    (a, b, c),
                    &self.program,
                    |a, b| a + b,
                ),
                Opcodes::Mul(a, b, c) => intcode::execute_binary_op_nose(
                    command_signature,
                    (a, b, c),
                    &self.program,
                    |a, b| a * b,
                ),
                Opcodes::Leq(a, b, c) => intcode::execute_binary_op_nose(
                    command_signature,
                    (a, b, c),
                    &self.program,
                    |a, b| (a < b) as isize,
                ),
                Opcodes::Cmp(a, b, c) => intcode::execute_binary_op_nose(
                    command_signature,
                    (a, b, c),
                    &self.program,
                    |a, b| (a == b) as isize,
                ),
                Opcodes::Jne(b, c) => intcode::execute_binary_op_se(
                    command_signature,
                    (b, c),
                    &self.program,
                    |p, v| {
                        if p != 0 {
                            rip = v as usize;
                        }
                    },
                ),
                Opcodes::Je(b, c) => intcode::execute_binary_op_se(
                    command_signature,
                    (b, c),
                    &self.program,
                    |p, v| {
                        if p == 0 {
                            rip = v as usize;
                        }
                    },
                ),
                Opcodes::Mov(mode) => {
                    // modify rbase.
                    let cell = command_signature.next().unwrap();
                    let value = intcode::process_parameter(cell.borrow(), mode, &self.program);
                    self.rbase += value;
                }
                Opcodes::Out(mode) => {
                    // save output in vector.
                    let cell = command_signature.next().unwrap();
                    self.outputs.push(intcode::process_parameter(
                        cell.borrow(),
                        mode,
                        &self.program,
                    ));
                }
                Opcodes::In(mode) => {
                    let input = match inputs.next() {
                        Some(input) => *input,
                        None => {
                            let error = io::Error::new(
                                io::ErrorKind::InvalidInput,
                                "program halted waiting for input",
                            );
                            return Err(error);
                        }
                    };

                    let cell = command_signature.next().unwrap();
                    // here we want a reference to the
                    match mode {
                        ParamMode::Address(rbase) => {
                            let offset = cell.borrow().get_value();
                            let addr = (rbase + offset) as usize;
                            let mut instruction = self.program[addr].borrow_mut();
                            instruction.update(input);
                        }
                        ParamMode::Immediate => unreachable!(),
                    }
                }
            }
        }

        self.rip = rip;
        // return last output...
        match self.outputs.last() {
            Some(output) => Ok(*output),
            None => {
                let error = io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "program finished with no output produced.",
                );
                Err(error)
            }
        }
    }
}

fn main(part: Part) -> io::Result<[Option<Box<dyn fmt::Display>>; 2]> {
    let raw_data = fs::read_to_string("src/data/program.txt")?;
    let data = raw_data
        .split(',')
        .filter_map(|s| s.parse::<isize>().ok())
        .collect::<Vec<_>>();

    match part {
        Part::One => {
            let one = Box::new(part_one(&data)?);
            Ok([Some(one), None])
        }
        Part::Two => {
            let two = Box::new(part_two(&data)?);
            Ok([None, Some(two)])
        }
        Part::All => {
            let one = Box::new(part_one(&data)?);
            let two = Box::new(part_two(&data)?);
            Ok([Some(one), Some(two)])
        }
    }
}

fn part_one(data: &Vec<isize>) -> io::Result<isize> {
    let mut program = data
        .iter()
        .copied()
        .map(|e| Instruction::new(e))
        .map(|e| RefCell::new(e))
        .collect::<Vec<_>>();

    // program memory needs to be bigger
    program.resize_with(2048, Default::default);

    let mut prog = Process::<Opcodes>::new(program);
    prog.run([1].iter())
}

fn part_two(data: &Vec<isize>) -> io::Result<isize> {
    let mut program = data
        .iter()
        .copied()
        .map(|e| Instruction::new(e))
        .map(|e| RefCell::new(e))
        .collect::<Vec<_>>();

    // program memory needs to be bigger
    program.resize_with(2048, Default::default);

    let mut prog = Process::<Opcodes>::new(program);
    prog.run([2].iter())
}

#[cfg(any(feature = "all", feature = "day_09"))]
pub fn run(part: Part) {
    match main(part) {
        Ok(results) => {
            for (i, result) in results.iter().enumerate() {
                if let Some(r) = result {
                    println!("result-part({}):\n{}", i + 1, *r);
                }
            }
        }
        Err(errors) => eprintln!("[ERROR]: {}", errors),
    }
}

#[cfg(not(any(feature = "all", feature = "day_09")))]
pub fn run(_: Part) {}
