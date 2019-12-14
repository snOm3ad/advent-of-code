#![allow(dead_code)]

use crate::opcodes;
use crate::util::wrapper::Instruction;
use permutohedron::Heap;
use std::{cell::RefCell, fs, io};

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> io::Result<[Option<isize>; 2]> {
    let input = fs::read_to_string("src/data/thrusters.txt")?;
    let data = input
        .split(',')
        .filter_map(|s| s.parse::<isize>().ok())
        .collect::<Vec<_>>();

    match part {
        Part::One => {
            let one = Some(part_one(&data)?);
            Ok([one, None])
        }
        Part::Two => {
            let two = Some(part_two(&data)?);
            Ok([None, two])
        }
        Part::All => {
            let one = Some(part_one(&data)?);
            let two = Some(part_two(&data)?);
            Ok([one, two])
        }
    }
}

fn part_one(data: &Vec<isize>) -> io::Result<isize> {
    let mut possible_phases: [isize; 5] = [0, 1, 2, 3, 4];
    let mut max_thrust = isize::min_value();
    let permutations = Heap::new(&mut possible_phases);

    // program generator, produces a brand new program everytime it is called.
    let generator = || {
        data.iter()
            .map(|e| Instruction::new(*e))
            .map(|e| RefCell::new(e))
            .collect::<Vec<_>>()
    };

    // output handler
    let handler = |outputs: Vec<isize>| -> io::Result<isize> {
        if outputs.len() != 1 {
            let msg = format!("expected a single output, received {}", outputs.len());
            let error = io::Error::new(io::ErrorKind::Other, msg);
            return Err(error);
        }

        Ok(*outputs.first().unwrap())
    };

    for phases in permutations {
        let a = opcodes::default_runtime(generator, handler, &[phases[0], 0])?;
        let b = opcodes::default_runtime(generator, handler, &[phases[1], a])?;
        let c = opcodes::default_runtime(generator, handler, &[phases[2], b])?;
        let d = opcodes::default_runtime(generator, handler, &[phases[3], c])?;
        let e = opcodes::default_runtime(generator, handler, &[phases[4], d])?;
        max_thrust = std::cmp::max(e, max_thrust);
    }

    Ok(max_thrust)
}

struct Process {
    rip: usize,
    halted: bool,
    resumed: bool,
    outputs: Vec<isize>,
    program: Vec<RefCell<Instruction>>,
}

impl Process {
    fn new(program: Vec<RefCell<Instruction>>) -> Self {
        Self {
            rip: 0,
            halted: false,
            resumed: false,
            outputs: Vec::with_capacity(8),
            program,
        }
    }

    fn run<'a, T: Iterator<Item = &'a isize>>(&mut self, mut inputs: T) -> io::Result<isize> {
        use crate::intcode;
        use crate::intcode::{Command, InstructionSet};
        use crate::opcodes::DefaultOpcodes;
        type Cmd = Command<DefaultOpcodes>;

        // we need a local copy or rip, because otherwise the JNE and JE closures will require
        // `self.rip` but this will conflict because we already take a mutable reference to `self`
        // when we call `self.program.iter().skip().take()`.
        //
        // so to avoid issues with the borrow checker we keep a local copy, and the original will
        // be updated once the program has paused or halted execution.
        let mut rip = self.rip;

        while !self.halted {
            let command = {
                if rip < self.program.len() {
                    let instruction = self.program[rip].borrow();
                    DefaultOpcodes::process_opcode(&instruction.opcode, instruction.get_value())
                } else {
                    // NOTE: here we can no longer force the end the program, we have to return an
                    // error cause the program we received was ill-formed!
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "program is ill-formed",
                    ));
                }
            };

            let mut command_signature = self.program.iter().skip(rip + 1).take(command.stride);
            rip += command.stride + 1;

            match command.iset {
                DefaultOpcodes::End => {
                    // we need to know if the program stopped due to an output or because we hit
                    // an `End` opcode, which indicates that we finished execution.
                    self.halted = true;
                    break;
                }
                DefaultOpcodes::Data => {
                    let msg = format!(
                        "program halted on illegal instruction, intcode: {}",
                        command.value
                    );
                    let error = io::Error::new(io::ErrorKind::InvalidData, msg);
                    return Err(error);
                }
                DefaultOpcodes::Add(mode_a, mode_b) => intcode::execute_binary_op_nose(
                    command_signature,
                    (mode_a, mode_b),
                    &self.program,
                    |a, b| a + b,
                ),
                DefaultOpcodes::Mul(mode_a, mode_b) => intcode::execute_binary_op_nose(
                    command_signature,
                    (mode_a, mode_b),
                    &self.program,
                    |a, b| a * b,
                ),
                DefaultOpcodes::Leq(mode_a, mode_b) => intcode::execute_binary_op_nose(
                    command_signature,
                    (mode_a, mode_b),
                    &self.program,
                    |a, b| (a < b) as isize,
                ),
                DefaultOpcodes::Cmp(mode_a, mode_b) => intcode::execute_binary_op_nose(
                    command_signature,
                    (mode_a, mode_b),
                    &self.program,
                    |a, b| (a == b) as isize,
                ),
                DefaultOpcodes::Jne(mode_a, mode_b) => intcode::execute_binary_op_se(
                    command_signature,
                    (mode_a, mode_b),
                    &self.program,
                    |p, v| {
                        if p != 0 {
                            rip = v as usize;
                        }
                    },
                ),
                DefaultOpcodes::Je(mode_a, mode_b) => intcode::execute_binary_op_se(
                    command_signature,
                    (mode_a, mode_b),
                    &self.program,
                    |p, v| {
                        if p == 0 {
                            rip = v as usize;
                        }
                    },
                ),
                DefaultOpcodes::Out(mode) => match command_signature.next() {
                    Some(ptr) => {
                        // store the result as part of the state.
                        self.outputs.push(intcode::process_parameter(
                            &(ptr.borrow()),
                            mode,
                            &self.program,
                        ));
                        break;
                    }
                    _ => unreachable!(),
                },
                DefaultOpcodes::In => {
                    // if the program has not been resumed, then we read the phase input. otherwise
                    // we read the feedback input.
                    // THE BUG IS HERE.
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
                    match command_signature.next() {
                        Some(ptr) => {
                            let addr = ptr.borrow().get_value() as usize;
                            let mut instruction = self.program[addr].borrow_mut();
                            instruction.update(input);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        // once reached this point, we know that the next time the program runs it will be resumed
        // so we set the resumed flag to true.
        self.resumed = true;
        // we also have to update the value of the `rip` with that of the local instruction
        // pointer. so that we can resume from there at a later run.
        self.rip = rip;

        match self.outputs.last() {
            Some(output) => Ok(*output),
            None => {
                let error = io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "program finished execution with no output produced",
                );
                return Err(error);
            }
        }
    }
}

fn part_two(data: &Vec<isize>) -> io::Result<isize> {
    // for part-two, things get a little more contrived.
    let mut possible_phases: [isize; 5] = [5, 6, 7, 8, 9];
    let mut max_thrust = isize::min_value();
    let permutations = Heap::new(&mut possible_phases);

    let gen = || {
        data.iter()
            .map(|e| Instruction::new(*e))
            .map(|e| RefCell::new(e))
            .collect::<Vec<_>>()
    };

    for phases in permutations {
        // one input vector for each thruster. reseted at every new `phases` permutation
        let mut inputs: [[isize; 2]; 5] = [[0; 2]; 5];
        // unfortunately, we need to generate these at every new permutation.
        let mut processes: [Process; 5] = [
            Process::new(gen()),
            Process::new(gen()),
            Process::new(gen()),
            Process::new(gen()),
            Process::new(gen()),
        ];

        // collecting using iterators will have to change the type of the inputs collection, which
        // I strongly don't want to do.
        for (i, input) in inputs.iter_mut().enumerate() {
            input[0] = phases[i];
        }

        // requirement of the challenge, the first signal input of the first process has to be
        // equal to zero
        inputs[0][1] = 0;

        for i in 0.. {
            // pid.0 - current proces pid
            // pid.1 - upcoming process pid
            let pid = (i % 5, (i + 1) % 5);
            let output = {
                if i < 5 {
                    // during the first 5 iterations the thrusters are being initialized, hence both
                    // the phase and signal inputs are required.
                    processes[pid.0].run(inputs[pid.0].iter())?
                } else {
                    // in subsequent iterations only the signal input is required.
                    processes[pid.0].run(inputs[pid.0].iter().skip(1))?
                }
            };

            // if the process that just finished running had a `pid` of 4, i.e. the last
            // process/thruster, and it just finished execution then we have reached the end of the
            // current phase iteration. so we look for the maximum and break
            if pid.0 == 4 && processes[4].halted {
                max_thrust = std::cmp::max(output, max_thrust);
                break;
            }

            // have the signal input of the next process be the output of the current proc.
            // special requirement is that the signal sent to process 'A' <-> '0' has to be zero
            // for the first time, after that has to be the output of process 'E' <-> '4'.
            inputs[pid.1][1] = output;
        }
    }

    Ok(max_thrust)
}

#[cfg(any(feature = "all", feature = "day_07"))]
pub fn run(part: Part) {
    match main(part) {
        Ok(results) => {
            for (i, result) in results.iter().enumerate() {
                if let Some(r) = result {
                    println!("result-part({}): {}", i + 1, r);
                }
            }
        }
        Err(errors) => eprintln!("[ERROR]: {}", errors),
    }
}

#[cfg(not(any(feature = "all", feature = "day_07")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod test {
    use super::*;

    mod part_one {
        #[test]
        fn easy() {
            let raw_data = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            assert_eq!(43_210, super::part_one(&data).unwrap());
        }

        #[test]
        fn medium() {
            let raw_data = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\
                            1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            assert_eq!(65_210, super::part_one(&data).unwrap());
        }
    }

    mod part_two {
        #[test]
        fn easy() {
            let raw_data = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\
                            27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            assert_eq!(139_629_729, super::part_two(&data).unwrap());
        }

        #[test]
        fn hard() {
            let raw_data = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\
                            -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\
                            53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            assert_eq!(18_216, super::part_two(&data).unwrap());
        }
    }
}
