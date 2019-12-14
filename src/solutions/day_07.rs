#![allow(dead_code)]

use crate::intcode;
use crate::intcode::{Command, InstructionSet};
use crate::opcodes::DefaultOpcodes;
use crate::util::wrapper::Instruction;
use rand::seq::SliceRandom;
use std::collections::HashSet;
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
        _ => unimplemented!(),
    }
}

fn run_intcode_program(data: &Vec<isize>, inputs: &[isize]) -> io::Result<isize> {
    let program = data
        .iter()
        .map(|e| Instruction::new(*e))
        .map(|i| RefCell::new(i))
        .collect::<Vec<_>>();

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

    if outputs.len() != 1 {
        let msg = format!("expected a single output, received {}", outputs.len());
        let error = io::Error::new(io::ErrorKind::Other, msg);
        return Err(error);
    }

    Ok(*outputs.first().unwrap())
}

fn part_one(data: &Vec<isize>) -> io::Result<isize> {
    let mut rng = &mut rand::thread_rng();
    let possible_phases: [isize; 5] = [0, 1, 2, 3, 4];
    let powers = [10_000, 1000, 100, 10, 1];
    let mut set = HashSet::<isize>::with_capacity(120);
    let mut max_thrust = isize::min_value();

    // at this point we have tried all possible combinations
    while set.len() < 120 {
        let phases = possible_phases
            .choose_multiple(&mut rng, 5)
            .map(|e| *e)
            .collect::<Vec<_>>();
        // convert the phases array into a number, e.g. [0, 2, 3, 1, 4] -> 2314
        let n = phases
            .iter()
            .enumerate()
            .map(|(i, d)| powers[i] * d)
            .fold(0, |acc, e| acc + e);
        // only run the program if the given phases haven't been tested before.
        if set.insert(n) {
            let a = run_intcode_program(&data, &[phases[0], 0])?;
            let b = run_intcode_program(&data, &[phases[1], a])?;
            let c = run_intcode_program(&data, &[phases[2], b])?;
            let d = run_intcode_program(&data, &[phases[3], c])?;
            let e = run_intcode_program(&data, &[phases[4], d])?;
            max_thrust = std::cmp::max(e, max_thrust);
        }
    }

    Ok(max_thrust)
}

fn part_two(_data: &Vec<isize>) -> io::Result<isize> {
    Ok(0)
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
}
