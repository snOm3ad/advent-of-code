#![allow(dead_code)]

use crate::util::wrapper;
use std::{cell::Ref, cell::RefCell, fs, io};

pub enum Part {
    One,
    Two,
    All,
}

#[derive(Debug)]
enum ParamMode {
    Immediate,
    Address,
}

mod part_one {
    use super::*;

    #[derive(Debug)]
    pub(super) enum InstructionType {
        Add(ParamMode, ParamMode),
        Mul(ParamMode, ParamMode),
        Out(ParamMode),
        In,
        End,
        Data,
    }

    #[derive(Debug)]
    pub(super) struct Command {
        pub itype: InstructionType,
        pub stride: usize,
        pub value: isize,
    }

    impl Command {
        pub(super) fn new(itype: InstructionType, value: isize) -> Self {
            let stride = match itype {
                InstructionType::Mul(_, _) | InstructionType::Add(_, _) => 3,
                InstructionType::Out(_) | InstructionType::In => 1,
                InstructionType::End | InstructionType::Data => 0,
            };
            Command {
                itype,
                stride,
                value,
            }
        }
    }

    pub(super) fn process_opcode(opcode: &[u8; 5], value: isize) -> Command {
        // if the value is less than zero then it for sure isn't an instruction.
        if value < 0 {
            return Command::new(InstructionType::Data, value);
        }

        match opcode {
            [0, b, c, 0, 1] => {
                // dereference the parameter modes
                let (b, c) = (*b, *c);
                if b > 1 || c > 1 {
                    Command::new(InstructionType::Data, value)
                } else {
                    let params = {
                        use ParamMode::*;
                        match (b, c) {
                            (0, 0) => (Address, Address),
                            (1, 1) => (Immediate, Immediate),
                            (0, 1) => (Address, Immediate),
                            (1, 0) => (Immediate, Address),
                            (_, _) => unreachable!(),
                        }
                    };
                    Command::new(InstructionType::Add(params.0, params.1), value)
                }
            }
            [0, b, c, 0, 2] => {
                let (b, c) = (*b, *c);
                if b > 1 || c > 1 {
                    Command::new(InstructionType::Data, value)
                } else {
                    let params = {
                        use ParamMode::*;
                        match (b, c) {
                            (0, 0) => (Address, Address),
                            (1, 1) => (Immediate, Immediate),
                            (0, 1) => (Address, Immediate),
                            (1, 0) => (Immediate, Address),
                            (_, _) => unreachable!(),
                        }
                    };
                    Command::new(InstructionType::Mul(params.0, params.1), value)
                }
            }
            [0, 0, 0, 0, 3] => Command::new(InstructionType::In, value),
            [0, 0, c, 0, 4] => {
                let c = *c;
                if c > 1 {
                    Command::new(InstructionType::Data, value)
                } else {
                    let param = match c {
                        0 => ParamMode::Address,
                        1 => ParamMode::Immediate,
                        _ => unreachable!(),
                    };
                    Command::new(InstructionType::Out(param), value)
                }
            }
            [_, _, _, 9, 9] => Command::new(InstructionType::End, value),
            [_, _, _, _, _] => Command::new(InstructionType::Data, value),
        }
    }
}

fn main(part: Part) -> io::Result<[Option<isize>; 2]> {
    let raw_data = fs::read_to_string("src/data/opcode_adv.txt")?;
    let data = raw_data
        .split(',')
        .filter_map(|s| s.parse::<isize>().ok())
        .collect::<Vec<_>>();

    match part {
        Part::One => {
            let inputs = [1];
            let one = Some(part_one(&data, &inputs)?);
            Ok([one, None])
        }
        _ => unimplemented!(),
    }
}

fn process_parameter<'a>(
    ptr: &Ref<'a, wrapper::Instruction>,
    mode: ParamMode,
    program: &Vec<RefCell<wrapper::Instruction>>,
) -> isize {
    match mode {
        ParamMode::Address => {
            let addr = ptr.get_value() as usize;
            let cell = program[addr].borrow();
            cell.get_value()
        }
        ParamMode::Immediate => ptr.get_value(),
    }
}

fn execute_binary_op<'a, T, F>(
    mut block: T,
    modes: (ParamMode, ParamMode),
    program: &Vec<RefCell<wrapper::Instruction>>,
    binary_op: F,
) where
    T: Iterator<Item = &'a RefCell<wrapper::Instruction>>,
    F: FnOnce(isize, isize) -> isize,
{
    let param_one = match block.next() {
        Some(p) => process_parameter(&(p.borrow()), modes.1, program),
        _ => unreachable!(),
    };

    let param_two = match block.next() {
        Some(p) => process_parameter(&(p.borrow()), modes.0, program),
        _ => unreachable!(),
    };

    let address = match block.next() {
        Some(p) => p.borrow().get_value() as usize,
        _ => unreachable!(),
    };

    let mut instruction = program[address].borrow_mut();
    instruction.update(binary_op(param_one, param_two));
}

fn part_one(data: &Vec<isize>, inputs: &[isize]) -> io::Result<isize> {
    use part_one::*;
    use wrapper::Instruction;

    // NOTE: the reason why we create the program here is because we want to be able to re-use the
    // parsed data coming from the file.
    let program = data
        .iter()
        .map(|ins| Instruction::new(*ins))
        .map(|ins| RefCell::new(ins))
        .collect::<Vec<_>>();

    // the instruction pointer.
    let mut rip = program.iter();
    // inputs and outputs
    let mut outputs: Vec<isize> = Vec::new();
    let mut inputs = inputs.iter();

    loop {
        // first get the details of the command to be executed.
        let command = {
            match rip.next() {
                Some(address) => {
                    let instruction = address.borrow();
                    process_opcode(&instruction.opcode, instruction.get_value())
                }
                None => Command::new(InstructionType::End, 99),
            }
        };

        // contains the parameters required to execute the command.
        let mut cmd_signature = rip.by_ref().take(command.stride);

        // now that we have the details of the command we execute it.
        match command.itype {
            InstructionType::End => break,
            InstructionType::Data => {
                let msg = format!(
                    "program halted on invalid instruction, instruction value: {}",
                    command.value
                );
                let error = io::Error::new(io::ErrorKind::Interrupted, msg);
                return Err(error);
            }
            InstructionType::Add(mode_a, mode_b) => {
                execute_binary_op(cmd_signature, (mode_a, mode_b), &program, |a, b| a + b)
            }
            InstructionType::Mul(mode_a, mode_b) => {
                execute_binary_op(cmd_signature, (mode_a, mode_b), &program, |a, b| a * b)
            }
            InstructionType::Out(mode) => match cmd_signature.next() {
                Some(param) => {
                    let output = process_parameter(&(param.borrow()), mode, &program);
                    outputs.push(output);
                }
                _ => unreachable!(),
            },
            InstructionType::In => {
                // This one is also simple, grab the input from the inputs iterator, note that if
                // we reach this instruction and the iterator has been exhausted something our
                // program was wrong, hence we return an error.
                let input = match inputs.next() {
                    Some(input) => input,
                    None => {
                        let error = io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "program halted waiting for input.",
                        );
                        return Err(error);
                    }
                };

                // the block iterator contains the a pointer to the address to which we write the input to.
                match cmd_signature.next() {
                    Some(ptr) => {
                        let write_addr = ptr.borrow().get_value() as usize;
                        let mut instruction = program[write_addr].borrow_mut();
                        instruction.update(*input);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    let diagnostic_code = outputs
        .into_iter()
        .skip_while(|e| *e == 0)
        .collect::<Vec<_>>();

    if diagnostic_code.len() > 1 {
        let error = io::Error::new(
            io::ErrorKind::InvalidData,
            "program run unsuccessfully, more than one non-zero output code",
        );
        return Err(error);
    }

    match diagnostic_code.first() {
        Some(code) => Ok(*code),
        _ => unreachable!(),
    }
}

#[cfg(any(feature = "all", feature = "day_05"))]
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

#[cfg(not(any(feature = "all", feature = "day_05")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod test {
    use super::*;

    mod part_one {
        use super::*;

        #[test]
        #[should_panic]
        fn ill_formed() {
            // the empty program
            let data = vec![99];
            let inputs = [];
            part_one(&data, &inputs).unwrap();
        }

        #[test]
        fn echo() {
            let data = vec![3, 0, 4, 0, 99];
            let inputs = [1];
            assert_eq!(1, part_one(&data, &inputs).unwrap());
        }

        #[test]
        fn easy() {
            let raw_data = "1101,10,-8,4,0,2,5,2,1002,2,5,0,4,0,99";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            let inputs = [];
            assert_eq!(-80, part_one(&data, &inputs).unwrap());
        }
    }
}
