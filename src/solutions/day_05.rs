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

trait InstructionSet {
    fn get_stride(&self) -> usize;
}

#[derive(Debug)]
enum IsetPartOne {
    Add(ParamMode, ParamMode),
    Mul(ParamMode, ParamMode),
    Out(ParamMode),
    In,
    End,
    Data,
}

impl Default for IsetPartOne {
    fn default() -> Self {
        IsetPartOne::Data
    }
}

impl InstructionSet for IsetPartOne {
    fn get_stride(&self) -> usize {
        use IsetPartOne::*;
        match self {
            Add(_, _) | Mul(_, _) => 3,
            Out(_) | In => 1,
            End | Data => 0,
        }
    }
}

#[derive(Debug)]
enum IsetPartTwo {
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

impl Default for IsetPartTwo {
    fn default() -> Self {
        IsetPartTwo::Data
    }
}

impl InstructionSet for IsetPartTwo {
    fn get_stride(&self) -> usize {
        use IsetPartTwo::*;
        match self {
            Add(_, _) | Mul(_, _) | Leq(_, _) | Cmp(_, _) => 3,
            Jne(_, _) | Je(_, _) => 2,
            Out(_) | In => 1,
            End | Data => 0,
        }
    }
}

#[derive(Debug)]
struct Command<T: Default + InstructionSet> {
    iset: T,
    stride: usize,
    value: isize,
}

impl<T: Default + InstructionSet> Command<T> {
    fn new(iset: T, value: isize) -> Self {
        let stride = iset.get_stride();
        Self {
            iset,
            stride,
            value,
        }
    }
}

fn parse_unary<T, F>(p: u8, value: isize, gen: F) -> Command<T>
where
    T: Default + InstructionSet,
    F: FnOnce(ParamMode, isize) -> Command<T>,
{
    if p > 1 {
        return Command::<T>::new(T::default(), value);
    }

    let p = match p {
        0 => ParamMode::Address,
        1 => ParamMode::Immediate,
        _ => unreachable!(),
    };

    gen(p, value)
}

fn parse_binary<F, T>(b: u8, c: u8, value: isize, gen: F) -> Command<T>
where
    T: Default + InstructionSet,
    F: FnOnce(ParamMode, ParamMode, isize) -> Command<T>,
{
    use ParamMode::*;
    if b > 1 || c > 1 {
        return Command::<T>::new(T::default(), value);
    }

    let (b, c) = match (b, c) {
        (0, 0) => (Address, Address),
        (1, 1) => (Immediate, Immediate),
        (0, 1) => (Address, Immediate),
        (1, 0) => (Immediate, Address),
        (_, _) => unreachable!(),
    };

    gen(b, c, value)
}

mod part_one {
    use super::*;
    pub(super) type Cmd = Command<IsetPartOne>;

    pub(super) fn process_opcode(opcode: &[u8; 5], value: isize) -> Command<IsetPartOne> {
        // if the value is less than zero then it for sure isn't an instruction.
        if value < 0 {
            return Cmd::new(IsetPartOne::default(), value);
        }

        match opcode {
            [0, b, c, 0, 1] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartOne::Add(b, c), v))
            }
            [0, b, c, 0, 2] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartOne::Mul(b, c), v))
            }
            [0, 0, 0, 0, 3] => Cmd::new(IsetPartOne::In, value),
            [0, 0, c, 0, 4] => parse_unary(*c, value, |p, v| Cmd::new(IsetPartOne::Out(p), v)),
            [_, _, _, 9, 9] => Cmd::new(IsetPartOne::End, value),
            [_, _, _, _, _] => Cmd::new(IsetPartOne::Data, value),
        }
    }
}

mod part_two {
    use super::*;
    pub(super) type Cmd = Command<IsetPartTwo>;

    pub(super) fn process_opcode(opcode: &[u8; 5], value: isize) -> Command<IsetPartTwo> {
        // if the value is less than zero then it for sure isn't an instruction.
        if value < 0 {
            return Cmd::new(IsetPartTwo::default(), value);
        }

        match opcode {
            [0, b, c, 0, 1] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartTwo::Add(b, c), v))
            }
            [0, b, c, 0, 2] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartTwo::Mul(b, c), v))
            }
            [0, 0, 0, 0, 3] => Cmd::new(IsetPartTwo::In, value),
            [0, 0, c, 0, 4] => parse_unary(*c, value, |p, v| Cmd::new(IsetPartTwo::Out(p), v)),
            [0, b, c, 0, 5] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartTwo::Jne(b, c), v))
            }
            [0, b, c, 0, 6] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartTwo::Je(b, c), v))
            }
            [0, b, c, 0, 7] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartTwo::Leq(b, c), v))
            }
            [0, b, c, 0, 8] => {
                parse_binary(*b, *c, value, |b, c, v| Cmd::new(IsetPartTwo::Cmp(b, c), v))
            }
            [_, _, _, 9, 9] => Cmd::new(IsetPartTwo::End, value),
            [_, _, _, _, _] => Cmd::new(IsetPartTwo::Data, value),
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
        Part::Two => {
            let inputs = [5];
            let two = Some(part_two(&data, &inputs)?);
            Ok([None, two])
        }
        _ => {
            let inputs = [[1], [5]];
            let one = Some(part_one(&data, &inputs[0])?);
            let two = Some(part_two(&data, &inputs[1])?);
            Ok([one, two])
        }
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

fn execute_binary_op_nose<'a, T, F>(
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
        Some(ptr) => ptr.borrow().get_value() as usize,
        _ => unreachable!(),
    };

    let mut instruction = program[address].borrow_mut();
    instruction.update(binary_op(param_one, param_two));
}

fn execute_binary_op_se<'a, T, F>(
    mut block: T,
    modes: (ParamMode, ParamMode),
    program: &Vec<RefCell<wrapper::Instruction>>,
    binary_op: F,
) where
    T: Iterator<Item = &'a RefCell<wrapper::Instruction>>,
    F: FnOnce(isize, isize),
{
    let param_one = match block.next() {
        Some(p) => process_parameter(&(p.borrow()), modes.1, program),
        _ => unreachable!(),
    };

    let param_two = match block.next() {
        Some(p) => process_parameter(&(p.borrow()), modes.0, program),
        _ => unreachable!(),
    };

    binary_op(param_one, param_two);
}

fn part_two(data: &Vec<isize>, inputs: &[isize]) -> io::Result<isize> {
    use part_two::Cmd;
    use wrapper::Instruction;

    let program = data
        .iter()
        .map(|e| Instruction::new(*e))
        .map(|i| RefCell::new(i))
        .collect::<Vec<_>>();

    // instruction pointer, cannot be iterator as code is no longer linear.
    let mut rip = 0usize;
    // inputs and outputs
    let mut inputs = inputs.iter();
    let mut outputs: Vec<isize> = Vec::new();

    loop {
        let command = {
            if rip < program.len() {
                let instruction = program[rip].borrow();
                part_two::process_opcode(&instruction.opcode, instruction.get_value())
            } else {
                Cmd::new(IsetPartTwo::End, 99)
            }
        };

        // the good news is that we can still use iterators to get the command signature.
        let mut command_signature = program.iter().skip(rip + 1).take(command.stride);
        // NOTE: the `JNE` and `JE` instructions may modify the instruction pointer condtionally,
        // but in which case they will override this. In the case that they don't then the program
        // proceeds sequentially.
        rip += command.stride + 1;

        match command.iset {
            IsetPartTwo::End => break,
            IsetPartTwo::Data => {
                let msg = format!(
                    "program halted on invalid instruction, instruction value: {}",
                    command.value
                );
                let error = io::Error::new(io::ErrorKind::InvalidInput, msg);
                return Err(error);
            }
            IsetPartTwo::Add(mode_a, mode_b) => {
                execute_binary_op_nose(command_signature, (mode_a, mode_b), &program, |a, b| a + b)
            }
            IsetPartTwo::Mul(mode_a, mode_b) => {
                execute_binary_op_nose(command_signature, (mode_a, mode_b), &program, |a, b| a * b)
            }
            IsetPartTwo::Leq(mode_a, mode_b) => {
                execute_binary_op_nose(command_signature, (mode_a, mode_b), &program, |a, b| {
                    (a < b) as isize
                })
            }
            IsetPartTwo::Cmp(mode_a, mode_b) => {
                execute_binary_op_nose(command_signature, (mode_a, mode_b), &program, |a, b| {
                    (a == b) as isize
                })
            }
            IsetPartTwo::Jne(mode_a, mode_b) => {
                execute_binary_op_se(command_signature, (mode_a, mode_b), &program, |p, v| {
                    if p != 0 {
                        rip = v as usize;
                    }
                })
            }
            IsetPartTwo::Je(mode_a, mode_b) => {
                execute_binary_op_se(command_signature, (mode_a, mode_b), &program, |p, v| {
                    if p == 0 {
                        rip = v as usize;
                    }
                })
            }
            IsetPartTwo::Out(mode) => match command_signature.next() {
                Some(param) => {
                    let output = process_parameter(&(param.borrow()), mode, &program);
                    outputs.push(output);
                }
                _ => unreachable!(),
            },
            IsetPartTwo::In => {
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

                match command_signature.next() {
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

    if outputs.is_empty() {
        let error = io::Error::new(
            io::ErrorKind::InvalidInput,
            "expected one output code, found 0",
        );
        return Err(error);
    }

    Ok(*outputs.first().unwrap())
}

fn part_one(data: &Vec<isize>, inputs: &[isize]) -> io::Result<isize> {
    use part_one::Cmd;
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
                    part_one::process_opcode(&instruction.opcode, instruction.get_value())
                }
                None => Cmd::new(IsetPartOne::End, 99),
            }
        };

        // contains the parameters required to execute the command.
        let mut command_signature = rip.by_ref().take(command.stride);

        // now that we have the details of the command we execute it.
        match command.iset {
            IsetPartOne::End => break,
            IsetPartOne::Data => {
                let msg = format!(
                    "program halted on invalid instruction, instruction value: {}",
                    command.value
                );
                let error = io::Error::new(io::ErrorKind::Interrupted, msg);
                return Err(error);
            }
            IsetPartOne::Add(mode_a, mode_b) => {
                execute_binary_op_nose(command_signature, (mode_a, mode_b), &program, |a, b| a + b)
            }
            IsetPartOne::Mul(mode_a, mode_b) => {
                execute_binary_op_nose(command_signature, (mode_a, mode_b), &program, |a, b| a * b)
            }
            IsetPartOne::Out(mode) => match command_signature.next() {
                Some(param) => {
                    let output = process_parameter(&(param.borrow()), mode, &program);
                    outputs.push(output);
                }
                _ => unreachable!(),
            },
            IsetPartOne::In => {
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
                match command_signature.next() {
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
        None => {
            let error = io::Error::new(io::ErrorKind::InvalidData, "no output code was found");
            Err(error)
        }
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

    mod part_two {
        use super::*;

        #[test]
        fn easy() {
            let data = vec![1, 0, 3, 3, 1005, 2, 10, 5, 1, 0, 4, 1, 99];
            let inputs = [8];
            assert_eq!(0, part_two(&data, &inputs).unwrap());
        }

        #[test]
        fn countdown() {
            let data = vec![101, -1, 7, 7, 4, 7, 1105, 11, 0, 99];
            // outputs: 10, 9, 8, ..., 1, 0 since the second part returns the first element in the
            // output vector we assert that is a 10.
            assert_eq!(10, part_two(&data, &[]).unwrap());
        }

        #[test]
        fn output_self() {
            let raw_data = "101,-5,5,5,4,5,101,6,5,5,1007,5,23,15,1105,1,0,99";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            // literally outputs itself, so the first element has to be the first instruction i.e. 101
            assert_eq!(101, part_two(&data, &[]).unwrap());
        }

        #[test]
        fn medium() {
            let raw_data = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
                            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
                            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";
            let data = raw_data
                .split(',')
                .filter_map(|s| s.parse::<isize>().ok())
                .collect::<Vec<_>>();
            let inputs = [8];
            assert_eq!(1000, part_two(&data, &inputs).unwrap());
        }
    }
}
