#![allow(dead_code)]

use std::{cell::Cell, fs, io};

pub enum Part {
    One,
    Two,
    All,
}

enum Command {
    Add(usize, usize, usize),
    Mul(usize, usize, usize),
    Halt,
}

fn main(part: Part) -> [Option<io::Result<usize>>; 2] {
    let raw_data = fs::read_to_string("src/data/opcode.txt").unwrap();
    let opcodes = raw_data
        .split(",")
        .filter_map(|opcode| opcode.parse::<usize>().ok())
        .map(|opcode| Cell::new(opcode))
        .collect::<Vec<_>>();

    const TARGET: usize = 19_690_720;

    // NOTE: this is required only for the program specified in the file.
    let input = (12, 2);

    match part {
        Part::One => [Some(part_one(&opcodes, Some(input))), None],
        Part::Two => [None, Some(part_two(&opcodes, TARGET))],
        _ => [
            Some(part_one(&opcodes.clone(), Some(input))),
            Some(part_two(&opcodes, TARGET)),
        ],
    }
}

fn process_upcode(opcode: &[Cell<usize>]) -> Command {
    // convert from &[RefCell<usize>] to Vec<usize>
    let opcode = opcode.iter().map(|code| code.get()).collect::<Vec<usize>>();
    // check the first element, if it's an operation opcode then it must be followed by 3 args
    let cmd = opcode.first();
    match cmd {
        Some(1) if opcode.len() == 4 => Command::Add(opcode[1], opcode[2], opcode[3]),
        Some(2) if opcode.len() == 4 => Command::Mul(opcode[1], opcode[2], opcode[3]),
        _ => Command::Halt,
    }
}

fn part_one(opcodes: &[Cell<usize>], input: Option<(usize, usize)>) -> io::Result<usize> {
    if opcodes.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "expected array larger than 0.",
        ));
    }

    if let Some((noun, verb)) = input {
        opcodes[1].replace(noun);
        opcodes[2].replace(verb);
    }

    for window in opcodes.chunks(4) {
        match process_upcode(window) {
            Command::Add(left, right, target) => {
                let res = opcodes[left].get() + opcodes[right].get();
                opcodes[target].replace(res);
            }
            Command::Mul(left, right, target) => {
                let res = opcodes[left].get() * opcodes[right].get();
                opcodes[target].replace(res);
            }
            Command::Halt => break,
        }
    }

    let result = opcodes.first().unwrap();
    // replace returns the old value
    Ok(result.replace(0))
}

fn part_two(opcodes: &[Cell<usize>], target: usize) -> io::Result<usize> {
    let mut program = Vec::from(opcodes);
    //  the program basically represents the function
    //
    //      f(noun, verb) = (slope * noun + verb) + f(0, 0)
    //
    //  the value of `slope` is unknown for us at the moment, however, we can find
    //  it by first finding `f(0, 0)` which is given by simply setting the values
    //  of the verb and noun to zero:
    //
    //      f(noun, verb) - f(0, 0) = slope * noun + verb
    //
    //  notice that we also don't know the value of `verb` but because we know that
    //  verb < 100 and noun < 100, then we can simply brute force both but w/o having
    //  to compute the output of the entire program over and over again.
    let base = part_one(&program, Some((0, 0))).unwrap(); // 2106513
    program.clone_from_slice(opcodes);
    let next = part_one(&program, Some((1, 0))).unwrap(); // 2438290

    let slope = next - base;
    let target = target - base;

    let noun = {
        let mut steps = 0;
        while target - steps * slope > 99 {
            steps += 1;
        }
        steps
    };

    let verb = target - noun * slope;
    let magic = (noun, verb);

    assert_eq!(target, noun * slope + verb);

    part_one(&opcodes, Some(magic))
}

#[cfg(any(feature = "all", feature = "day_02"))]
pub fn run(part: Part) {
    let results = main(part);
    for (idx, result) in results.iter().enumerate() {
        if let Some(Ok(result)) = result {
            println!("result part({}): {}", idx + 1, result);
        }
    }
}

#[cfg(not(any(feature = "all", feature = "day_02")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part_one_works_one() {
        let v = vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50];
        let v = v
            .into_iter()
            .map(|opcode| Cell::new(opcode))
            .collect::<Vec<_>>();
        assert_eq!(3500, part_one(&v, None).unwrap());
    }

    #[test]
    fn part_one_works_two() {
        let v = vec![2, 4, 4, 5, 99, 0];
        let v = v
            .into_iter()
            .map(|opcode| Cell::new(opcode))
            .collect::<Vec<_>>();
        assert_eq!(2, part_one(&v, None).unwrap());
    }

    #[test]
    fn part_one_works_three() {
        let v = vec![1, 1, 1, 4, 99, 5, 6, 0, 99];
        let v = v
            .into_iter()
            .map(|opcode| Cell::new(opcode))
            .collect::<Vec<_>>();
        assert_eq!(30, part_one(&v, None).unwrap());
    }
}
