use super::util::wrapper;
use std::cell::{Ref, RefCell};

pub mod opcodes;

#[derive(Debug)]
pub enum ParamMode {
    Immediate,
    Address,
}

pub trait InstructionSet {
    fn get_stride(&self) -> usize;
    fn process_opcode(_: &[u8; 5], _: isize) -> Command<Self>
    where
        Self: Default;
}

#[derive(Debug)]
pub struct Command<T: Default + InstructionSet> {
    pub iset: T,
    pub stride: usize,
    pub value: isize,
}

impl<T: Default + InstructionSet> Command<T> {
    pub fn new(iset: T, value: isize) -> Self {
        let stride = iset.get_stride();
        Self {
            iset,
            stride,
            value,
        }
    }
}

pub fn parse_unary<T, F>(p: u8, value: isize, gen: F) -> Command<T>
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

pub fn parse_binary<F, T>(b: u8, c: u8, value: isize, gen: F) -> Command<T>
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

pub fn process_parameter<'a>(
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

pub fn execute_binary_op_nose<'a, T, F>(
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

pub fn execute_binary_op_se<'a, T, F>(
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
