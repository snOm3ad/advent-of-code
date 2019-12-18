use super::util::wrapper;
use std::cell::{Ref, RefCell};

#[derive(Debug, Clone, Copy)]
pub enum ParamMode {
    Immediate,
    Address(isize),
}

pub trait InstructionSet {
    /// determines the stride, i.e. total number of parameters a given instruction needs
    fn get_stride(&self) -> usize;

    /// determines the appropriate instruction from a given opcode
    fn process_opcode(_: &[u8; 5], rbase: &isize, value: isize) -> Command<Self>
    where
        Self: Default;

    /// parses arguments of a given opcode
    fn parse<F>(params: [u8; 3], rbase: &isize, value: isize, gen: F) -> Command<Self>
    where
        Self: Default,
        F: FnOnce([ParamMode; 3]) -> Command<Self>;
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

pub fn process_parameter<'a>(
    ptr: &Ref<'a, wrapper::Instruction>,
    mode: ParamMode,
    program: &Vec<RefCell<wrapper::Instruction>>,
) -> isize {
    match mode {
        ParamMode::Address(0) => {
            let addr = ptr.get_value() as usize;
            let cell = program[addr].borrow();
            cell.get_value()
        }
        ParamMode::Immediate => ptr.get_value(),
        ParamMode::Address(rb) => {
            let offset = ptr.get_value();
            let addr = (rb + offset) as usize;
            let cell = program[addr].borrow();
            cell.get_value()
        }
    }
}

/// Executes binary operation with **no** side effect, the binary operation is required to return a
/// result which is written to the specified address of the third parameter.
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

/// Executes binary operation with side effect, the binary operation returns `()`
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
