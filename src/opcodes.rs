use super::intcode;
use super::intcode::{Command, InstructionSet, ParamMode};

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
