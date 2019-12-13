use digits_iterator::*;
use std::error::Error;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

pub fn read_file<T: FromStr>(path: &str, hint: usize) -> std::io::Result<Vec<T>>
where
    <T as FromStr>::Err: 'static + Error + Send + Sync,
{
    let input = File::open(path)?;
    let br = io::BufReader::new(input);
    let mut v = Vec::with_capacity(hint);
    for line in br.lines() {
        let line = line?;
        let content = line
            .trim()
            .parse::<T>()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        v.push(content);
    }
    Ok(v)
}

pub mod wrapper {
    use super::*;

    #[derive(Default, Debug)]
    pub struct Instruction {
        value: isize,
        digits: Vec<u8>,
        pub opcode: [u8; 5],
    }

    impl Instruction {
        pub fn new(value: isize) -> Self {
            let mut ins = Self::default();
            ins.update(value);
            ins
        }

        pub fn get_value(&self) -> isize {
            self.value
        }

        pub fn update(&mut self, value: isize) {
            // if the capacity is 0, then the call comes from new, which means we need to allocate
            // memory to store the digit representation of value.
            if self.digits.capacity() == 0 {
                // allocate memory and never bother again.
                self.digits.reserve(16);
            }
            // now update the digits and upcode.
            self.value = value;
            self.update_digits();
            self.update_opcode();
        }

        fn update_digits(&mut self) {
            // need to clear the vector before doing anything, NOTE this has no effect on the
            // allocated capacity.
            self.digits.clear();
            // using the iterator avoids allocating memory for a new vector which is the desired
            // behavior. NOTE that digits will transform the isize to the respective unsigned
            // representation.
            for digit in self.value.digits() {
                self.digits.push(digit);
            }
        }

        fn update_opcode(&mut self) {
            // NOTE: before we update the opcode we need to override the current value of the
            // opcode, as we might be updating from a value with a greater number of digits to one
            // with less digits. This means that if the previous opcode was `[0, 0, 1, 2, 3]` and
            // we are updating to the value `5` then we want opcode to be `[0, 0, 0, 0, 5]` as
            // opposed of `[0, 0, 1, 2, 5]`.
            self.opcode = [0; 5];
            // now update the upcode.
            let opcode = self.digits.iter().rev().take(5);
            for (i, digit) in opcode.enumerate() {
                self.opcode[4 - i] = *digit;
            }
        }
    }
}
