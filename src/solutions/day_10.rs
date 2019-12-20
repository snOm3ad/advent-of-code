#![allow(dead_code)]

use std::{fmt, io};

pub enum Part {
    One,
    Two,
    All,
}

struct Station {
    loc: (isize, isize),
    count: usize,
}

impl Station {
    fn new(x: isize, y: isize, count: usize) -> Self {
        Self { loc: (x, y), count }
    }
}

impl fmt::Display for Station {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "location: ({}, {}), count: {}",
            self.loc.0, self.loc.1, self.count
        )
    }
}

fn main(part: Part) -> io::Result<[Option<Box<dyn fmt::Display>>; 2]> {
    match part {
        Part::One => {
            let one = Box::new(part_one());
            Ok([Some(one), None])
        }
        _ => unimplemented!(),
    }
}

fn part_one() -> Station {
    Station::new(0, 0, 0)
}

#[cfg(any(feature = "all", feature = "day_10"))]
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

#[cfg(not(any(feature = "all", feature = "day_10")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part_one() {
        let raw_data = ".#..#\n\
                        .....\n\
                        #####\n\
                        ....#\n\
                        ...##";
    }
}
