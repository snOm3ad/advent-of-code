#![allow(dead_code)]

use std::io;

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> io::Result<[Option<usize>; 2]> {
    match part {
        Part::One => {
            let one = Some(part_one());
            Ok([one, None])
        }
        _ => unimplemented!(),
    }
}

fn part_one() -> usize {
    0
}

#[cfg(any(feature = "all", feature = "day_08"))]
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

#[cfg(not(any(feature = "all", feature = "day_08")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn part_one() {}
}
