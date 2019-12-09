#![allow(dead_code)]
use crate::util::read_file;

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> [Option<usize>; 2] {
    let masses = read_file::<usize>("src/data/input.txt", 100).unwrap();
    match part {
        Part::One => [Some(part_one(&masses)), None],
        Part::Two => [None, Some(part_two(&masses))],
        _ => [Some(part_one(&masses)), Some(part_two(&masses))],
    }
}

fn part_one(masses: &[usize]) -> usize {
    let required_fuel = masses
        .iter()
        .map(|mass| (mass / 3) - 2)
        .collect::<Vec<usize>>();
    required_fuel.iter().fold(0, |acc, x| acc + x)
}

fn part_two(masses: &[usize]) -> usize {
    let required_fuel = masses
        .iter()
        .map(|mass| {
            let mut required_fuel = 0;
            let mut m = *mass;
            loop {
                match (m / 3).checked_sub(2) {
                    Some(r) => m = r,
                    _ => break,
                }
                required_fuel += m;
            }
            required_fuel
        })
        .collect::<Vec<usize>>();
    required_fuel.iter().fold(0, |acc, x| acc + x)
}

#[cfg(any(feature = "all", feature = "day_01"))]
pub fn run(part: Part) {
    let results = main(part);
    for (idx, result) in results.iter().enumerate() {
        if let Some(result) = result {
            println!("result part({}): {}", idx + 1, result);
        }
    }
}

#[cfg(not(any(feature = "all", feature = "day_01")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_works() {
        let v = [100756];
        assert_eq!(33583, part_one(&v));
    }

    #[test]
    fn part_two_works() {
        let v = [100756];
        assert_eq!(50346, part_two(&v));
    }
}
