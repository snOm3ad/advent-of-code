#![allow(dead_code)]
use crate::util::read_file;

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> std::io::Result<(Option<usize>, Option<usize>)> {
    let masses = read_file::<usize>("src/data/input.txt", 100)?;
    match part {
        Part::One => Ok((Some(part_one(&masses)), None)),
        Part::Two => Ok((None, Some(part_two(&masses)))),
        _ => Ok((Some(part_one(&masses)), Some(part_two(&masses)))),
    }
}

fn part_one(masses: &Vec<usize>) -> usize {
    let required_fuel = masses
        .iter()
        .map(|mass| (mass / 3) - 2)
        .collect::<Vec<usize>>();
    required_fuel.iter().fold(0, |acc, x| acc + x)
}

fn part_two(masses: &Vec<usize>) -> usize {
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

#[cfg(any(feature = "all", feature = "day_one"))]
pub fn run(part: Part) {
    match main(part) {
        Ok((Some(result_p1), None)) => {
            println!("part-one: {}", result_p1);
        }
        Ok((None, Some(result_p2))) => {
            println!("part-two: {}", result_p2);
        }
        Ok((Some(result_p1), Some(result_p2))) => {
            println!("part-one: {}  part-two: {}", result_p1, result_p2);
        }
        _ => panic!("hmmmm"),
    }
}

#[cfg(not(any(feature = "all", feature = "day_one")))]
pub fn run(_: Part) {}
