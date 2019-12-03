mod util;
use util::read_file;

mod day_one {
    use super::*;

    #[allow(dead_code)]
    pub enum Part {
        One,
        Two,
        All,
    }

    pub fn run(part: Part) -> std::io::Result<(Option<usize>, Option<usize>)> {
        let masses = read_file::<usize>("src/data/input.txt", 100)?;
        match part {
            Part::One => Ok((Some(part_one(&masses)), None)),
            Part::Two => Ok((None, Some(part_two(&masses)))),
            _ => Ok((Some(part_one(&masses)), Some(part_two(&masses)))),
        }
    }

    pub fn part_one(masses: &Vec<usize>) -> usize {
        let required_fuel = masses
            .iter()
            .map(|mass| (mass / 3) - 2)
            .collect::<Vec<usize>>();
        required_fuel.iter().fold(0, |acc, x| acc + x)
    }

    pub fn part_two(masses: &Vec<usize>) -> usize {
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
}

fn main() {
    match day_one::run(day_one::Part::All) {
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
