#![allow(dead_code)]

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> [Option<usize>; 2] {
    let beg = 372_037;
    let end = 905_157;
    let seq = (beg..=end)
        .map(|e| {
            e.to_string()
                .chars()
                .map(|e| e.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    match part {
        Part::One => {
            let one = Some(part_one(&seq[..]));
            [one, None]
        }
        Part::Two => {
            let two = Some(part_two(&seq[..]));
            [None, two]
        }
        _ => {
            let one = Some(part_one(&seq[..]));
            let two = Some(part_two(&seq[..]));
            [one, two]
        }
    }
}

fn part_one(seq: &[Vec<u32>]) -> usize {
    let seq = seq
        .iter()
        .filter(|num| {
            for d in num.windows(2) {
                if d[0] > d[1] {
                    return false;
                }
            }
            true
        })
        .filter(|num| {
            for d in num.windows(2) {
                if d[0] == d[1] {
                    return true;
                }
            }
            false
        })
        .collect::<Vec<_>>();

    seq.len()
}

fn part_two(seq: &[Vec<u32>]) -> usize {
    let seq = seq
        .iter()
        .filter(|num| {
            for d in num.windows(2) {
                if d[0] > d[1] {
                    return false;
                }
            }
            true
        })
        .filter(|num| {
            let mut digits = [0; 10];
            for d in num.windows(2) {
                if d[0] == d[1] {
                    let i = d[0] as usize;
                    digits[i] += 1;
                }
            }
            digits.contains(&1)
        })
        .collect::<Vec<_>>();

    seq.len()
}

#[cfg(any(feature = "all", feature = "day_04"))]
pub fn run(part: Part) {
    let results = main(part);
    for (i, res) in results.iter().enumerate() {
        if let Some(r) = res {
            println!("result-part({}): {}", i + 1, r);
        }
    }
}

#[cfg(not(any(feature = "all", feature = "day_04")))]
pub fn run(_: Part) {}
