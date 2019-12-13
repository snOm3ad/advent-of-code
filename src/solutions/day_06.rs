#![allow(dead_code)]

use petgraph::algo;
use petgraph::graphmap;
use std::{fs, io};

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> io::Result<[Option<usize>; 2]> {
    let raw_data = fs::read_to_string("src/data/orbit.txt")?;
    let data = raw_data
        .split('\n')
        .filter(|e| !e.is_empty())
        .map(|s| {
            let v = s.split(')').collect::<Vec<_>>();
            (v[0], v[1])
        })
        .collect::<Vec<_>>();

    match part {
        Part::One => {
            let one = Some(part_one(&data));
            Ok([one, None])
        }
        Part::Two => {
            let two = Some(part_two(&data));
            Ok([None, two])
        }
        _ => {
            let one = Some(part_one(&data));
            let two = Some(part_two(&data));
            Ok([one, two])
        }
    }
}

fn part_one(input: &Vec<(&str, &str)>) -> usize {
    let g = graphmap::UnGraphMap::<_, ()>::from_edges(input.iter());
    let node_map = algo::dijkstra(&g, "COM", None, |_| 1);
    node_map.iter().fold(0, |acc, e| acc + e.1)
}

fn part_two(input: &Vec<(&str, &str)>) -> usize {
    let g = graphmap::UnGraphMap::<_, ()>::from_edges(input.iter());
    let node_map = algo::dijkstra(&g, "YOU", Some("SAN"), |_| 1);

    match node_map.iter().find(|(n, _)| *n == &"SAN") {
        Some((_, d)) => *d,
        _ => unreachable!(),
    }
}

#[cfg(any(feature = "all", feature = "day_06"))]
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

#[cfg(not(any(feature = "all", feature = "day_06")))]
pub fn run(_: Part) {}
