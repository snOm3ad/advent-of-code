#![allow(dead_code)]

use std::collections::HashMap;
use std::fs;
use std::io;

pub enum Part {
    One,
    Two,
    All,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
    Invalid,
}

#[derive(PartialEq)]
enum Orientation {
    Colinear,
    Clockwise,
    CounterClockwise,
}

impl Default for Direction {
    fn default() -> Self {
        Direction::Invalid
    }
}

#[derive(Clone, Copy, Default, Debug)]
struct Line {
    steps: isize,
    start: (isize, isize),
    direction: Direction,
}

impl Line {
    fn new(steps: isize, direction: Direction) -> Self {
        Line {
            steps,
            direction,
            start: (0, 0),
        }
    }
}

struct Point(isize, isize);

impl Point {
    fn new(p: (isize, isize)) -> Self {
        Point { 0: p.0, 1: p.1 }
    }
}

type Ray = Vec<(isize, Direction)>;
struct Board(Ray, Ray);

fn parse_wire_layout(wire_layout: &str) -> Ray {
    // e.g. path = ["R", "200", "L", "10", ...]
    let path = wire_layout.split(',').collect::<Vec<_>>();
    // now partition the path into directions and steps
    let (steps, directions): (Vec<&str>, Vec<&str>) =
        path.iter().partition(|s| s.parse::<isize>().is_ok());
    // now actually parse the steps.
    let steps = steps
        .iter()
        .map(|s| s.parse::<isize>().unwrap())
        .collect::<Vec<_>>();
    // parse the directions as well
    let directions = directions
        .iter()
        .map(|s| match s.as_ref() {
            "R" => Direction::Right,
            "L" => Direction::Left,
            "D" => Direction::Down,
            "U" => Direction::Up,
            _ => unreachable!(),
        })
        .filter(|&d| d != Direction::Invalid)
        .collect::<Vec<_>>();

    steps
        .into_iter()
        .zip(directions.into_iter())
        .collect::<Vec<_>>()
}

fn parse_input(input: &str) -> io::Result<Board> {
    let board = input
        .split('\n')
        .filter(|layout| !layout.is_empty())
        .collect::<Vec<_>>();
    // must provide exactly two wires..
    if board.len() != 2 {
        let error = io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "expected board with exactly two wires, received {}",
                board.len()
            ),
        );
        return Err(error);
    }

    let wire_one = parse_wire_layout(board.first().unwrap());
    let wire_two = parse_wire_layout(board.last().unwrap());

    Ok(Board(wire_one, wire_two))
}

fn main(part: Part) -> io::Result<[Option<isize>; 2]> {
    let input = fs::read_to_string("src/data/distance.txt")?;
    let board = parse_input(&input)?;

    match part {
        Part::One => {
            let one = Some(part_one(&board));
            Ok([one, None])
        }
        Part::Two => {
            let two = Some(part_two(&board));
            Ok([None, two])
        }
        _ => {
            let one = Some(part_one(&board));
            let two = Some(part_two(&board));
            Ok([one, two])
        }
    }
}

fn trace_line(line: &Line) -> Vec<(isize, isize)> {
    let (x, y) = line.start;
    match line.direction {
        Direction::Right => (x + 1..=x + line.steps).map(|x| (x, y)).collect(),
        Direction::Left => (x - line.steps..x).rev().map(|x| (x, y)).collect(),
        Direction::Up => (y + 1..=y + line.steps).map(|y| (x, y)).collect(),
        Direction::Down => (y - line.steps..y).rev().map(|y| (x, y)).collect(),
        _ => unreachable!(),
    }
}

fn orientation(s: &(Point, Point), r: &Point) -> Orientation {
    // extract the values
    let (p, q) = s;
    // calculate orientation
    let value = (q.1 - p.1) * (r.0 - q.0) - (q.0 - p.0) * (r.1 - q.1);
    match value {
        0 => Orientation::Colinear,
        v if v > 0 => Orientation::Clockwise,
        v if v < 0 => Orientation::CounterClockwise,
        _ => unreachable!(),
    }
}

fn convert_to_segment(line: &Line) -> (Point, Point) {
    let p = Point::new(line.start);
    // now compute the end-points associated with these points
    let q = match line.direction {
        Direction::Down => Point(line.start.0, line.start.1 - line.steps),
        Direction::Up => Point(line.start.0, line.start.1 + line.steps),
        Direction::Right => Point(line.start.0 + line.steps, line.start.1),
        Direction::Left => Point(line.start.0 - line.steps, line.start.1),
        _ => unreachable!(),
    };

    (p, q)
}

fn detect_intersection(line_a: &Line, line_b: &Line) -> bool {
    let seg_a = convert_to_segment(line_a);
    let seg_b = convert_to_segment(line_b);

    let o1 = orientation(&seg_a, &seg_b.0);
    let o2 = orientation(&seg_a, &seg_b.1);
    let o3 = orientation(&seg_b, &seg_a.0);
    let o4 = orientation(&seg_b, &seg_a.1);

    o1 != o2 && o3 != o4
}

type TupVec<T> = Vec<(T, T)>;

fn find_intersection_point(
    traced_line_a: &TupVec<isize>,
    traced_line_b: &TupVec<isize>,
) -> Option<(isize, isize)> {
    for a in traced_line_a {
        for b in traced_line_b {
            if a == b {
                return Some(*a);
            }
        }
    }
    None
}

fn construct_wire_path(w: &Ray) -> Vec<Line> {
    // transform the path into lines
    let path = w
        .iter()
        .map(|&(steps, direction)| Line::new(steps, direction))
        .collect::<Vec<_>>();

    path.iter()
        .enumerate()
        .scan(Line::default(), |state, (idx, &line)| {
            *state = {
                if idx == 0 {
                    line
                } else {
                    match state.direction {
                        Direction::Down => Line {
                            start: (state.start.0, state.start.1 - state.steps),
                            ..line
                        },
                        Direction::Up => Line {
                            start: (state.start.0, state.start.1 + state.steps),
                            ..line
                        },
                        Direction::Right => Line {
                            start: (state.start.0 + state.steps, state.start.1),
                            ..line
                        },
                        Direction::Left => Line {
                            start: (state.start.0 - state.steps, state.start.1),
                            ..line
                        },
                        _ => unreachable!(),
                    }
                }
            };
            Some(*state)
        })
        .collect::<Vec<_>>()
}

fn get_board_layout(board: &Board) -> (Vec<Line>, Vec<Line>) {
    // Construct a vector of lines for each wire
    let untraced_path_a = construct_wire_path(&board.0);
    let untraced_path_b = construct_wire_path(&board.1);
    (untraced_path_a, untraced_path_b)
}

fn find_intersecting_lines(upath_a: &Vec<Line>, upath_b: &Vec<Line>) -> Vec<(usize, usize)> {
    let mut matches: Vec<(usize, usize)> = Vec::new();

    // check for every line in both paths if there exists an intersection.
    for (a, line_a) in upath_a.iter().enumerate() {
        for (b, line_b) in upath_b.iter().enumerate() {
            if detect_intersection(line_a, line_b) {
                if a != 0 || b != 0 {
                    matches.push((a, b));
                }
            }
        }
    }

    matches
}

fn get_intersection_points(
    path_a: &Vec<Line>,
    path_b: &Vec<Line>,
    matches: &TupVec<usize>,
) -> TupVec<isize> {
    // the board layout is the path that each wire follows. NOTE: the path comes in a compact form,
    // as we only need to trace a subset of the lines in the path of a given wire.
    let mut intersections: TupVec<isize> = Vec::with_capacity(matches.len());
    // cache the lines that we have traced, the key is this complicated because we need to know
    // from which wire the index comes from! otherwise you may override the cached result of one
    // wire with the other (no bueno).
    let mut cached_traces: HashMap<(usize, usize), TupVec<isize>> = HashMap::new();

    for (a, b) in matches {
        let (a, b) = (*a, *b);
        // trace the line only if its not cached.
        if cached_traces.get(&(0, a)).is_none() {
            let trace = trace_line(&path_a[a]);
            cached_traces.insert((0, a), trace);
        }

        if cached_traces.get(&(1, b)).is_none() {
            let trace = trace_line(&path_b[b]);
            cached_traces.insert((1, b), trace);
        }

        let traced_line_a = cached_traces.get(&(0, a)).unwrap();
        let traced_line_b = cached_traces.get(&(1, b)).unwrap();

        match find_intersection_point(traced_line_a, traced_line_b) {
            Some(point) => intersections.push(point),
            _ => continue,
        }
    }

    intersections
}

fn part_one(board: &Board) -> isize {
    let (upath_a, upath_b) = get_board_layout(board);
    let matches = find_intersecting_lines(&upath_a, &upath_b);
    let intersections = get_intersection_points(&upath_a, &upath_b, &matches);

    intersections
        .iter()
        .fold(isize::max_value(), |curr_min, p| {
            std::cmp::min(curr_min, isize::abs(p.0) + isize::abs(p.1))
        })
}

fn get_steps_to_intersection(upath: &[Line], p: &(isize, isize)) -> isize {
    // p: is the intersection point
    let prevs = upath.len() - 1;
    assert!(prevs > 0);
    // accumulate the number of steps from the start to the (n - 1) line
    // i.e. the line just before the intersection happens.
    let mut sum = upath[..prevs].iter().fold(0, |acc, line| acc + line.steps);
    // and the starting point of the intersecting line
    let (x, y) = upath[prevs].start;

    // add the number of steps required to reach the intersection point from
    // the start of the intersecting line, for both wires.
    sum += match &upath[prevs].direction {
        Direction::Left | Direction::Right => isize::abs(p.0 - x),
        Direction::Down | Direction::Up => isize::abs(p.1 - y),
        _ => unreachable!(),
    };

    sum
}

fn part_two(board: &Board) -> isize {
    let (upath_a, upath_b) = get_board_layout(board);
    let matches = find_intersecting_lines(&upath_a, &upath_b);
    // we need the intersection for the coupe de grace, otherwise we overshoot the result by moving
    // to the endpoint of the line in which the intersection occurs (regardless of the wire).
    let intersections = get_intersection_points(&upath_a, &upath_b, &matches);
    assert_eq!(matches.len(), intersections.len());
    // now we want to match
    let distances = matches
        .iter()
        .enumerate()
        .map(|(i, hit)| {
            let sum_a = get_steps_to_intersection(&upath_a[..=hit.0], &intersections[i]);
            let sum_b = get_steps_to_intersection(&upath_b[..=hit.1], &intersections[i]);
            sum_a + sum_b
        })
        .collect::<Vec<_>>();

    distances.into_iter().min().unwrap()
}

#[cfg(any(feature = "all", feature = "day_03"))]
pub fn run(part: Part) {
    match main(part) {
        Ok(results) => {
            for (i, result) in results.iter().enumerate() {
                if let Some(r) = result {
                    println!("result-part({}): {}", i + 1, r);
                }
            }
        }
        Err(errors) => println!("[ERROR]: {}", errors),
    }
}

#[cfg(not(any(feature = "all", feature = "day_03")))]
pub fn run(_: Part) {}

#[cfg(test)]
mod test {
    use super::*;

    mod part_one {
        use super::*;
        #[test]
        fn easy() {
            let input = "R,8,U,5,L,5,D,3\nU,7,R,6,D,4,L,4";
            let board = parse_input(input).unwrap();
            assert_eq!(6, part_one(&board));
        }

        #[test]
        fn hard_a() {
            let input = "R,75,D,30,R,83,U,83,L,12,D,49,R,71,U,7,L,72\nU,62,R,66,U,55,R,34,D,71,R,55,D,58,R,83";
            let board = parse_input(input).unwrap();
            assert_eq!(159, part_one(&board));
        }

        #[test]
        fn hard_b() {
            let input = "R,98,U,47,R,26,D,63,R,33,U,87,L,62,D,20,R,33,U,53,R,51\nU,98,R,91,D,20,R,16,D,67,R,40,U,7,R,15,U,6,R,7";
            let board = parse_input(input).unwrap();
            assert_eq!(135, part_one(&board));
        }
    }

    mod part_two {
        use super::*;

        #[test]
        fn easy() {
            let input = "R,8,U,5,L,5,D,3\nU,7,R,6,D,4,L,4";
            let board = parse_input(input).unwrap();
            assert_eq!(30, part_two(&board));
        }

        #[test]
        fn hard_a() {
            let input = "R,75,D,30,R,83,U,83,L,12,D,49,R,71,U,7,L,72\nU,62,R,66,U,55,R,34,D,71,R,55,D,58,R,83";
            let board = parse_input(input).unwrap();
            assert_eq!(610, part_two(&board));
        }

        #[test]
        fn hard_b() {
            let input = "R,98,U,47,R,26,D,63,R,33,U,87,L,62,D,20,R,33,U,53,R,51\nU,98,R,91,D,20,R,16,D,67,R,40,U,7,R,15,U,6,R,7";
            let board = parse_input(input).unwrap();
            assert_eq!(410, part_two(&board));
        }
    }
}
