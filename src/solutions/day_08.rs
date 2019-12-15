#![allow(dead_code)]

use std::{fmt, fs, io};

pub enum Part {
    One,
    Two,
    All,
}

enum Results<T, V>
where
    T: fmt::Display,
    V: fmt::Display,
{
    One(T),
    Two(V),
    Empty,
}

fn main(part: Part) -> io::Result<[Results<usize, String>; 2]> {
    let raw_data = fs::read_to_string("src/data/image.txt")?;
    let data = raw_data
        .chars()
        .filter_map(|s| s.to_digit(10))
        .map(|d| d as usize)
        .collect::<Vec<_>>();

    let dims = (25, 6);

    match part {
        Part::One => {
            let one = Results::One(part_one(&data, &dims)?);
            Ok([one, Results::Empty])
        }
        Part::Two => {
            let two = Results::Two(part_two(&data, &dims)?);
            Ok([Results::Empty, two])
        }
        Part::All => {
            let one = Results::One(part_one(&data, &dims)?);
            let two = Results::Two(part_two(&data, &dims)?);
            Ok([one, two])
        }
    }
}

struct Layer<'a> {
    dims: (usize, usize),
    data: &'a [usize],
}

impl<'a> Layer<'a> {
    fn new(data: &'a [usize], dims: (usize, usize)) -> Self {
        Self { data, dims }
    }

    fn hits(&self, target: usize) -> usize {
        self.data.iter().filter(|&e| *e == target).count()
    }

    fn get_pixel_value(&self, row: usize, col: usize) -> usize {
        let idx = row + col * self.dims.0;
        self.data[idx]
    }
}

// homogeneous zip 😉
type HomoZip<T> = std::iter::Zip<T, T>;
fn get_paired_ranges(bytes: usize, size: usize) -> HomoZip<impl Iterator<Item = usize>> {
    // range containing start points
    let start = (0..bytes).step_by(size);
    // range containing end points
    let end = ((size - 1)..bytes).step_by(size);
    start.zip(end)
}

fn layer_up<'a>(data: &'a Vec<usize>, dims: &(usize, usize)) -> io::Result<Vec<Layer<'a>>> {
    let size = dims.0 * dims.1;
    let bytes = data.len();
    // short-circuiting, avoids division by 0.
    if size == 0 || bytes < size || bytes % size != 0 {
        return Err(io::Error::from(io::ErrorKind::InvalidInput));
    }

    let mut layers = Vec::<Layer>::with_capacity(bytes / size);
    // first element contains the start of the layer, second the end.
    let layer_ranges = get_paired_ranges(bytes, size);
    for (i, j) in layer_ranges {
        layers.push(Layer::new(&data[i..=j], *dims));
    }

    Ok(layers)
}

fn part_one(data: &Vec<usize>, dims: &(usize, usize)) -> io::Result<usize> {
    // get the layers for the image.
    let layers = layer_up(data, dims)?;
    // find the layer with layer with the min number of zeroes.
    match layers.iter().min_by_key(|e| e.hits(0)) {
        Some(layer) => Ok(layer.hits(1) * layer.hits(2)),
        _ => unreachable!(),
    }
}

fn part_two(data: &Vec<usize>, dims: &(usize, usize)) -> io::Result<String> {
    // get the layers for the image.
    let layers = layer_up(data, dims)?;
    let mut image = String::with_capacity(2 * dims.0 * dims.1 + dims.1);
    // for every (row, col) i.e. pixel go through the layers in order to determine
    // the color of it.
    for cpos in 0..dims.1 {
        for rpos in 0..dims.0 {
            for layer in layers.iter() {
                match layer.get_pixel_value(rpos, cpos) {
                    0 => {
                        image.push_str(". ");
                        break;
                    }
                    1 => {
                        image.push_str("# ");
                        break;
                    }
                    2 => continue,
                    _ => unreachable!(),
                }
            }
        }
        image.push('\n');
    }

    Ok(image)
}

#[cfg(any(feature = "all", feature = "day_08"))]
pub fn run(part: Part) {
    match main(part) {
        Ok(results) => {
            for (i, result) in results.iter().enumerate() {
                match result {
                    Results::One(r) => println!("result-part({}): {}", i + 1, r),
                    Results::Two(r) => println!("result-part({}):\n{}", i + 1, r),
                    Results::Empty => {}
                }
            }
        }
        Err(errors) => eprintln!("[ERROR]: {}", errors),
    }
}

#[cfg(not(any(feature = "all", feature = "day_08")))]
pub fn run(_: Part) {}
