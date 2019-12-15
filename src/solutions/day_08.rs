#![allow(dead_code)]

use std::{fs, io};

pub enum Part {
    One,
    Two,
    All,
}

fn main(part: Part) -> io::Result<[Option<usize>; 2]> {
    let raw_data = fs::read_to_string("src/data/image.txt")?;
    let data = raw_data
        .chars()
        .filter_map(|s| s.to_digit(10))
        .map(|d| d as usize)
        .collect::<Vec<_>>();

    let dims = (25, 6);

    match part {
        Part::One => {
            let one = Some(part_one(&data, &dims)?);
            Ok([one, None])
        }
        Part::Two => {
            part_two(&data, &dims)?;
            Ok([None, Some(0)])
        }
        Part::All => {
            let one = Some(part_one(&data, &dims)?);
            part_two(&data, &dims)?;
            Ok([one, Some(0)])
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

fn part_two(data: &Vec<usize>, dims: &(usize, usize)) -> io::Result<()> {
    // get the layers for the image.
    let layers = layer_up(data, dims)?;
    // for every (row, col) i.e. pixel go through the layers in order to determine
    // the color of it.
    for cpos in 0..dims.1 {
        for rpos in 0..dims.0 {
            for layer in layers.iter() {
                match layer.get_pixel_value(rpos, cpos) {
                    0 => {
                        print!(". ");
                        break;
                    }
                    1 => {
                        print!("# ");
                        break;
                    }
                    2 => continue,
                    _ => unreachable!(),
                }
            }
        }
        println!();
    }

    Ok(())
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
