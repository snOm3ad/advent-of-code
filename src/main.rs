mod util;
use util::read_file;

fn day_one() -> std::io::Result<usize> {
    let modules = read_file::<usize>("src/data/input.txt", 100)?;
    let fuel_requirements = modules
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

    Ok(fuel_requirements.iter().fold(0, |acc, x| acc + x))
}

fn main() {
    match day_one() {
        Err(err) => {
            eprintln!("[ERROR]: {}", err);
        }
        Ok(res) => {
            println!("{}", res);
        }
    }
}
