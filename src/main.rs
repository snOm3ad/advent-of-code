mod solutions;
mod util;

use solutions::day_one;
use solutions::day_three;
use solutions::day_two;

fn main() {
    day_one::run(day_one::Part::All);
    day_two::run(day_two::Part::All);
    day_three::run(day_three::Part::One);
}
