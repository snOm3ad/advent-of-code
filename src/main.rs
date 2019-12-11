mod solutions;
mod util;

use solutions::day_01;
use solutions::day_02;
use solutions::day_03;
use solutions::day_04;
use solutions::day_05;

fn main() {
    day_01::run(day_01::Part::All);
    day_02::run(day_02::Part::All);
    day_03::run(day_03::Part::All);
    day_04::run(day_04::Part::All);
    day_05::run(day_05::Part::One);
}
