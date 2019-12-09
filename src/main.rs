mod solutions;
mod util;

use solutions::day_01;
use solutions::day_02;
use solutions::day_03;

fn main() {
    day_01::run(day_01::Part::All);
    day_02::run(day_02::Part::All);
    day_03::run(day_03::Part::All);
}
