#[cfg(feature = "serde")]
use std::io::{self, BufRead, BufReader};

#[cfg(feature = "serde")]
fn main() {
    for line in BufReader::new(io::stdin()).lines() {
        let input = line.unwrap();
        let (_, output) = query_parser::parse(&input).unwrap();
        let json = serde_json::to_string_pretty(&output).unwrap();
        println!("{}", json);
    }
}

#[cfg(not(feature = "serde"))]
fn main() {
    eprint!("This example requires the `serde` feature")
}
