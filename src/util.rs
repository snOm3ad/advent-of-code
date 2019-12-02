use std::error::Error;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str::FromStr;

pub fn read_file<T: FromStr>(path: &str, hint: usize) -> std::io::Result<Vec<T>>
where
    <T as FromStr>::Err: 'static + Error + Send + Sync,
{
    let input = File::open(path)?;
    let br = io::BufReader::new(input);
    let mut v = Vec::with_capacity(hint);
    for line in br.lines() {
        let line = line?;
        let content = line
            .trim()
            .parse::<T>()
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        v.push(content);
    }
    Ok(v)
}
