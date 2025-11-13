extern crate cranelift_isle;

use cranelift_isle::compile;
use cranelift_isle::error::Errors;
use std::{
    str::FromStr,
    io,
    io::Write,
    path::PathBuf,
};

fn main() -> Result<(), Errors> {
    let inputs: Vec<PathBuf> =
        std::env::args().skip(1).map(|s| PathBuf::from_str(&s).unwrap()).collect();

    let code = compile::from_files(inputs, &Default::default())?;
    io::stdout().write_all(code.as_bytes()).unwrap();

    Ok(())
}
