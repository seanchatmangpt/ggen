//! GENERATED binary entry point. Do not edit.

use std::process::ExitCode;

fn main() -> ExitCode {
    match inventoryctl::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("ERROR: {error}");
            ExitCode::FAILURE
        }
    }
}
