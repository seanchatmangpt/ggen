//! GENERATED binary entry point. Do not edit.

use std::process::ExitCode;

fn main() -> ExitCode {
    match custom_handler_demo::run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("ERROR: {error}");
            ExitCode::FAILURE
        }
    }
}
