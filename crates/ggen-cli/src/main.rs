//! ggen CLI entry point
//!
//! This is the main entry point for the ggen command-line interface. It delegates
//! to the `ggen_cli` library for the actual CLI implementation.

use ggen_cli;

#[tokio::main]
async fn main() {
    match ggen_cli::cli_match().await {
        Ok(()) => {
            // Successful execution
            std::process::exit(0);
        }
        Err(e) => {
            // Error occurred - print error and exit with code 1
            eprintln!("ERROR: {}", e);
            std::process::exit(1);
        }
    }
}
