//! mcpp CLI entry point
//!
//! This is the main entry point for the mcpp command-line interface. It delegates
//! to the `mcpp_cli_lib` crate for the actual CLI implementation.

#[tokio::main]
async fn main() {
    match mcpp_cli_lib::cli_match().await {
        Ok(()) => {
            // Successful execution
            std::process::exit(0);
        }
        Err(e) => {
            // Error occurred - print error and exit with semantic code
            eprintln!("ERROR: {}", e);
            std::process::exit(e.exit_code());
        }
    }
}
