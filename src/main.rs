//! ggen CLI entry point in root package
//!
//! This is the main entry point for the ggen command-line interface at the root level.
//! It delegates to the `ggen_cli_lib` crate for the actual CLI implementation.

#[tokio::main]
async fn main() {
    match ggen_cli_lib::cli_match().await {
        Ok(()) => {
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("ERROR: {}", e);
            std::process::exit(1);
        }
    }
}
