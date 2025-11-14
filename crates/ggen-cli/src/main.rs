//! ggen CLI entry point
//!
//! This is the main entry point for the ggen command-line interface. It delegates
//! to the `ggen_cli_lib` crate for the actual CLI implementation.

use ggen_cli_lib;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    ggen_cli_lib::cli_match()
        .await
        .map_err(|e| anyhow::anyhow!("CLI error: {}", e))
}
