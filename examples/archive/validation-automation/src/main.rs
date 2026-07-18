#!/usr/bin/env cargo
//! Validation Automation CLI
//!
//! Developer Experience & Quality of Life automation for validation approaches.
//!
//! Usage:
//!   cargo run -- --input "SELECT * FROM {graph}" --intensity 100 --approach all --report all

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    let runtime = tokio::runtime::Runtime::new()?;
    runtime.block_on(validation_automation::cli::run()).map_err(|e| e.into())
}
