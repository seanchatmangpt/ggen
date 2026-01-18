#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! ggen-core = { path = "../crates/ggen-core" }
//! ggen-utils = { path = "../crates/ggen-utils" }
//! tokio = { version = "1", features = ["full"] }
//! ```

//! Workaround for ggen CLI verb discovery issue
//!
//! The ggen sync command exists but isn't being discovered at runtime.
//! This script calls SyncExecutor directly to generate bindings.

use ggen_core::codegen::{SyncExecutor, SyncOptions};
use std::path::PathBuf;

fn main() -> ggen_utils::error::Result<()> {
    // Build options for dogfooding generation
    let mut options = SyncOptions::new();
    options.manifest_path = PathBuf::from("ggen.toml");
    options.verbose = true;

    println!("Executing ggen sync via SyncExecutor...");
    println!("Manifest: {:?}", options.manifest_path);

    // Execute sync
    let result = SyncExecutor::new(options).execute()?;

    // Print results
    println!("\n✅ Sync completed successfully!");
    println!("Status: {}", result.status);
    println!("Files synced: {}", result.files_synced);
    println!("Duration: {}ms", result.duration_ms);
    println!("Inference rules: {}", result.inference_rules_executed);
    println!("Generation rules: {}", result.generation_rules_executed);

    println!("\nGenerated files:");
    for file in result.files {
        println!("  {} - {} ({} bytes)", file.action, file.path, file.size_bytes);
    }

    Ok(())
}
