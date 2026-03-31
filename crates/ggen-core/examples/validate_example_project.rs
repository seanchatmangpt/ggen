//! Validate an example ggen project using all quality gates

use ggen_core::manifest::GgenManifest;
use ggen_core::poka_yoke::quality_gates::QualityGateRunner;
use std::path::PathBuf;

fn main() {
    println!("=== Running validate_pipeline on example project ===\n");

    // Load an example project's manifest
    let manifest_path = PathBuf::from("examples/basic-template-generation/ggen.toml");
    let base_path = PathBuf::from("examples/basic-template-generation");

    println!("Loading manifest from: {:?}", manifest_path);
    let manifest_content =
        std::fs::read_to_string(&manifest_path).expect("Failed to read ggen.toml");

    let manifest: GgenManifest =
        toml::from_str(&manifest_content).expect("Failed to parse ggen.toml");

    println!("Manifest loaded successfully\n");

    // Run quality gates
    let runner = QualityGateRunner::new();
    let checkpoints = runner.checkpoints();

    println!("Found {} quality gates:\n", checkpoints.len());
    for (i, checkpoint) in checkpoints.iter().enumerate() {
        println!("  {}. {}", i + 1, checkpoint.name);
    }
    println!();

    // Run all gates
    println!("Running quality gates...\n");
    let result = runner.run_all(&manifest, &base_path);

    match result {
        Ok(()) => {
            println!("\n✅ ALL QUALITY GATES PASSED\n");
            println!("Quality Gate Results:");
            println!("=====================");
            for checkpoint in checkpoints {
                println!("  ✅ {} - PASSED", checkpoint.name);
            }
        }
        Err(e) => {
            println!("\n❌ QUALITY GATE FAILED\n");
            println!("Error: {}\n", e);
            println!("Quality Gate Results:");
            println!("=====================");
            for checkpoint in checkpoints {
                let status = if e.to_string().contains(&checkpoint.name) {
                    "❌ FAILED"
                } else {
                    "✅ PASSED"
                };
                println!(
                    "  {} {} - {}",
                    status.chars().next().unwrap(),
                    checkpoint.name,
                    status
                );
            }
        }
    }
}
