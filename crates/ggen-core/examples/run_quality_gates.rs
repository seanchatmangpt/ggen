//! Direct quality gates runner - bypass MCP entirely
//!
//! Usage: cargo run --example run_quality_gates --manifest-path crates/ggen-core/Cargo.toml

use std::path::PathBuf;

fn main() -> anyhow::Result<()> {
    println!("Running Quality Gates on ggen Project");
    println!("======================================\n");

    let project_path = PathBuf::from("/Users/sac/ggen");
    let manifest_path = project_path.join("ggen.toml");

    if !manifest_path.exists() {
        anyhow::bail!("ggen.toml not found at {}", manifest_path.display());
    }

    println!("Project: {}", project_path.display());
    println!("Manifest: {}\n", manifest_path.display());

    // Parse manifest
    println!("Step 1: Parsing manifest...");
    let manifest = ggen_core::manifest::ManifestParser::parse(&manifest_path)?;
    println!("✓ Manifest parsed successfully\n");

    // Run quality gates
    println!("Step 2: Running quality gates...\n");
    let runner = ggen_core::poka_yoke::quality_gates::QualityGateRunner::new();
    let checkpoints = runner.checkpoints();

    println!("Quality Gates ({} total):\n", checkpoints.len());
    for (i, checkpoint) in checkpoints.iter().enumerate() {
        println!("  {}. {}", i + 1, checkpoint.name);
    }
    println!();

    // Run the gates
    match runner.run_all(&manifest, &project_path) {
        Ok(_) => {
            println!("✅ All quality gates PASSED!\n");
            println!("Passed checks:");
            for checkpoint in checkpoints.iter() {
                println!("  ✓ {}", checkpoint.name);
            }
        }
        Err(e) => {
            println!("❌ Quality gate FAILED:\n");
            println!("Error: {}\n", e);
            println!("This is expected for the main ggen project which may have");
            println!("intentional violations for testing purposes.");
        }
    }

    Ok(())
}
