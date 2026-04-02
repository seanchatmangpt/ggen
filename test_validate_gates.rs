use ggen_core::poka_yoke::quality_gates::QualityGateRunner;
use std::path::PathBuf;

fn main() {
    println!("Testing validate_pipeline quality gates...\n");

    // Test 1: Create quality gate runner
    let runner = QualityGateRunner::new();
    let checkpoints = runner.checkpoints();

    println!("Found {} quality gates:", checkpoints.len());
    for (i, checkpoint) in checkpoints.iter().enumerate() {
        println!("  {}: {}", i + 1, checkpoint.name);
    }

    println!("\nExpected gates:");
    println!("  1: Manifest Schema");
    println!("  2: Ontology Dependencies");
    println!("  3: SPARQL Validation");
    println!("  4: Template Validation");
    println!("  5: File Permissions");
    println!("  6: Rule Validation");

    // Test 2: Verify example manifest exists
    let manifest_path = PathBuf::from("/Users/sac/ggen/examples/basic-template-generation/ggen.toml");
    println!("\nTest manifest path: {}", manifest_path.display());
    println!("Manifest exists: {}", manifest_path.exists());

    // Test 3: Run quality gates on example manifest
    if manifest_path.exists() {
        println!("\nRunning quality gates on example manifest...");
        match runner.run_all(&manifest_path) {
            Ok(results) => {
                println!("Quality gate results:");
                for result in &results {
                    println!("  - {}: {}", result.gate_name,
                        if result.passed { "PASS" } else { "FAIL" });
                    if !result.message.is_empty() {
                        println!("    Message: {}", result.message);
                    }
                }
            }
            Err(e) => {
                println!("Error running quality gates: {}", e);
            }
        }
    }
}
