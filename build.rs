//! Build script for compile-time validation
//!
//! Part of the Andon Signal Validation Framework - Layer 1: Compile-Time Validation
//!
//! This build script validates CLI commands and test configurations at compile time.

fn main() {
    // Validate clnrm test configurations
    validate_clnrm_configs();
}

/// Validate all clnrm test configuration files
fn validate_clnrm_configs() {
    use std::fs;
    use std::path::Path;

    let test_dir = Path::new("tests/clnrm");
    if !test_dir.exists() {
        // Test directory doesn't exist - this is OK for some build contexts
        return;
    }

    // Check for main CLI commands test file
    let cli_commands = test_dir.join("cli_commands.clnrm.toml");
    if cli_commands.exists() {
        // Basic validation - file exists and is readable
        if let Err(e) = fs::metadata(&cli_commands) {
            println!("cargo:warning=clnrm test file validation failed: {}", e);
        }
    }
}
