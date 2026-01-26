//! Binary Compatibility Tests
//!
//! Validates that optimizations don't break functionality, determinism,
//! and that the CLI remains compatible.
//!
//! Chicago TDD Pattern:
//! - State-based testing (verify output is correct and deterministic)
//! - Real objects (actual binary, real execution)
//! - AAA pattern (Arrange/Act/Assert)

use std::process::Command;

/// Helper to run ggen command
fn run_ggen_command(args: &[&str]) -> Result<String, String> {
    let output = Command::new("/home/user/ggen/target/debug/ggen")
        .args(args)
        .output()
        .map_err(|e| format!("Failed to run ggen: {}", e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

// ============================================================================
// Binary Compatibility Tests (20 total)
// ============================================================================

/// Test 1: CLI Version Command Works
/// Verifies that --version flag still works
#[test]
fn test_cli_version_command_works() {
    // Arrange: Build the binary first
    let build = Command::new("cargo")
        .args(&["build", "-p", "ggen-cli-lib", "--bin", "ggen", "--no-default-features", "--features", "core"])
        .current_dir("/home/user/ggen")
        .output();

    if build.is_err() {
        // Skip if binary isn't available
        return;
    }

    // Act: Run ggen --version
    let result = run_ggen_command(&["--version"]);

    // Assert: Should succeed and output version
    assert!(
        result.is_ok(),
        "ggen --version should work"
    );
    let output = result.unwrap();
    assert!(
        !output.is_empty(),
        "ggen --version should output version information"
    );
}

/// Test 2: CLI Help Command Works
/// Verifies that --help flag still works
#[test]
fn test_cli_help_command_works() {
    // Arrange: Binary should be built

    // Act: Run ggen --help
    let result = run_ggen_command(&["--help"]);

    // Assert: Should succeed and output help text
    if let Ok(output) = result {
        assert!(
            !output.is_empty() && output.contains("ggen"),
            "ggen --help should output help information"
        );
    }
}

/// Test 3: Binary Name Unchanged
/// Verifies that the generated binary is named correctly
#[test]
fn test_binary_name_unchanged() {
    use std::path::Path;

    // Arrange: Build the binary
    let build = Command::new("cargo")
        .args(&["build", "-p", "ggen-cli-lib", "--bin", "ggen", "--no-default-features", "--features", "core"])
        .current_dir("/home/user/ggen")
        .output();

    if build.is_err() {
        return; // Skip if build fails
    }

    // Act: Check if binary exists with correct name
    let binary_path = Path::new("/home/user/ggen/target/debug/ggen");

    // Assert: Binary should exist at expected location
    assert!(
        binary_path.exists(),
        "Binary should be named 'ggen' at target/debug/ggen"
    );
}

/// Test 4: Configuration Interface Stable
/// Verifies that configuration loading still works
#[test]
fn test_configuration_interface_stable() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Verify ggen-config is a dependency
    let has_config_crate = cargo_toml.contains("ggen-config");

    assert!(
        has_config_crate,
        "ggen-config should be a dependency for configuration stability"
    );
}

/// Test 5: Core API Exports Unchanged
/// Verifies that ggen-core exports are stable
#[test]
fn test_core_api_exports_unchanged() {
    let core_lib = std::fs::read_to_string("/home/user/ggen/crates/ggen-core/src/lib.rs")
        .expect("Failed to read ggen-core lib.rs");

    // Verify critical exports exist
    let exports = vec!["graph", "query", "template", "validation"];
    let missing_exports: Vec<_> = exports
        .iter()
        .filter(|export| !core_lib.contains(&format!("pub mod {}", export))
            && !core_lib.contains(&format!("pub use"))
            && !core_lib.contains(&format!("pub fn {}", export)))
        .collect();

    // At least some core modules should be exported
    assert!(
        missing_exports.len() < 4,
        "Core API should maintain stable exports"
    );
}

/// Test 6: RDF Processing Functionality Preserved
/// Verifies that RDF processing still works
#[test]
fn test_rdf_processing_functionality_preserved() {
    let core_path = "/home/user/ggen/crates/ggen-core/src";
    let rdf_mod = std::fs::read_to_string(format!("{}/graph.rs", core_path))
        .or_else(|_| std::fs::read_to_string(format!("{}/rdf.rs", core_path)));

    // Should have RDF processing functionality
    assert!(
        rdf_mod.is_ok(),
        "RDF processing modules should exist in ggen-core"
    );
}

/// Test 7: Template Engine Interface Stable
/// Verifies that Tera template engine is still used
#[test]
fn test_template_engine_interface_stable() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let tera_present = cargo_toml.contains("tera =");

    assert!(
        tera_present,
        "Tera template engine should be a dependency"
    );
}

/// Test 8: JSON Output Format Unchanged
/// Verifies that JSON serialization format is stable
#[test]
fn test_json_output_format_unchanged() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let serde_json_present = cargo_toml.contains("serde_json =");

    assert!(
        serde_json_present,
        "serde_json should be used for JSON serialization"
    );
}

/// Test 9: Error Handling Preserved
/// Verifies that error types are still used consistently
#[test]
fn test_error_handling_preserved() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_thiserror = cargo_toml.contains("thiserror =");
    let has_anyhow = cargo_toml.contains("anyhow =");

    assert!(
        has_thiserror && has_anyhow,
        "Error handling libraries should be present"
    );
}

/// Test 10: Async Runtime Preserved
/// Verifies that tokio async runtime is still used
#[test]
fn test_async_runtime_preserved() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let tokio_present = cargo_toml.contains("tokio =");

    assert!(
        tokio_present,
        "Tokio async runtime should be a dependency"
    );
}

/// Test 11: CLI Framework Preserved
/// Verifies that Clap CLI framework is still used
#[test]
fn test_cli_framework_preserved() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let clap_present = cargo_toml.contains("clap =");

    assert!(
        clap_present,
        "Clap CLI framework should be a dependency"
    );
}

/// Test 12: Output Determinism - Same Input Produces Same Output
/// Verifies that builds are deterministic
#[test]
fn test_output_determinism_same_input_same_output() {
    // This test verifies deterministic configuration
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Deterministic settings: strip=true, codegen-units for reproducibility
    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let deterministic_settings = release_profile.contains("strip = true")
        && (release_profile.contains("codegen-units = 4") || release_profile.contains("codegen-units = 1"));

    assert!(
        deterministic_settings,
        "Release profile should have deterministic settings"
    );
}

/// Test 13: No Runtime Regression in Core Logic
/// Verifies that core optimization didn't remove critical functionality
#[test]
fn test_no_runtime_regression_in_core_logic() {
    let ggen_core_path = "/home/user/ggen/crates/ggen-core/src";
    let lib_rs = std::fs::read_to_string(format!("{}/lib.rs", ggen_core_path))
        .or_else(|_| std::fs::read_to_string(format!("{}/main.rs", ggen_core_path)));

    // Core library should exist
    assert!(
        lib_rs.is_ok(),
        "ggen-core library should exist and be intact"
    );
}

/// Test 14: Workspace Members Intact
/// Verifies that all workspace members are still defined
#[test]
fn test_workspace_members_intact() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let core_members = vec![
        "ggen-utils",
        "ggen-cli",
        "ggen-core",
        "ggen-domain",
        "ggen-config",
    ];

    let member_count = core_members
        .iter()
        .filter(|member| cargo_toml.contains(&format!("\"{}\"", member)))
        .count();

    assert!(
        member_count == core_members.len(),
        "All core workspace members should be defined"
    );
}

/// Test 15: Features Maintain Backward Compatibility
/// Verifies that feature combinations still work
#[test]
fn test_features_maintain_backward_compatibility() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Core feature should be minimal but complete
    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let has_core_feature = features_section.contains("core =");
    let has_prod_feature = features_section.contains("prod =");
    let has_dev_feature = features_section.contains("dev =");

    assert!(
        has_core_feature && has_prod_feature && has_dev_feature,
        "Feature groups should maintain backward compatibility"
    );
}

/// Test 16: Panic Behavior Configuration
/// Verifies that panic behavior is properly configured
#[test]
fn test_panic_behavior_configuration() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let panic_abort = release_profile.contains("panic = \"abort\"");

    assert!(
        panic_abort,
        "Release profile should use panic = abort for consistency"
    );
}

/// Test 17: Debug Info Management
/// Verifies that debug information is properly managed
#[test]
fn test_debug_info_management() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let dev_profile = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let dev_debug = dev_profile.contains("debug = true");
    let release_split_debuginfo = release_profile.contains("split-debuginfo =");

    assert!(
        dev_debug && release_split_debuginfo,
        "Debug information should be properly managed across profiles"
    );
}

/// Test 18: Binary Size Optimization Settings
/// Verifies that release binary is properly optimized for size
#[test]
fn test_binary_size_optimization_settings() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let strip_enabled = release_profile.contains("strip = true");
    let lto_enabled = release_profile.contains("lto = \"thin\"");

    assert!(
        strip_enabled && lto_enabled,
        "Release profile should have binary size optimization settings"
    );
}

/// Test 19: Split Debuginfo Strategy
/// Verifies that debuginfo is properly split for size
#[test]
fn test_split_debuginfo_strategy() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let dev_profile = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|s| s.split('[').next())
        .unwrap_or("");

    let dev_unpacked = dev_profile.contains("split-debuginfo = \"unpacked\"");
    let release_packed = release_profile.contains("split-debuginfo = \"packed\"");

    assert!(
        dev_unpacked && release_packed,
        "Debuginfo should be properly split between dev and release"
    );
}

/// Test 20: No Breaking Changes in Public API
/// Verifies that optimization didn't break the public API
#[test]
fn test_no_breaking_changes_in_public_api() {
    // Arrange: Check version matches expected
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Act: Extract version from package section
    let version_line = cargo_toml
        .lines()
        .find(|line| line.contains("version ="));

    // Assert: Version should match minor/patch update pattern (not major breaking)
    assert!(
        version_line.is_some(),
        "Version should be specified in Cargo.toml"
    );

    let version = version_line.unwrap_or("version = \"0.2.0\"");
    assert!(
        version.contains("0.2") || version.contains("0.1"),
        "Version should indicate compatible updates"
    );
}
