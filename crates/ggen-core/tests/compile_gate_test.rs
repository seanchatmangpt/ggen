//! Integration tests for CompileGate.
//!
//! Chicago TDD: these tests run the REAL `cargo check` against actual crates
//! in the workspace. No mocks, no test doubles.
//!
//! These tests are intentionally integration-level: they invoke the real cargo
//! process and assert on the observable compilation outcome.

use ggen_core::validation::compile_gate::{CompileError, CompileGate, CompileResult, CompileTarget};
use std::path::PathBuf;
use std::time::Duration;

/// Find the workspace root from CARGO_MANIFEST_DIR (compile-time env var).
fn workspace_root() -> PathBuf {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.pop(); // ggen-core
    dir.pop(); // crates
    dir
}

#[test]
fn test_compile_gate_ggen_a2a_mcp_succeeds() {
    // Arrange: create a CompileGate targeting this workspace.
    let gate = CompileGate::new(workspace_root())
        .unwrap()
        .with_timeout(Duration::from_secs(300));

    // Act: run cargo check on ggen-a2a-mcp (a real crate in the workspace).
    let result = gate.validate_crate("ggen-a2a-mcp");

    // Assert: the crate should compile successfully.
    assert!(
        result.passed,
        "Expected ggen-a2a-mcp to compile, but cargo check failed:\n{}",
        result.output
    );
    assert!(result.duration_ms > 0, "Duration should be recorded");
}

#[test]
fn test_compile_gate_ggen_core_succeeds() {
    let gate = CompileGate::new(workspace_root())
        .unwrap()
        .with_timeout(Duration::from_secs(300));
    let result = gate.validate_crate("ggen-core");
    assert!(
        result.passed,
        "Expected ggen-core to compile, but cargo check failed:\n{}",
        result.output
    );
}

#[test]
fn test_compile_gate_nonexistent_crate_fails() {
    let gate = CompileGate::new(workspace_root())
        .unwrap()
        .with_timeout(Duration::from_secs(60));
    let result = gate.validate_crate("nonexistent-crate-xyz-12345");

    assert!(!result.passed, "Nonexistent crate should fail compilation");
    assert!(
        !result.output.is_empty(),
        "Error output should be populated for failed compilation"
    );
}

#[test]
fn test_compile_gate_check_convenience_api() {
    let gate = CompileGate::new(workspace_root())
        .unwrap()
        .with_timeout(Duration::from_secs(300));

    // Ok path
    let ok = gate.check("ggen-core");
    assert!(ok.is_ok(), "ggen-core should compile: {:?}", ok.err());

    // Err path
    let err = gate.check("nonexistent-crate-xyz-12345");
    assert!(err.is_err(), "Nonexistent crate should fail");
    let err_msg = err.unwrap_err();
    assert!(!err_msg.is_empty(), "Error message should not be empty");
}

#[test]
fn test_compile_result_display_on_success() {
    let result = CompileResult {
        target_name: "my-crate".to_string(),
        passed: true,
        duration_ms: 42,
        output: String::new(),
    };
    let text = format!("{}", result);
    assert!(text.contains("[PASS]"));
    assert!(text.contains("my-crate"));
}

#[test]
fn test_compile_result_display_on_failure() {
    let result = CompileResult {
        target_name: "broken-crate".to_string(),
        passed: false,
        duration_ms: 999,
        output: "error[E0425]: cannot find value `x` in this scope".to_string(),
    };
    let text = format!("{}", result);
    assert!(text.contains("[FAIL]"));
    assert!(text.contains("broken-crate"));
    assert!(text.contains("cannot find value"));
}

// ========== New Tests for Enhanced Functionality ==========

#[test]
fn test_compile_gate_new_api_with_package_target() {
    let gate = CompileGate::new(workspace_root()).unwrap();
    let result = gate.validate(&CompileTarget::Package("ggen-core".to_string()));

    assert!(
        result.is_ok(),
        "Validation should succeed: {:?}",
        result
    );
    let compile_result = result.unwrap();
    assert!(
        compile_result.passed,
        "ggen-core should compile: {}",
        compile_result.output
    );
}

#[test]
fn test_compile_gate_manifest_path_succeeds() {
    let gate = CompileGate::new(workspace_root()).unwrap();
    let manifest = workspace_root().join("crates/ggen-a2a-mcp/Cargo.toml");

    let result = gate.validate(&CompileTarget::ManifestPath(manifest));

    assert!(
        result.is_ok(),
        "Validation should succeed: {:?}",
        result
    );
    let compile_result = result.unwrap();
    assert!(
        compile_result.passed,
        "ggen-a2a-mcp should compile via manifest path: {}",
        compile_result.output
    );
}

#[test]
fn test_compile_gate_manifest_not_found() {
    let gate = CompileGate::new(workspace_root()).unwrap();
    let nonexistent = PathBuf::from("/nonexistent/path/Cargo.toml");

    let result = gate.validate(&CompileTarget::ManifestPath(nonexistent));

    assert!(result.is_err(), "Should return error for nonexistent manifest");
    match result {
        Err(CompileError::ManifestNotFound(path)) => {
            assert_eq!(path, PathBuf::from("/nonexistent/path/Cargo.toml"));
        }
        _ => panic!("Expected ManifestNotFound error, got: {:?}", result),
    }
}

#[test]
fn test_compile_gate_timeout_configured() {
    let gate = CompileGate::new(workspace_root())
        .unwrap()
        .with_timeout(Duration::from_secs(60));
    assert_eq!(gate.timeout(), Duration::from_secs(60));
}

#[test]
fn test_compile_gate_check_package_convenience() {
    let gate = CompileGate::new(workspace_root()).unwrap();

    // Success path
    let ok = gate.check_package("ggen-core");
    assert!(ok.is_ok(), "ggen-core should compile: {:?}", ok);

    // Failure path
    let err = gate.check_package("nonexistent-crate-xyz-12345");
    assert!(err.is_err(), "Nonexistent crate should fail");
    match err {
        Err(CompileError::CompilationFailed { target, .. }) => {
            assert_eq!(target, "nonexistent-crate-xyz-12345");
        }
        _ => panic!("Expected CompilationFailed error, got: {:?}", err),
    }
}

#[test]
fn test_compile_gate_check_manifest_convenience() {
    let gate = CompileGate::new(workspace_root()).unwrap();
    let manifest = workspace_root().join("crates/ggen-a2a-mcp/Cargo.toml");

    let result = gate.check_manifest(&manifest);
    assert!(
        result.is_ok(),
        "ggen-a2a-mcp should compile via manifest: {:?}",
        result
    );
}

#[test]
fn test_compile_gate_root_not_found() {
    let nonexistent = PathBuf::from("/this/path/does/not/exist/xyz123");
    let result = CompileGate::new(&nonexistent);

    assert!(result.is_err(), "Should return error for nonexistent root");
    match result {
        Err(CompileError::RootNotFound(path)) => {
            assert_eq!(path, nonexistent);
        }
        Err(other) => panic!("Expected RootNotFound error, got: {}", other),
        Ok(_) => panic!("Expected error, got Ok"),
    }
}

#[test]
fn test_compile_gate_pipeline_integration() {
    // Demonstrates μ₄ → μ₅ flow
    let gate = CompileGate::new(workspace_root())
        .unwrap()
        .with_timeout(Duration::from_secs(120));

    // μ₄: Validate
    let result = gate
        .validate(&CompileTarget::Package("ggen-core".to_string()))
        .unwrap();

    if result.passed {
        // μ₅: Emit (simulated by assertion)
        assert!(true, "Would proceed to emit stage");
    } else {
        panic!("Validation failed: {}", result.output);
    }
}

#[test]
fn test_compile_result_constructors() {
    // Success constructor
    let success = CompileResult::success("test-crate", 1234);
    assert!(success.passed);
    assert_eq!(success.target_name, "test-crate");
    assert_eq!(success.duration_ms, 1234);
    assert!(success.output.is_empty());

    // Failure constructor
    let failure = CompileResult::failure("broken-crate", 5678, "error: failed".to_string());
    assert!(!failure.passed);
    assert_eq!(failure.target_name, "broken-crate");
    assert_eq!(failure.duration_ms, 5678);
    assert_eq!(failure.output, "error: failed");
}
