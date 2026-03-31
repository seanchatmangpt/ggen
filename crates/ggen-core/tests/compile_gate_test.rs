//! Integration tests for CompileGate.
//!
//! Chicago TDD: these tests run the REAL `cargo check` against actual crates
//! in the workspace. No mocks, no test doubles.
//!
//! These tests are intentionally integration-level: they invoke the real cargo
//! process and assert on the observable compilation outcome.

use ggen_core::validation::compile_gate::{CompileGate, CompileResult};

/// Find the workspace root from CARGO_MANIFEST_DIR (compile-time env var).
fn workspace_root() -> std::path::PathBuf {
    let mut dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.pop(); // ggen-core
    dir.pop(); // crates
    dir
}

#[test]
fn test_compile_gate_ggen_a2a_mcp_succeeds() {
    // Arrange: create a CompileGate targeting this workspace.
    let gate = CompileGate::new(workspace_root()).with_timeout(300);

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
    let gate = CompileGate::new(workspace_root()).with_timeout(300);
    let result = gate.validate_crate("ggen-core");
    assert!(
        result.passed,
        "Expected ggen-core to compile, but cargo check failed:\n{}",
        result.output
    );
}

#[test]
fn test_compile_gate_nonexistent_crate_fails() {
    let gate = CompileGate::new(workspace_root()).with_timeout(60);
    let result = gate.validate_crate("nonexistent-crate-xyz-12345");

    assert!(!result.passed, "Nonexistent crate should fail compilation");
    assert!(
        !result.output.is_empty(),
        "Error output should be populated for failed compilation"
    );
}

#[test]
fn test_compile_gate_check_convenience_api() {
    let gate = CompileGate::new(workspace_root()).with_timeout(300);

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
        crate_name: "my-crate".to_string(),
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
        crate_name: "broken-crate".to_string(),
        passed: false,
        duration_ms: 999,
        output: "error[E0425]: cannot find value `x` in this scope".to_string(),
    };
    let text = format!("{}", result);
    assert!(text.contains("[FAIL]"));
    assert!(text.contains("broken-crate"));
    assert!(text.contains("cannot find value"));
}
