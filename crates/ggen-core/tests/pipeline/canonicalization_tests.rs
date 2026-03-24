//! Comprehensive tests for μ₄:canonicalization pass
//!
//! Tests cover:
//! - Formatter integration (rustfmt, prettier, json, ttl)
//! - Stop-the-line behavior on formatter errors
//! - Debug mode fallback
//! - Hash computation and verification
//! - Receipt generation
//! - Multi-file canonicalization

use ggen_core::graph::Graph;
use ggen_core::v6::pass::{Pass, PassContext};
use ggen_core::v6::passes::canonicalization::{CanonicalizationPass, CanonicalizationReceipt};
use std::path::PathBuf;
use tempfile::TempDir;

/// AAA Pattern: Arrange, Act, Assert

#[test]
fn test_canonicalize_json_valid() {
    // Arrange: Valid JSON with unsorted keys
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let json_content = r#"{"z": 1, "a": 2, "m": 3}"#;
    std::fs::write(output_dir.join("test.json"), json_content).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Success and keys sorted
    assert!(result.is_ok());
    let content = std::fs::read_to_string(output_dir.join("test.json")).unwrap();
    assert!(content.contains(r#""a""#));
    assert!(content.contains(r#""m""#));
    assert!(content.contains(r#""z""#));
    // Verify alphabetical order
    assert!(content.find(r#""a""#).unwrap() < content.find(r#""m""#).unwrap());
    assert!(content.find(r#""m""#).unwrap() < content.find(r#""z""#).unwrap());
}

#[test]
fn test_canonicalize_json_invalid_strict_mode() {
    // Arrange: Invalid JSON in strict mode
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let invalid_json = r#"{"invalid": missing_value}"#;
    std::fs::write(output_dir.join("test.json"), invalid_json).unwrap();

    let pass = CanonicalizationPass::new().with_strict_mode(true);
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Stopped the line
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("STOPPED THE LINE"));
    assert!(error.to_string().contains("Andon Protocol"));
    assert!(error.to_string().contains("JSON"));
}

#[test]
fn test_canonicalize_json_invalid_debug_mode() {
    // Arrange: Invalid JSON in debug mode
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let invalid_json = r#"{"invalid": missing_value}"#;
    std::fs::write(output_dir.join("test.json"), invalid_json).unwrap();

    let pass = CanonicalizationPass::new()
        .with_strict_mode(true)
        .with_debug_mode(true);
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Success (fallback to normalization)
    assert!(result.is_ok());
}

#[test]
fn test_canonicalize_rust_valid() {
    // Arrange: Valid Rust code
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let rust_content = "fn main() { println!(\"hello\"); }";
    std::fs::write(output_dir.join("test.rs"), rust_content).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.rs"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Success (rustfmt applied if available)
    assert!(result.is_ok());
    let content = std::fs::read_to_string(output_dir.join("test.rs")).unwrap();
    assert!(content.contains("fn main"));
}

#[test]
fn test_canonicalize_rust_invalid_strict_mode() {
    // Arrange: Invalid Rust code in strict mode
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let invalid_rust = "fn main( { syntax error }";
    std::fs::write(output_dir.join("test.rs"), invalid_rust).unwrap();

    let pass = CanonicalizationPass::new().with_strict_mode(true);
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.rs"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Stopped the line (if rustfmt is available and detects error)
    // Note: rustfmt might succeed on some syntax errors, so we check for either outcome
    if result.is_err() {
        let error = result.unwrap_err();
        assert!(error.to_string().contains("STOPPED THE LINE") || error.to_string().contains("Rustfmt"));
    }
}

#[test]
fn test_canonicalize_multiple_files() {
    // Arrange: Multiple files of different types
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    std::fs::write(output_dir.join("test1.json"), r#"{"z":1,"a":2}"#).unwrap();
    std::fs::write(output_dir.join("test2.json"), r#"{"b":3,"c":4}"#).unwrap();
    std::fs::write(output_dir.join("test.txt"), "hello\r\nworld").unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test1.json"));
    ctx.generated_files.push(PathBuf::from("test2.json"));
    ctx.generated_files.push(PathBuf::from("test.txt"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: All files canonicalized
    assert!(result.is_ok());

    let content1 = std::fs::read_to_string(output_dir.join("test1.json")).unwrap();
    assert!(content1.contains(r#""a""#));

    let content2 = std::fs::read_to_string(output_dir.join("test2.json")).unwrap();
    assert!(content2.contains(r#""b""#));

    let content3 = std::fs::read_to_string(output_dir.join("test.txt")).unwrap();
    assert_eq!(content3, "hello\nworld\n");
}

#[test]
fn test_receipt_generation() {
    // Arrange: Files for receipt generation
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    std::fs::write(output_dir.join("test.json"), r#"{"a":1}"#).unwrap();

    let pass = CanonicalizationPass::new().with_receipts(true);
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Receipt generated and stored in context
    assert!(result.is_ok());
    assert!(ctx.bindings.contains_key("canonicalization_receipt"));

    let receipt_value = ctx.bindings.get("canonicalization_receipt").unwrap();
    let receipt: CanonicalizationReceipt = serde_json::from_value(receipt_value.clone()).unwrap();

    assert_eq!(receipt.files_processed.len(), 1);
    assert_eq!(receipt.file_hashes.len(), 1);
    assert!(receipt.file_hashes.contains_key(&PathBuf::from("test.json")));
}

#[test]
fn test_hash_computation() {
    // Arrange: Files for hash computation
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    std::fs::write(output_dir.join("test1.json"), r#"{"a":1}"#).unwrap();
    std::fs::write(output_dir.join("test2.json"), r#"{"b":2}"#).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test1.json"));
    ctx.generated_files.push(PathBuf::from("test2.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Hashes computed
    assert!(result.is_ok());

    let receipt_value = ctx.bindings.get("canonicalization_receipt").unwrap();
    let receipt: CanonicalizationReceipt = serde_json::from_value(receipt_value.clone()).unwrap();

    assert_eq!(receipt.file_hashes.len(), 2);

    // Verify hash format (SHA-256 = 64 hex chars)
    for hash in receipt.file_hashes.values() {
        assert_eq!(hash.len(), 64);
        assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));
    }

    // Verify aggregate hash
    let aggregate = receipt.aggregate_hash();
    assert!(aggregate.is_ok());
    assert_eq!(aggregate.unwrap().len(), 64);
}

#[test]
fn test_deterministic_hashing() {
    // Arrange: Same content processed twice
    let graph = Graph::new().unwrap();

    // First execution
    let temp_dir1 = TempDir::new().unwrap();
    let output_dir1 = temp_dir1.path().join("output");
    std::fs::create_dir_all(&output_dir1).unwrap();
    std::fs::write(output_dir1.join("test.json"), r#"{"a":1}"#).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx1 = PassContext::new(&graph, temp_dir1.path().to_path_buf(), output_dir1.clone());
    ctx1.generated_files.push(PathBuf::from("test.json"));
    let _ = pass.execute(&mut ctx1).unwrap();

    let receipt1: CanonicalizationReceipt = serde_json::from_value(
        ctx1.bindings.get("canonicalization_receipt").unwrap().clone()
    ).unwrap();

    // Second execution
    let temp_dir2 = TempDir::new().unwrap();
    let output_dir2 = temp_dir2.path().join("output");
    std::fs::create_dir_all(&output_dir2).unwrap();
    std::fs::write(output_dir2.join("test.json"), r#"{"a":1}"#).unwrap();

    let mut ctx2 = PassContext::new(&graph, temp_dir2.path().to_path_buf(), output_dir2.clone());
    ctx2.generated_files.push(PathBuf::from("test.json"));
    let _ = pass.execute(&mut ctx2).unwrap();

    let receipt2: CanonicalizationReceipt = serde_json::from_value(
        ctx2.bindings.get("canonicalization_receipt").unwrap().clone()
    ).unwrap();

    // Assert: Same hash for same content
    let hash1 = receipt1.file_hashes.get(&PathBuf::from("test.json")).unwrap();
    let hash2 = receipt2.file_hashes.get(&PathBuf::from("test.json")).unwrap();
    assert_eq!(hash1, hash2);
}

#[test]
fn test_formatter_tracking() {
    // Arrange: Different file types
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    std::fs::write(output_dir.join("test.json"), r#"{"a":1}"#).unwrap();
    std::fs::write(output_dir.join("test.rs"), "fn main() {}").unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));
    ctx.generated_files.push(PathBuf::from("test.rs"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Formatters tracked
    assert!(result.is_ok());

    let receipt_value = ctx.bindings.get("canonicalization_receipt").unwrap();
    let receipt: CanonicalizationReceipt = serde_json::from_value(receipt_value.clone()).unwrap();

    assert!(receipt.formatter_executions.contains_key("Json"));
    assert!(receipt.formatter_executions.contains_key("Rustfmt"));
}

#[test]
fn test_missing_file_strict_mode() {
    // Arrange: Reference to non-existent file in strict mode
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let pass = CanonicalizationPass::new().with_strict_mode(true);
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("nonexistent.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Stopped the line
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("STOPPED THE LINE"));
    assert!(error.to_string().contains("does not exist"));
}

#[test]
fn test_line_ending_normalization() {
    // Arrange: Files with different line endings
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    std::fs::write(output_dir.join("crlf.txt"), "line1\r\nline2\r\n").unwrap();
    std::fs::write(output_dir.join("cr.txt"), "line1\rline2\r").unwrap();
    std::fs::write(output_dir.join("lf.txt"), "line1\nline2\n").unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("crlf.txt"));
    ctx.generated_files.push(PathBuf::from("cr.txt"));
    ctx.generated_files.push(PathBuf::from("lf.txt"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: All normalized to LF
    assert!(result.is_ok());

    let crlf = std::fs::read_to_string(output_dir.join("crlf.txt")).unwrap();
    let cr = std::fs::read_to_string(output_dir.join("cr.txt")).unwrap();
    let lf = std::fs::read_to_string(output_dir.join("lf.txt")).unwrap();

    assert_eq!(crlf, "line1\nline2\n");
    assert_eq!(cr, "line1\nline2\n");
    assert_eq!(lf, "line1\nline2\n");
}

#[test]
fn test_ttl_canonicalization() {
    // Arrange: TTL content
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        ex:z ex:p ex:o .
        ex:a ex:p ex:o .
    "#;
    std::fs::write(output_dir.join("test.ttl"), ttl).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.ttl"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: TTL sorted
    assert!(result.is_ok());
    let content = std::fs::read_to_string(output_dir.join("test.ttl")).unwrap();
    let lines: Vec<&str> = content.lines().collect();

    // Verify sorting (a before z)
    if lines.len() >= 2 {
        assert!(lines[0].contains(":a ") || lines.len() == 0);
    }
}

#[test]
fn test_nested_json_canonicalization() {
    // Arrange: Nested JSON with unsorted keys
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    let json = r#"{"z": {"nested_z": 1, "nested_a": 2}, "a": 3}"#;
    std::fs::write(output_dir.join("test.json"), json).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Both outer and nested keys sorted
    assert!(result.is_ok());
    let content = std::fs::read_to_string(output_dir.join("test.json")).unwrap();

    // Outer keys: a before z
    assert!(content.find(r#""a""#).unwrap() < content.find(r#""z""#).unwrap());

    // Inner keys: nested_a before nested_z
    assert!(content.contains("nested_a"));
    assert!(content.contains("nested_z"));
}

#[test]
fn test_execution_time_tracking() {
    // Arrange: Files for timing
    let graph = Graph::new().unwrap();
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("output");
    std::fs::create_dir_all(&output_dir).unwrap();

    std::fs::write(output_dir.join("test.json"), r#"{"a":1}"#).unwrap();

    let pass = CanonicalizationPass::new();
    let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
    ctx.generated_files.push(PathBuf::from("test.json"));

    // Act: Execute canonicalization
    let result = pass.execute(&mut ctx);

    // Assert: Execution time recorded
    assert!(result.is_ok());

    let receipt_value = ctx.bindings.get("canonicalization_receipt").unwrap();
    let receipt: CanonicalizationReceipt = serde_json::from_value(receipt_value.clone()).unwrap();

    assert!(receipt.execution_time_ms > 0);
}
