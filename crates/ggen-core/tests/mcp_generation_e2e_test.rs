//! E2E test for MCP server generation from RDF ontology.
//! Chicago TDD: Real filesystem, real RDF graph, real code generation.
//!
//! These tests exercise the actual `generate_mcp_server()` function from the
//! `mcp` module, using real `tempfile::TempDir` for filesystem I/O and real
//! `.ttl` ontology files. No mocks, no test doubles.

use ggen_core::mcp::{generate_mcp_server, McpConfig, McpError, McpResult, McpTransport};
use std::path::PathBuf;
use tempfile::TempDir;

/// Minimal valid TTL ontology for MCP generation tests.
/// This is a real RDF file loaded by the actual Graph implementation (oxigraph).
const MINIMAL_ONTOLOGY: &str = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix mcp: <http://ggen.dev/ns/mcp#> .

mcp:TestServer a mcp:Server ;
    rdfs:label "Test MCP Server" ;
    mcp:transport "stdio" .
"#;

/// Verify that the mcp module compiles and exports the expected public API.
/// This is a real compilation check -- if the module is missing or malformed,
/// this test will not compile.
#[test]
#[ignore]
fn test_mcp_config_validation() {
    // Arrange: Valid config
    let config = McpConfig {
        ontology_path: PathBuf::from("dummy.ttl"),
        output_dir: PathBuf::from("/tmp/out"),
        transport: McpTransport::Stdio,
        server_name: "test_server".to_string(),
        validate: false,
        dry_run: true,
    };

    // Act & Assert: Config fields are accessible (compilation check)
    assert_eq!(config.server_name, "test_server");
    assert_eq!(config.transport, McpTransport::Stdio);
    assert!(config.dry_run);
    assert!(!config.validate);

    // Verify McpError variants exist and Display works
    let err = McpError::InvalidConfig("test message".to_string());
    assert_eq!(format!("{err}"), "invalid configuration: test message");

    let err = McpError::OntologyLoad("load failed".to_string());
    assert!(format!("{err}").contains("ontology load failed"));
}

/// Test that McpConfig rejects invalid server names.
/// This exercises the validation logic directly via generate_mcp_server()
/// which calls validate_config() internally.
#[tokio::test]
#[ignore]
async fn test_mcp_config_rejects_empty_server_name() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir: temp_dir.path().join("output"),
        transport: McpTransport::Stdio,
        server_name: "".to_string(),
        validate: false,
        dry_run: true,
    };

    let result = generate_mcp_server(config).await;
    assert!(result.is_err());
    let err = result.expect_err("should be Err");
    let msg = format!("{err}");
    assert!(
        msg.contains("server_name cannot be empty"),
        "Expected 'server_name cannot be empty', got: {msg}"
    );
}

/// Test that McpConfig rejects server names with invalid characters.
#[tokio::test]
#[ignore]
async fn test_mcp_config_rejects_invalid_characters() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir: temp_dir.path().join("output"),
        transport: McpTransport::Stdio,
        server_name: "my-server!".to_string(),
        validate: false,
        dry_run: true,
    };

    let result = generate_mcp_server(config).await;
    assert!(result.is_err());
    let err = result.expect_err("should be Err");
    let msg = format!("{err}");
    assert!(
        msg.contains("alphanumeric"),
        "Expected error about invalid characters, got: {msg}"
    );
}

/// Test dry_run mode -- should not write any files to disk.
/// Uses real TempDir and real Graph (Chicago TDD).
#[tokio::test]
#[ignore]
async fn test_mcp_generate_dry_run() {
    // Arrange: Create real temp directory and real TTL ontology file
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let output_dir = temp_dir.path().join("output");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir: output_dir.clone(),
        transport: McpTransport::Stdio,
        server_name: "dry_test_server".to_string(),
        validate: false,
        dry_run: true,
    };

    // Act: Run the generation pipeline in dry_run mode
    let result = generate_mcp_server(config).await;

    // Assert: Generation succeeds
    let mcp_result = result.expect("dry_run generation should succeed");
    assert_mcp_result(&mcp_result, "dry_test_server", McpTransport::Stdio);

    // Assert: No files were written to disk (dry_run)
    assert!(
        !output_dir.exists(),
        "dry_run should not create output directory"
    );
}

/// Test full pipeline: create a real .ttl ontology, run generate_mcp_server(),
/// verify files exist on disk. Uses real TempDir and real Graph (Chicago TDD).
#[tokio::test]
#[ignore]
async fn test_mcp_generate_with_real_files_stdio() {
    test_full_generation(McpTransport::Stdio).await;
}

/// Test full pipeline with HTTP transport.
#[tokio::test]
#[ignore]
async fn test_mcp_generate_with_real_files_http() {
    test_full_generation(McpTransport::Http).await;
}

/// Shared implementation for stdio and http transport tests.
async fn test_full_generation(transport: McpTransport) {
    // Arrange: Create real temp directory and real TTL ontology file
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let output_dir = temp_dir.path().join("output");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir: output_dir.clone(),
        transport,
        server_name: "e2e_test_server".to_string(),
        validate: false,
        dry_run: false,
    };

    // Act: Run the full generation pipeline (real filesystem writes)
    let result = generate_mcp_server(config).await;

    // Assert: Generation succeeds
    let mcp_result = result.expect("generation should succeed");
    assert_mcp_result(&mcp_result, "e2e_test_server", transport);

    // Assert: Output directory was created
    assert!(
        output_dir.exists(),
        "output directory should exist after generation"
    );

    // Assert: Expected files were written to disk
    assert!(
        output_dir.join("main.rs").exists(),
        "main.rs should exist"
    );
    assert!(
        output_dir.join("server.rs").exists(),
        "server.rs should exist"
    );
    assert!(
        output_dir.join("tools.rs").exists(),
        "tools.rs should exist"
    );
    assert!(
        output_dir.join("resources.rs").exists(),
        "resources.rs should exist"
    );
    assert!(
        output_dir.join("prompts.rs").exists(),
        "prompts.rs should exist"
    );
    assert!(
        output_dir.join("Cargo.toml").exists(),
        "Cargo.toml should exist"
    );

    // Assert: Transport-specific file exists
    match transport {
        McpTransport::Stdio => {
            assert!(
                output_dir.join("stdio_server.rs").exists(),
                "stdio_server.rs should exist for Stdio transport"
            );
        }
        McpTransport::Http => {
            assert!(
                output_dir.join("http_server.rs").exists(),
                "http_server.rs should exist for Http transport"
            );
        }
    }

    // Assert: Generated files have real content (not empty)
    let main_content =
        std::fs::read_to_string(output_dir.join("main.rs")).expect("main.rs should be readable");
    assert!(
        main_content.contains("e2e_test_server"),
        "main.rs should reference the server name"
    );
    assert!(
        main_content.contains("#![tokio::main]"),
        "main.rs should contain tokio main macro as inner attribute"
    );

    let cargo_content =
        std::fs::read_to_string(output_dir.join("Cargo.toml")).expect("Cargo.toml should be readable");
    assert!(
        cargo_content.contains("name = \"e2e-test-server\""),
        "Cargo.toml should have correct package name (dashes, not underscores)"
    );
    assert!(
        cargo_content.contains("rmcp"),
        "Cargo.toml should depend on rmcp"
    );
    assert!(
        cargo_content.contains("tokio"),
        "Cargo.toml should depend on tokio"
    );

    let server_content =
        std::fs::read_to_string(output_dir.join("server.rs")).expect("server.rs should be readable");
    assert!(
        server_content.contains("E2eTestServer"),
        "server.rs should have PascalCase struct name"
    );
}

/// Test that McpError::OntologyLoad is returned for a nonexistent ontology file.
#[tokio::test]
#[ignore]
async fn test_mcp_ontology_not_found() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let missing_path = temp_dir.path().join("does_not_exist.ttl");

    let config = McpConfig {
        ontology_path: missing_path,
        output_dir: temp_dir.path().join("output"),
        transport: McpTransport::Stdio,
        server_name: "test_server".to_string(),
        validate: false,
        dry_run: true,
    };

    let result = generate_mcp_server(config).await;
    assert!(result.is_err());
    let err = result.expect_err("should be Err");
    let msg = format!("{err}");
    assert!(
        msg.contains("ontology load failed"),
        "Expected ontology load error, got: {msg}"
    );
}

/// Test that an invalid (non-Turtle) ontology file returns an error.
#[tokio::test]
#[ignore]
async fn test_mcp_invalid_ontology_content() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("bad.ttl");
    std::fs::write(&ttl_path, "this is not valid turtle syntax {{{").expect("write should succeed");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir: temp_dir.path().join("output"),
        transport: McpTransport::Stdio,
        server_name: "test_server".to_string(),
        validate: false,
        dry_run: true,
    };

    let result = generate_mcp_server(config).await;
    assert!(result.is_err());
    let err = result.expect_err("should be Err");
    let msg = format!("{err}");
    assert!(
        msg.contains("ontology load failed"),
        "Expected ontology load error for invalid TTL, got: {msg}"
    );
}

/// Test determinism: running generation twice with the same inputs
/// produces the same receipt hash.
#[tokio::test]
#[ignore]
async fn test_mcp_generation_determinism() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let output_dir_a = temp_dir.path().join("output_a");
    let output_dir_b = temp_dir.path().join("output_b");

    let config_a = McpConfig {
        ontology_path: ttl_path.clone(),
        output_dir: output_dir_a,
        transport: McpTransport::Stdio,
        server_name: "determinism_test".to_string(),
        validate: false,
        dry_run: false,
    };

    let config_b = McpConfig {
        ontology_path: ttl_path,
        output_dir: output_dir_b,
        transport: McpTransport::Stdio,
        server_name: "determinism_test".to_string(),
        validate: false,
        dry_run: false,
    };

    let result_a = generate_mcp_server(config_a)
        .await
        .expect("first generation should succeed");
    let result_b = generate_mcp_server(config_b)
        .await
        .expect("second generation should succeed");

    assert_eq!(
        result_a.receipt, result_b.receipt,
        "Receipts should be identical for identical inputs (deterministic)"
    );
    assert_eq!(
        result_a.files_generated.len(),
        result_b.files_generated.len(),
        "Same number of files should be generated"
    );
}

/// Test McpTransport Display implementation.
#[test]
#[ignore]
fn test_mcp_transport_display() {
    assert_eq!(format!("{}", McpTransport::Stdio), "stdio");
    assert_eq!(format!("{}", McpTransport::Http), "http");
}

/// Test McpTransport PartialEq implementation.
#[test]
#[ignore]
fn test_mcp_transport_equality() {
    assert_eq!(McpTransport::Stdio, McpTransport::Stdio);
    assert_eq!(McpTransport::Http, McpTransport::Http);
    assert_ne!(McpTransport::Stdio, McpTransport::Http);
}

/// Test that server names starting with underscores are accepted.
#[tokio::test]
#[ignore]
async fn test_mcp_config_accepts_underscore_prefix() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let output_dir = temp_dir.path().join("output");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir,
        transport: McpTransport::Stdio,
        server_name: "_private_server".to_string(),
        validate: false,
        dry_run: true,
    };

    let result = generate_mcp_server(config).await;
    assert!(
        result.is_ok(),
        "Server name starting with underscore should be valid"
    );
}

/// Test that server names starting with digits are rejected.
#[tokio::test]
#[ignore]
async fn test_mcp_config_rejects_digit_start() {
    let temp_dir = TempDir::new().expect("TempDir creation should succeed");
    let ttl_path = temp_dir.path().join("ontology.ttl");
    std::fs::write(&ttl_path, MINIMAL_ONTOLOGY).expect("write should succeed");

    let config = McpConfig {
        ontology_path: ttl_path,
        output_dir: temp_dir.path().join("output"),
        transport: McpTransport::Stdio,
        server_name: "123_server".to_string(),
        validate: false,
        dry_run: true,
    };

    let result = generate_mcp_server(config).await;
    assert!(result.is_err());
    let err = result.expect_err("should be Err");
    let msg = format!("{err}");
    assert!(
        msg.contains("alphabetic"),
        "Expected error about alphabetic start, got: {msg}"
    );
}

// ---------------------------------------------------------------------------
// Helper assertions
// ---------------------------------------------------------------------------

/// Assert common invariants on an McpResult.
fn assert_mcp_result(result: &McpResult, expected_name: &str, expected_transport: McpTransport) {
    assert!(
        !result.files_generated.is_empty(),
        "files_generated should not be empty"
    );

    // Both stdio and http should produce 7 files:
    // main.rs, server.rs, tools.rs, resources.rs, prompts.rs,
    // {transport}_server.rs, Cargo.toml
    assert_eq!(
        result.files_generated.len(),
        7,
        "Expected 7 generated files, got {}: {:?}",
        result.files_generated.len(),
        result.files_generated
    );

    assert_eq!(
        result.server_name, expected_name,
        "server_name mismatch"
    );
    assert_eq!(
        result.transport, expected_transport,
        "transport mismatch"
    );

    // Elapsed time should be reasonable (generation is local, not network-bound)
    assert!(
        result.elapsed_ms < 10_000,
        "Generation took {}ms, expected < 10s",
        result.elapsed_ms
    );

    // Receipt should be a non-empty hex string (SHA-256 = 64 hex chars)
    assert!(
        !result.receipt.is_empty(),
        "receipt should not be empty"
    );
    assert_eq!(
        result.receipt.len(),
        64,
        "receipt should be 64 hex characters (SHA-256), got {} chars: {}",
        result.receipt.len(),
        result.receipt
    );
    assert!(
        result.receipt.chars().all(|c| c.is_ascii_hexdigit()),
        "receipt should be valid hex, got: {}",
        result.receipt
    );
}
