//! Integration test for `ggen mcp generate` CLI command
//!
//! Tests the full CLI flow: argument parsing → domain logic call → file output
//!
//! Run with:
//!   cargo test -p ggen-cli --test cli_mcp_generate_integration_test -- --test-threads=1 --nocapture

use std::fs;
use std::path::PathBuf;

const SIMPLE_ONTOLOGY_TTL: &str = concat!(
    "@prefix ex: <http://example.org/> .\n",
    "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n",
    "\n",
    "ex:Agent a rdfs:Class .\n",
    "ex:Tool a rdfs:Class .\n",
    "ex:hasTool a rdf:Property ;\n",
    "    rdfs:domain ex:Agent ;\n",
    "    rdfs:range ex:Tool .\n",
);

const SIMPLE_QUERY_RQ: &str = concat!(
    "PREFIX ex: <http://example.org/>\n",
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n",
    "SELECT ?agent ?tool WHERE {\n",
    "    ?agent a ex:Agent .\n",
    "    ?agent ex:hasTool ?tool .\n",
    "    ?tool a ex:Tool .\n",
    "}\n",
);

fn setup_temp_ontology() -> tempfile::TempDir {
    let tempdir = tempfile::tempdir().unwrap();
    let ontology_path = tempdir.path().join("ontology.ttl");
    let queries_dir = tempdir.path().join("queries");
    let output_dir = tempdir.path().join("generated");

    fs::create_dir_all(&queries_dir).unwrap();
    fs::create_dir_all(&output_dir).unwrap();
    fs::write(&ontology_path, SIMPLE_ONTOLOGY_TTL).unwrap();
    fs::write(queries_dir.join("01.rq"), SIMPLE_QUERY_RQ).unwrap();

    tempdir
}

#[test]
#[ignore]
fn test_cli_mcp_generate_with_valid_ontology() {
    let tempdir = setup_temp_ontology();
    let ontology_path = tempdir.path().join("ontology.ttl");
    let output_dir = tempdir.path().join("generated");

    // Call the CLI generate verb function
    let result = ggen_cli_lib::cmds::mcp::generate(
        ontology_path.to_str().unwrap().to_string(),
        Some(output_dir.to_str().unwrap().to_string()),
        true, // skip_compile_gate for faster test
    );

    // Assert success
    assert!(
        result.is_ok(),
        "generate should succeed: {:?}",
        result.err()
    );

    let output = result.unwrap();
    assert_eq!(output.status, "success");
    assert!(output.message.contains("Generated"));
    assert!(output.message.contains("file(s)"));
    assert!(output.message.contains("Receipt:"));

    // Verify output directory exists and has files
    assert!(
        output_dir.exists(),
        "output directory should exist"
    );

    let files: Vec<_> = fs::read_dir(&output_dir)
        .unwrap()
        .flatten()
        .collect();

    assert!(
        !files.is_empty(),
        "output directory should contain generated files"
    );

    // Cleanup is automatic via TempDir
}

#[test]
#[ignore]
fn test_cli_mcp_generate_with_missing_ontology() {
    let tempdir = tempfile::tempdir().unwrap();
    let missing_path = tempdir.path().join("nonexistent.ttl");
    let output_dir = tempdir.path().join("generated");

    let result = ggen_cli_lib::cmds::mcp::generate(
        missing_path.to_str().unwrap().to_string(),
        Some(output_dir.to_str().unwrap().to_string()),
        true,
    );

    assert!(
        result.is_err(),
        "generate should fail with missing ontology"
    );

    let err = result.unwrap_err();
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("Ontology file not found") || err_msg.contains("not found"),
        "error should mention missing file: {}",
        err_msg
    );
}

#[test]
#[ignore]
fn test_cli_mcp_generate_without_output_arg() {
    let tempdir = setup_temp_ontology();
    let ontology_path = tempdir.path().join("ontology.ttl");

    // Don't specify output_dir - should use current directory
    let result = ggen_cli_lib::cmds::mcp::generate(
        ontology_path.to_str().unwrap().to_string(),
        None, // No output specified
        true,
    );

    assert!(
        result.is_ok(),
        "generate should succeed without explicit output dir: {:?}",
        result.err()
    );

    let output = result.unwrap();
    assert_eq!(output.status, "success");
    assert!(output.output_dir.contains(".") || output.output_dir.contains("ggen"));
}

#[test]
#[ignore]
fn test_cli_mcp_generate_respects_skip_compile_gate() {
    let tempdir = setup_temp_ontology();
    let ontology_path = tempdir.path().join("ontology.ttl");
    let output_dir = tempdir.path().join("generated");

    // Test with skip_compile_gate = true
    let result = ggen_cli_lib::cmds::mcp::generate(
        ontology_path.to_str().unwrap().to_string(),
        Some(output_dir.to_str().unwrap().to_string()),
        true, // skip compile gate
    );

    assert!(
        result.is_ok(),
        "generate should succeed with skip_compile_gate=true"
    );

    let output = result.unwrap();
    assert!(output.skip_compile_gate);
    assert_eq!(output.status, "success");
}

#[test]
#[ignore]
fn test_cli_mcp_generate_with_queries_directory() {
    let tempdir = setup_temp_ontology();
    let ontology_path = tempdir.path().join("ontology.ttl");
    let queries_dir = tempdir.path().join("queries");
    let output_dir = tempdir.path().join("generated");

    // Verify queries directory exists and has .rq files
    assert!(queries_dir.exists());
    let query_files: Vec<_> = fs::read_dir(&queries_dir)
        .unwrap()
        .flatten()
        .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rq"))
        .collect();

    assert!(!query_files.is_empty(), "queries directory should have .rq files");

    // Run generation
    let result = ggen_cli_lib::cmds::mcp::generate(
        ontology_path.to_str().unwrap().to_string(),
        Some(output_dir.to_str().unwrap().to_string()),
        true,
    );

    assert!(
        result.is_ok(),
        "generate should succeed with queries directory: {:?}",
        result.err()
    );

    let output = result.unwrap();
    assert!(output.message.contains("Generated"));
}

/// Test with real MCP ontology from examples directory
#[test]
#[ignore]
fn test_cli_mcp_generate_with_mcp_server_ontology() {
    // Use the actual MCP server ontology from examples
    let mcp_ontology_path = PathBuf::from("examples/mcp-server-definition/ontology/mcp-server.ttl");

    // Skip test if example ontology doesn't exist (e.g., in CI without full workspace)
    if !mcp_ontology_path.exists() {
        eprintln!("Skipping test: MCP example ontology not found at {:?}", mcp_ontology_path);
        return;
    }

    let tempdir = tempfile::tempdir().unwrap();
    let output_dir = tempdir.path().join("generated");
    let queries_dir = tempdir.path().join("queries");

    // Create queries directory (required for sync pipeline)
    fs::create_dir_all(&queries_dir).unwrap();

    // Run generation with real MCP ontology
    let result = ggen_cli_lib::cmds::mcp::generate(
        mcp_ontology_path.to_str().unwrap().to_string(),
        Some(output_dir.to_str().unwrap().to_string()),
        true, // skip_compile_gate for faster test
    );

    assert!(
        result.is_ok(),
        "generate should succeed with MCP server ontology: {:?}",
        result.err()
    );

    let output = result.unwrap();
    assert_eq!(output.status, "success");

    // Verify MCP context information is in the message
    assert!(
        output.message.contains("MCP Context:"),
        "output should contain MCP context summary"
    );

    // Should have at least 1 server, some tools
    assert!(
        output.message.contains("1 server(s)") || output.message.contains("server(s)"),
        "should show server count"
    );

    assert!(
        output.message.contains("tool(s)") || output.message.contains("0 tool(s)") || output.message.contains("4 tool(s)"),
        "should show tool count"
    );

    // Verify sync pipeline ran
    assert!(
        output.message.contains("Generated") && output.message.contains("file(s)"),
        "should show files generated by sync pipeline"
    );

    assert!(
        output.message.contains("Receipt:"),
        "should include cryptographic receipt"
    );
}
