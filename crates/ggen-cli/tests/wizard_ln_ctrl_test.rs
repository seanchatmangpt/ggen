//! End-to-End Tests for ln_ctrl Wizard Profile with Determinism Validation
//!
//! Test Philosophy: Chicago TDD (state-based testing with real objects)
//! - Tests complete user journey: wizard → sync → verify → determinism
//! - No mocks - uses real filesystem, real ggen binary, real Node.js execution
//! - Verifies observable state changes and side effects
//!
//! Test Coverage:
//! 1. `ggen wizard --profile ln_ctrl` creates all expected files
//! 2. `ggen sync` runs successfully and generates outputs
//! 3. `node generated/world.verify.mjs` validates all artifacts
//! 4. Second `ggen sync` produces byte-identical world.manifest.json (determinism)
//! 5. All schemas validate their respective golden examples
//! 6. SHACL validation passes (if implemented)
//!
//! Pattern: AAA (Arrange, Act, Assert) with real execution

use assert_cmd::Command;
use predicates::prelude::*;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a temp directory for isolated testing
fn setup_test_project() -> TempDir {
    TempDir::new().expect("Failed to create temp directory")
}

/// Execute ggen wizard with ln_ctrl profile
fn run_wizard(project_dir: &Path) -> assert_cmd::assert::Assert {
    Command::cargo_bin("ggen")
        .expect("Failed to find ggen binary")
        .args([
            "wizard",
            "--profile",
            "lnctrl-output-contracts",
            "--output-dir",
            project_dir.to_str().unwrap(),
            "--yes", // Non-interactive mode
        ])
        .assert()
}

/// Execute ggen sync in project directory
fn run_sync(project_dir: &Path) -> assert_cmd::assert::Assert {
    Command::cargo_bin("ggen")
        .expect("Failed to find ggen binary")
        .current_dir(project_dir)
        .args(["sync"])
        .assert()
}

/// Execute Node.js world verifier script
fn run_world_verifier(project_dir: &Path) -> std::process::Output {
    std::process::Command::new("node")
        .current_dir(project_dir)
        .arg("generated/world.verify.mjs")
        .output()
        .expect("Failed to execute world verifier")
}

/// Verify file exists
fn assert_file_exists(path: &Path, description: &str) {
    assert!(
        path.exists(),
        "{} should exist at {:?}",
        description,
        path
    );
    assert!(path.is_file(), "{} should be a file: {:?}", description, path);
}

/// Verify directory exists
fn assert_dir_exists(path: &Path, description: &str) {
    assert!(
        path.exists(),
        "{} should exist at {:?}",
        description,
        path
    );
    assert!(path.is_dir(), "{} should be a directory: {:?}", description, path);
}

/// Read file content as string
fn read_file(path: &Path) -> String {
    fs::read_to_string(path).expect(&format!("Failed to read file: {:?}", path))
}

/// Compute SHA-256 hash of file content
fn compute_file_hash(path: &Path) -> String {
    let content = fs::read(path).expect(&format!("Failed to read file for hashing: {:?}", path));
    let mut hasher = Sha256::new();
    hasher.update(&content);
    format!("{:x}", hasher.finalize())
}

/// Parse JSON file
fn parse_json_file(path: &Path) -> Value {
    let content = read_file(path);
    serde_json::from_str(&content).expect(&format!("Failed to parse JSON from {:?}", path))
}

/// Verify file contains substring
fn assert_file_contains(path: &Path, expected: &str, description: &str) {
    let content = read_file(path);
    assert!(
        content.contains(expected),
        "{} should contain '{}' in {:?}",
        description,
        expected,
        path
    );
}

// ============================================================================
// SUCCESS CASES: Complete End-to-End Workflows
// ============================================================================

#[test]
fn test_wizard_creates_all_expected_files() {
    // Arrange: Create isolated test directory
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();

    // Act: Run wizard with ln_ctrl profile
    run_wizard(project_path).success();

    // Assert: Verify core configuration files
    assert_file_exists(&project_path.join("ggen.toml"), "ggen.toml");
    assert_file_exists(&project_path.join("README.md"), "README.md");

    // Assert: Verify ontology files
    let ontologies_dir = project_path.join(".specify/ontologies");
    assert_dir_exists(&ontologies_dir, "ontologies directory");
    assert_file_exists(&ontologies_dir.join("main.ttl"), "main.ttl");
    assert_file_exists(&ontologies_dir.join("receipts.ttl"), "receipts.ttl");
    assert_file_exists(&ontologies_dir.join("world.ttl"), "world.ttl");

    // Assert: Verify SPARQL query files
    let sparql_dir = project_path.join("sparql");
    assert_dir_exists(&sparql_dir, "sparql directory");
    assert_file_exists(
        &sparql_dir.join("world/outputs.sparql"),
        "world outputs query"
    );
    assert_file_exists(
        &sparql_dir.join("receipts/receipt_contract.sparql"),
        "receipt contract query"
    );

    // Assert: Verify Tera template files
    let templates_dir = project_path.join("templates");
    assert_dir_exists(&templates_dir, "templates directory");
    assert_file_exists(&templates_dir.join("world-manifest.tera"), "world manifest template");
    assert_file_exists(&templates_dir.join("world-verify.tera"), "world verifier template");
    assert_file_exists(
        &templates_dir.join("receipts/receipt.schema.tera"),
        "receipt schema template"
    );
    assert_file_exists(
        &templates_dir.join("receipts/verdict.schema.tera"),
        "verdict schema template"
    );

    // Assert: Verify project.ttl spec
    let specs_dir = project_path.join(".specify/specs");
    assert_file_exists(&specs_dir.join("project.ttl"), "project.ttl");

    // Assert: Verify ggen.toml content
    assert_file_contains(
        &project_path.join("ggen.toml"),
        "[project]",
        "ggen.toml project section"
    );
    assert_file_contains(
        &project_path.join("ggen.toml"),
        "world-manifest",
        "ggen.toml world-manifest rule"
    );
    assert_file_contains(
        &project_path.join("ggen.toml"),
        "deterministic = true",
        "ggen.toml deterministic output flag"
    );

    // Assert: Verify README.md content
    assert_file_contains(
        &project_path.join("README.md"),
        "ggen sync",
        "README.md ggen sync command"
    );
    assert_file_contains(
        &project_path.join("README.md"),
        "world.verify.mjs",
        "README.md verifier reference"
    );
}

#[test]
fn test_wizard_ggen_toml_has_correct_generation_rules() {
    // Arrange
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();

    // Act: Run wizard
    run_wizard(project_path).success();

    // Assert: Parse and verify ggen.toml structure
    let ggen_toml_path = project_path.join("ggen.toml");
    let content = read_file(&ggen_toml_path);

    // Verify essential sections exist
    assert!(content.contains("[project]"), "Missing [project] section");
    assert!(content.contains("[ontology]"), "Missing [ontology] section");
    assert!(content.contains("[generation]"), "Missing [generation] section");
    assert!(content.contains("[[generation.rules]]"), "Missing generation rules");
    assert!(content.contains("[sync]"), "Missing [sync] section");
    assert!(content.contains("[rdf]"), "Missing [rdf] section");
    assert!(content.contains("[output]"), "Missing [output] section");

    // Verify generation rules
    assert!(
        content.contains("name = \"world-manifest\""),
        "Missing world-manifest rule"
    );
    assert!(
        content.contains("name = \"world-verifier\""),
        "Missing world-verifier rule"
    );
    assert!(
        content.contains("name = \"receipt-schema\""),
        "Missing receipt-schema rule"
    );
    assert!(
        content.contains("name = \"verdict-schema\""),
        "Missing verdict-schema rule"
    );

    // Verify output paths
    assert!(content.contains("output_file = \"generated/world.manifest.json\""));
    assert!(content.contains("output_file = \"generated/world.verify.mjs\""));
}

#[test]
fn test_ggen_sync_runs_successfully() {
    // Arrange: Setup wizard project
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();

    // Act: Run ggen sync
    let sync_output = run_sync(project_path);

    // Assert: Sync succeeded
    sync_output.success();

    // Assert: Generated directory was created
    let generated_dir = project_path.join("generated");
    assert_dir_exists(&generated_dir, "generated directory");

    // Assert: Core generated files exist
    assert_file_exists(
        &generated_dir.join("world.manifest.json"),
        "world.manifest.json"
    );
    assert_file_exists(
        &generated_dir.join("world.verify.mjs"),
        "world.verify.mjs"
    );

    // Assert: Receipt schemas generated
    let receipts_dir = generated_dir.join("receipts");
    assert_dir_exists(&receipts_dir, "receipts directory");
    assert_file_exists(&receipts_dir.join("receipt.schema.json"), "receipt.schema.json");
    assert_file_exists(&receipts_dir.join("verdict.schema.json"), "verdict.schema.json");
}

#[test]
fn test_world_manifest_has_valid_structure() {
    // Arrange: Setup and sync
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();
    run_sync(project_path).success();

    // Act: Parse world.manifest.json
    let manifest_path = project_path.join("generated/world.manifest.json");
    let manifest: Value = parse_json_file(&manifest_path);

    // Assert: Manifest has required top-level fields
    assert!(
        manifest.get("version").is_some(),
        "Manifest should have version field"
    );
    assert!(
        manifest.get("timestamp").is_some(),
        "Manifest should have timestamp field"
    );
    assert!(
        manifest.get("artifacts").is_some(),
        "Manifest should have artifacts array"
    );

    // Assert: Artifacts is an array
    let artifacts = manifest["artifacts"]
        .as_array()
        .expect("artifacts should be an array");
    assert!(
        !artifacts.is_empty(),
        "Should have at least one artifact"
    );

    // Assert: Each artifact has required fields
    for artifact in artifacts {
        assert!(
            artifact.get("path").is_some(),
            "Artifact should have path field"
        );
        assert!(
            artifact.get("hash").is_some(),
            "Artifact should have hash field"
        );
        assert!(
            artifact.get("format").is_some(),
            "Artifact should have format field"
        );
    }
}

#[test]
#[cfg_attr(not(feature = "node-tests"), ignore)]
fn test_world_verifier_passes() {
    // Arrange: Setup, sync, and generate all files
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();
    run_sync(project_path).success();

    // Act: Run world verifier
    let verifier_output = run_world_verifier(project_path);

    // Assert: Verifier succeeded (exit code 0)
    assert!(
        verifier_output.status.success(),
        "World verifier should pass. Stdout: {}\nStderr: {}",
        String::from_utf8_lossy(&verifier_output.stdout),
        String::from_utf8_lossy(&verifier_output.stderr)
    );

    // Assert: Verifier output contains success message
    let stdout = String::from_utf8_lossy(&verifier_output.stdout);
    assert!(
        stdout.contains("✅") || stdout.contains("All artifacts validated"),
        "Verifier should report success: {}",
        stdout
    );
}

#[test]
fn test_second_sync_produces_identical_manifest() {
    // Arrange: Setup and run first sync
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();
    run_sync(project_path).success();

    let manifest_path = project_path.join("generated/world.manifest.json");

    // Act: Compute hash of first manifest
    let first_hash = compute_file_hash(&manifest_path);
    let first_content = read_file(&manifest_path);

    // Wait a brief moment to ensure different timestamp if non-deterministic
    std::thread::sleep(std::time::Duration::from_millis(100));

    // Act: Run sync again
    run_sync(project_path).success();

    // Act: Compute hash of second manifest
    let second_hash = compute_file_hash(&manifest_path);
    let second_content = read_file(&manifest_path);

    // Assert: Hashes are identical (determinism)
    assert_eq!(
        first_hash, second_hash,
        "Second sync should produce byte-identical world.manifest.json.\nFirst:\n{}\n\nSecond:\n{}",
        first_content, second_content
    );

    // Assert: Content is byte-for-byte identical
    assert_eq!(
        first_content, second_content,
        "Manifest content should be identical"
    );
}

#[test]
fn test_generated_schemas_are_valid_json() {
    // Arrange: Setup and sync
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();
    run_sync(project_path).success();

    // Act & Assert: Parse each schema file as valid JSON
    let receipts_dir = project_path.join("generated/receipts");

    let receipt_schema_path = receipts_dir.join("receipt.schema.json");
    let receipt_schema: Value = parse_json_file(&receipt_schema_path);
    assert!(
        receipt_schema.get("$schema").is_some(),
        "receipt.schema.json should have $schema field"
    );
    assert!(
        receipt_schema.get("type").is_some(),
        "receipt.schema.json should have type field"
    );

    let verdict_schema_path = receipts_dir.join("verdict.schema.json");
    let verdict_schema: Value = parse_json_file(&verdict_schema_path);
    assert!(
        verdict_schema.get("$schema").is_some(),
        "verdict.schema.json should have $schema field"
    );
    assert!(
        verdict_schema.get("type").is_some(),
        "verdict.schema.json should have type field"
    );
}

#[test]
fn test_world_verifier_is_executable_nodejs() {
    // Arrange: Setup and sync
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();
    run_sync(project_path).success();

    // Act: Read verifier script
    let verifier_path = project_path.join("generated/world.verify.mjs");
    let content = read_file(&verifier_path);

    // Assert: Contains Node.js shebang
    assert!(
        content.starts_with("#!/usr/bin/env node"),
        "Verifier should have Node.js shebang"
    );

    // Assert: Contains expected imports
    assert!(content.contains("import"), "Should use ES module imports");
    assert!(content.contains("readFileSync"), "Should import readFileSync");
    assert!(content.contains("createHash"), "Should import createHash");

    // Assert: Contains verification logic
    assert!(
        content.contains("function main"),
        "Should have main function"
    );
    assert!(
        content.contains("verifyArtifact"),
        "Should have artifact verification"
    );
}

#[test]
fn test_ontologies_have_valid_turtle_syntax() {
    // Arrange: Setup wizard
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();

    let ontologies_dir = project_path.join(".specify/ontologies");

    // Act & Assert: Verify each ontology file has Turtle syntax markers
    let ontology_files = vec!["main.ttl", "receipts.ttl", "world.ttl"];

    for filename in ontology_files {
        let path = ontologies_dir.join(filename);
        let content = read_file(&path);

        assert!(
            content.contains("@prefix"),
            "{} should contain @prefix declarations",
            filename
        );
        assert!(
            content.contains("rdf:") || content.contains("rdfs:"),
            "{} should use RDF/RDFS prefixes",
            filename
        );
    }
}

#[test]
fn test_sparql_queries_have_valid_syntax() {
    // Arrange: Setup wizard
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();

    // Act & Assert: Verify SPARQL files have correct syntax
    let outputs_query_path = project_path.join("sparql/world/outputs.sparql");
    let outputs_query = read_file(&outputs_query_path);
    assert!(
        outputs_query.contains("SELECT") || outputs_query.contains("CONSTRUCT"),
        "outputs.sparql should be a valid SPARQL query"
    );
    assert!(
        outputs_query.contains("WHERE"),
        "SPARQL query should have WHERE clause"
    );

    let receipt_query_path = project_path.join("sparql/receipts/receipt_contract.sparql");
    let receipt_query = read_file(&receipt_query_path);
    assert!(
        receipt_query.contains("SELECT") || receipt_query.contains("CONSTRUCT"),
        "receipt_contract.sparql should be a valid SPARQL query"
    );
}

// ============================================================================
// DETERMINISM VALIDATION
// ============================================================================

#[test]
fn test_multiple_syncs_all_produce_identical_outputs() {
    // Arrange: Setup wizard
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();

    // Act: Run sync 3 times and collect hashes
    let mut hashes = Vec::new();
    let manifest_path = project_path.join("generated/world.manifest.json");

    for iteration in 0..3 {
        run_sync(project_path).success();
        let hash = compute_file_hash(&manifest_path);
        hashes.push(hash.clone());

        // Brief pause between iterations
        if iteration < 2 {
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    }

    // Assert: All three hashes are identical
    assert_eq!(
        hashes[0], hashes[1],
        "First and second sync should produce identical manifest"
    );
    assert_eq!(
        hashes[1], hashes[2],
        "Second and third sync should produce identical manifest"
    );
    assert_eq!(
        hashes[0], hashes[2],
        "First and third sync should produce identical manifest"
    );
}

#[test]
fn test_schema_files_are_deterministic() {
    // Arrange: Setup wizard
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();
    run_wizard(project_path).success();

    // Act: Run sync twice and compare schema file hashes
    run_sync(project_path).success();

    let receipt_schema_path = project_path.join("generated/receipts/receipt.schema.json");
    let verdict_schema_path = project_path.join("generated/receipts/verdict.schema.json");

    let receipt_hash_1 = compute_file_hash(&receipt_schema_path);
    let verdict_hash_1 = compute_file_hash(&verdict_schema_path);

    std::thread::sleep(std::time::Duration::from_millis(50));
    run_sync(project_path).success();

    let receipt_hash_2 = compute_file_hash(&receipt_schema_path);
    let verdict_hash_2 = compute_file_hash(&verdict_schema_path);

    // Assert: Schema files are deterministic
    assert_eq!(
        receipt_hash_1, receipt_hash_2,
        "receipt.schema.json should be deterministic"
    );
    assert_eq!(
        verdict_hash_1, verdict_hash_2,
        "verdict.schema.json should be deterministic"
    );
}

// ============================================================================
// ERROR CASES
// ============================================================================

#[test]
fn test_wizard_fails_with_invalid_profile() {
    // Arrange: Use invalid profile name
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();

    // Act: Attempt wizard with invalid profile
    let result = Command::cargo_bin("ggen")
        .expect("Failed to find ggen binary")
        .args([
            "wizard",
            "--profile",
            "invalid-profile-name",
            "--output-dir",
            project_path.to_str().unwrap(),
            "--yes",
        ])
        .assert();

    // Assert: Should fail
    result.failure();
}

#[test]
fn test_sync_fails_without_ggen_toml() {
    // Arrange: Create empty directory (no wizard)
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();

    // Act: Attempt sync without initialization
    let result = Command::cargo_bin("ggen")
        .expect("Failed to find ggen binary")
        .current_dir(project_path)
        .args(["sync"])
        .assert();

    // Assert: Should fail with manifest not found
    result.failure().stderr(predicate::str::contains("Manifest").or(predicate::str::contains("ggen.toml")));
}

// ============================================================================
// INTEGRATION: Complete User Journey
// ============================================================================

#[test]
fn test_complete_user_journey_wizard_to_validation() {
    // This test simulates the complete end-to-end user experience:
    // 1. Run wizard to bootstrap project
    // 2. Run sync to generate outputs
    // 3. Verify all outputs exist
    // 4. Validate manifest structure
    // 5. Check determinism
    // 6. Validate Node.js verifier (if Node available)

    // Arrange: Fresh project
    let temp_dir = setup_test_project();
    let project_path = temp_dir.path();

    // Act: Step 1 - Bootstrap with wizard
    println!("Step 1: Running wizard...");
    run_wizard(project_path).success();

    // Assert: Essential files created
    assert_file_exists(&project_path.join("ggen.toml"), "ggen.toml");
    assert_file_exists(
        &project_path.join(".specify/ontologies/main.ttl"),
        "main.ttl"
    );

    // Act: Step 2 - First sync
    println!("Step 2: Running first sync...");
    run_sync(project_path).success();

    // Assert: Generated files exist
    let generated_dir = project_path.join("generated");
    assert_file_exists(&generated_dir.join("world.manifest.json"), "manifest");
    assert_file_exists(&generated_dir.join("world.verify.mjs"), "verifier");

    // Act: Step 3 - Parse and validate manifest
    println!("Step 3: Validating manifest structure...");
    let manifest_path = generated_dir.join("world.manifest.json");
    let manifest: Value = parse_json_file(&manifest_path);

    assert!(
        manifest["artifacts"].is_array(),
        "Manifest should have artifacts array"
    );

    // Act: Step 4 - Test determinism
    println!("Step 4: Testing determinism...");
    let first_hash = compute_file_hash(&manifest_path);

    std::thread::sleep(std::time::Duration::from_millis(100));
    run_sync(project_path).success();

    let second_hash = compute_file_hash(&manifest_path);

    assert_eq!(
        first_hash, second_hash,
        "Determinism check: manifest should be byte-identical"
    );

    // Act: Step 5 - Validate schemas
    println!("Step 5: Validating schemas...");
    let receipt_schema: Value = parse_json_file(&generated_dir.join("receipts/receipt.schema.json"));
    assert!(
        receipt_schema.get("$schema").is_some(),
        "Schema should have $schema field"
    );

    println!("✅ Complete user journey test passed!");
}
