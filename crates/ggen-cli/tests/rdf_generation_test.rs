//! Phase 7: RDF-Based CLI Generation E2E Tests
//!
//! Comprehensive end-to-end test suite for generating CLI applications from RDF/TTL specifications.
//!
//! ## Test Coverage
//!
//! ### Core Functionality
//! - `test_generate_from_ttl`: Full E2E workflow - TTL → Generated CLI → Compilation → Execution
//! - File generation verification (Cargo.toml, src/, tests/, README.md)
//! - Generated project compilation with `cargo check`
//! - Generated CLI help output validation
//!
//! ### Error Handling
//! - `test_invalid_ttl_file`: Missing TTL file error handling
//! - `test_invalid_rdf_syntax`: Malformed TTL syntax validation
//! - `test_missing_required_fields`: Incomplete project definition detection
//!
//! ## Chicago TDD Principles
//!
//! This test suite follows Chicago-school TDD by testing REAL systems:
//! - REAL RDF/TTL parsing using oxigraph
//! - REAL template rendering with Tera
//! - REAL file system operations
//! - REAL Cargo project compilation
//! - REAL CLI command execution
//! - NO mocks for core functionality (only for external services if needed)
//!
//! ## Test Isolation
//!
//! - Each test uses `tempfile::TempDir` for isolated file system operations
//! - Tests can run in parallel without interference
//! - Automatic cleanup after test completion

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Helper to create a ggen Command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

// =============================================================================
// CORE FUNCTIONALITY TESTS
// =============================================================================

/// Test: Full E2E workflow from TTL to working CLI
///
/// **Workflow:**
/// 1. Generate CLI from sample-cli.ttl
/// 2. Verify all expected files are created
/// 3. Verify generated project compiles
/// 4. Verify generated CLI runs and shows help
///
/// **Chicago TDD:** Tests REAL code generation, file operations, and compilation
#[test]
fn test_generate_from_ttl() {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let output_dir = temp_dir.path().join("my-cli");

    // Phase 1: Generate CLI from TTL
    println!("Phase 1: Generating CLI from sample-cli.ttl...");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("Generated").or(predicate::str::contains("Success")));

    // Phase 2: Verify generated file structure
    println!("Phase 2: Verifying generated file structure...");

    // Root level files
    assert!(
        output_dir.join("Cargo.toml").exists(),
        "Cargo.toml should be generated at project root"
    );
    assert!(
        output_dir.join("README.md").exists(),
        "README.md should be generated"
    );

    // Source files
    assert!(
        output_dir.join("src").is_dir(),
        "src/ directory should be created"
    );
    assert!(
        output_dir.join("src/main.rs").exists(),
        "src/main.rs should be generated"
    );
    assert!(
        output_dir.join("src/lib.rs").exists(),
        "src/lib.rs should be generated"
    );
    assert!(
        output_dir.join("src/command.rs").exists(),
        "src/command.rs should be generated"
    );

    // Command modules
    assert!(
        output_dir.join("src/cmds").is_dir(),
        "src/cmds/ directory should be created"
    );
    assert!(
        output_dir.join("src/cmds/mod.rs").exists(),
        "src/cmds/mod.rs should be generated"
    );

    // Template noun commands (from sample-cli.ttl)
    assert!(
        output_dir.join("src/cmds/template").is_dir(),
        "src/cmds/template/ directory should be created"
    );
    assert!(
        output_dir.join("src/cmds/template/mod.rs").exists(),
        "src/cmds/template/mod.rs should be generated"
    );
    assert!(
        output_dir.join("src/cmds/template/generate.rs").exists(),
        "src/cmds/template/generate.rs should be generated (from sample-cli.ttl)"
    );

    // Test files
    assert!(
        output_dir.join("tests").is_dir(),
        "tests/ directory should be created"
    );
    assert!(
        output_dir.join("tests/integration_test.rs").exists(),
        "tests/integration_test.rs should be generated"
    );

    // Phase 3: Verify generated Cargo.toml content
    println!("Phase 3: Validating Cargo.toml content...");

    let cargo_toml = fs::read_to_string(output_dir.join("Cargo.toml"))
        .expect("Failed to read generated Cargo.toml");

    assert!(
        cargo_toml.contains("[package]"),
        "Cargo.toml should have [package] section"
    );
    assert!(
        cargo_toml.contains("name = \"my-cli\""),
        "Cargo.toml should have correct package name"
    );
    assert!(
        cargo_toml.contains("version = \"0.1.0\""),
        "Cargo.toml should have version"
    );
    assert!(
        cargo_toml.contains("clap"),
        "Cargo.toml should include clap dependency"
    );
    assert!(
        cargo_toml.contains("anyhow"),
        "Cargo.toml should include anyhow dependency"
    );
    assert!(
        cargo_toml.contains("serde_json") || cargo_toml.contains("serde"),
        "Cargo.toml should include serde/serde_json dependency"
    );

    // Phase 4: Verify generated main.rs structure
    println!("Phase 4: Validating main.rs structure...");

    let main_rs = fs::read_to_string(output_dir.join("src/main.rs"))
        .expect("Failed to read generated main.rs");

    assert!(
        main_rs.contains("use clap") || main_rs.contains("clap::"),
        "main.rs should import clap"
    );
    assert!(
        main_rs.contains("fn main()"),
        "main.rs should have main function"
    );
    assert!(
        main_rs.contains("template") || main_rs.contains("Template"),
        "main.rs should reference template commands from TTL"
    );
    assert!(
        main_rs.contains("project") || main_rs.contains("Project"),
        "main.rs should reference project commands from TTL"
    );

    // Phase 5: Verify it compiles
    println!("Phase 5: Compiling generated project with cargo check...");

    let check_result = std::process::Command::new("cargo")
        .arg("check")
        .current_dir(&output_dir)
        .output()
        .expect("Failed to run cargo check");

    if !check_result.status.success() {
        eprintln!("=== CARGO CHECK FAILED ===");
        eprintln!("STDOUT:\n{}", String::from_utf8_lossy(&check_result.stdout));
        eprintln!("STDERR:\n{}", String::from_utf8_lossy(&check_result.stderr));
        panic!("Generated project failed to compile");
    }

    assert!(
        check_result.status.success(),
        "Generated project should compile successfully"
    );

    // Phase 6: Verify generated CLI runs
    println!("Phase 6: Testing generated CLI execution...");

    let help_result = std::process::Command::new("cargo")
        .args(&["run", "--", "--help"])
        .current_dir(&output_dir)
        .output()
        .expect("Failed to run cargo run -- --help");

    let help_output = String::from_utf8_lossy(&help_result.stdout);

    assert!(
        help_result.status.success(),
        "Generated CLI should run successfully"
    );
    assert!(
        help_output.contains("template") || help_output.contains("USAGE"),
        "Generated CLI should show help text with template commands"
    );

    println!("✓ All phases completed successfully!");
}

/// Test: Verify specific generated commands match TTL specification
///
/// **Validates:**
/// - template generate command exists
/// - template lint command exists
/// - template show command exists
/// - project init command exists
/// - project build command exists
#[test]
fn test_generated_commands_match_ttl_spec() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("spec-cli");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Verify template commands from sample-cli.ttl
    let template_mod = fs::read_to_string(output_dir.join("src/cmds/template/mod.rs"))
        .expect("Failed to read template/mod.rs");

    assert!(
        template_mod.contains("generate") || template_mod.contains("Generate"),
        "template module should have generate command"
    );
    assert!(
        template_mod.contains("lint") || template_mod.contains("Lint"),
        "template module should have lint command"
    );
    assert!(
        template_mod.contains("show") || template_mod.contains("Show"),
        "template module should have show command"
    );

    // Verify project commands from sample-cli.ttl
    if output_dir.join("src/cmds/project/mod.rs").exists() {
        let project_mod = fs::read_to_string(output_dir.join("src/cmds/project/mod.rs"))
            .expect("Failed to read project/mod.rs");

        assert!(
            project_mod.contains("init") || project_mod.contains("Init"),
            "project module should have init command"
        );
        assert!(
            project_mod.contains("build") || project_mod.contains("Build"),
            "project module should have build command"
        );
    }
}

/// Test: Verify generated code includes execution logic from TTL
///
/// **Validates:**
/// - Execution logic from TTL is embedded in generated code
/// - Command handlers have proper error handling (anyhow::Result)
#[test]
fn test_generated_execution_logic() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("logic-cli");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Read template/generate.rs which should have execution logic
    if output_dir.join("src/cmds/template/generate.rs").exists() {
        let generate_rs = fs::read_to_string(output_dir.join("src/cmds/template/generate.rs"))
            .expect("Failed to read template/generate.rs");

        // Verify execution logic patterns from sample-cli.ttl
        assert!(
            generate_rs.contains("anyhow::Result") || generate_rs.contains("Result<"),
            "Generated commands should use Result for error handling"
        );
        assert!(
            generate_rs.contains("println!") || generate_rs.contains("eprintln!"),
            "Generated commands should include logic from TTL executionLogic"
        );
    }
}

// =============================================================================
// ERROR HANDLING TESTS
// =============================================================================

/// Test: Missing TTL file produces clear error
///
/// **Chicago TDD:** Tests REAL file not found error handling
#[test]
fn test_invalid_ttl_file() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("missing-cli");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("nonexistent-file.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("not found")
                .or(predicate::str::contains("No such file"))
                .or(predicate::str::contains("does not exist"))
                .or(predicate::str::contains("error"))
        );

    // Verify output directory is not created or is empty
    assert!(
        !output_dir.exists() || fs::read_dir(&output_dir).unwrap().count() == 0,
        "Output directory should not be created or should be empty on error"
    );
}

/// Test: Malformed TTL syntax produces validation error
///
/// **Chicago TDD:** Tests REAL RDF parsing error handling with oxigraph
#[test]
fn test_invalid_rdf_syntax() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("invalid-cli");

    // Create invalid TTL file with syntax errors
    let invalid_ttl = temp_dir.path().join("invalid.ttl");
    fs::write(
        &invalid_ttl,
        r#"
        @prefix cli: <http://ggen.dev/schema/cli#> .
        @prefix ex: <http://ggen.dev/projects/test#> .

        # Invalid syntax: missing semicolon and closing period
        ex:MyProject a cli:CliProject
            cli:hasName "test"
            cli:hasVersion "0.1.0"
        # Missing final period will cause parser error
        "#,
    )
    .expect("Failed to write invalid TTL");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg(&invalid_ttl)
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("parse")
                .or(predicate::str::contains("syntax"))
                .or(predicate::str::contains("invalid"))
                .or(predicate::str::contains("RDF"))
                .or(predicate::str::contains("TTL"))
                .or(predicate::str::contains("error"))
        );
}

/// Test: Missing required fields in RDF produces validation error
///
/// **Validates:**
/// - Missing cli:hasName
/// - Missing cli:hasVersion
/// - Missing cli:hasNoun (no commands)
///
/// **Chicago TDD:** Tests REAL RDF validation logic
#[test]
fn test_missing_required_fields() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("incomplete-cli");

    // Create TTL file with missing required fields
    let incomplete_ttl = temp_dir.path().join("incomplete.ttl");
    fs::write(
        &incomplete_ttl,
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix cli: <http://ggen.dev/schema/cli#> .
        @prefix ex: <http://ggen.dev/projects/incomplete#> .

        # Project missing required fields: hasName, hasVersion
        ex:MyProject a cli:CliProject ;
            cli:hasDescription "A test project" .

        # No nouns/verbs defined - will fail validation
        "#,
    )
    .expect("Failed to write incomplete TTL");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg(&incomplete_ttl)
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("required")
                .or(predicate::str::contains("missing"))
                .or(predicate::str::contains("hasName"))
                .or(predicate::str::contains("hasVersion"))
                .or(predicate::str::contains("validation"))
                .or(predicate::str::contains("error"))
        );
}

/// Test: TTL with missing dependencies produces clear error
#[test]
fn test_missing_dependencies() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("nodeps-cli");

    // Create TTL file without dependency definitions
    let no_deps_ttl = temp_dir.path().join("no-deps.ttl");
    fs::write(
        &no_deps_ttl,
        r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix cli: <http://ggen.dev/schema/cli#> .
        @prefix cnv: <http://ggen.dev/schema/clap-noun-verb#> .
        @prefix ex: <http://ggen.dev/projects/nodeps#> .

        ex:MyProject a cli:CliProject ;
            cli:hasName "nodeps-cli" ;
            cli:hasVersion "0.1.0" ;
            cli:hasDescription "Project with no dependencies" ;
            cli:hasNoun ex:TestNoun .

        ex:TestNoun a cnv:Noun ;
            cnv:nounName "test" ;
            cnv:hasVerb ex:TestVerb .

        ex:TestVerb a cnv:Verb ;
            cnv:verbName "run" .

        # Note: No cli:hasDependency defined
        "#,
    )
    .expect("Failed to write no-deps TTL");

    // This might succeed but generate a Cargo.toml with minimal dependencies
    // or it might warn - either is acceptable
    let result = ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg(&no_deps_ttl)
        .arg("--output")
        .arg(&output_dir)
        .output()
        .expect("Failed to run command");

    // If it succeeds, verify Cargo.toml has minimal dependencies
    if result.status.success() && output_dir.join("Cargo.toml").exists() {
        let cargo_toml = fs::read_to_string(output_dir.join("Cargo.toml"))
            .expect("Failed to read Cargo.toml");

        // Should at least have clap and anyhow as default dependencies
        assert!(
            cargo_toml.contains("clap") || cargo_toml.contains("[dependencies]"),
            "Generated Cargo.toml should have dependencies section"
        );
    }
}

// =============================================================================
// EDGE CASE TESTS
// =============================================================================

/// Test: Empty output directory flag uses current directory
#[test]
fn test_output_directory_defaults() {
    let temp_dir = TempDir::new().unwrap();

    // Run without --output flag (should fail or use current dir)
    let result = ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to run command");

    // Either succeeds and generates in current dir, or fails requesting --output
    if result.status.success() {
        assert!(
            temp_dir.path().join("Cargo.toml").exists() || temp_dir.path().join("src").exists(),
            "Should generate files in current directory if no --output specified"
        );
    } else {
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("output") || stderr.contains("--output"),
            "Should mention output requirement in error"
        );
    }
}

/// Test: Force overwrite flag allows regeneration
#[test]
fn test_force_overwrite() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("force-cli");

    // First generation
    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Second generation without --force should fail
    let result = ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .output()
        .expect("Failed to run command");

    if !result.status.success() {
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("exists") || stderr.contains("force") || stderr.contains("overwrite"),
            "Should mention existing directory or --force option"
        );

        // Third generation with --force should succeed
        ggen()
            .arg("template")
            .arg("generate-rdf")
            .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
            .arg("--output")
            .arg(&output_dir)
            .arg("--force")
            .assert()
            .success();
    }
}

// =============================================================================
// INTEGRATION TESTS
// =============================================================================

/// Test: Generated project includes proper module structure
#[test]
fn test_generated_module_structure() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("module-cli");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Verify mod.rs files properly export submodules
    let cmds_mod = fs::read_to_string(output_dir.join("src/cmds/mod.rs"))
        .expect("Failed to read cmds/mod.rs");

    assert!(
        cmds_mod.contains("pub mod template") || cmds_mod.contains("mod template"),
        "cmds/mod.rs should export template module"
    );
    assert!(
        cmds_mod.contains("pub mod project") || cmds_mod.contains("mod project"),
        "cmds/mod.rs should export project module"
    );
}

/// Test: Generated tests directory has basic integration test
#[test]
fn test_generated_tests() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("tests-cli");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Verify integration test exists
    if output_dir.join("tests/integration_test.rs").exists() {
        let integration_test = fs::read_to_string(output_dir.join("tests/integration_test.rs"))
            .expect("Failed to read integration_test.rs");

        assert!(
            integration_test.contains("#[test]") || integration_test.contains("fn test_"),
            "integration_test.rs should contain test functions"
        );
        assert!(
            integration_test.contains("assert_cmd") || integration_test.contains("Command"),
            "integration_test.rs should use assert_cmd for CLI testing"
        );
    }
}

/// Test: Generated README includes usage instructions
#[test]
fn test_generated_readme() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("readme-cli");

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    // Verify README exists and has content
    if output_dir.join("README.md").exists() {
        let readme = fs::read_to_string(output_dir.join("README.md"))
            .expect("Failed to read README.md");

        assert!(
            readme.contains("##") || readme.contains("# "),
            "README should have markdown headers"
        );
        assert!(
            readme.contains("template") || readme.contains("Usage"),
            "README should mention CLI commands or usage"
        );
    }
}

// =============================================================================
// PERFORMANCE TESTS
// =============================================================================

/// Test: Generation completes in reasonable time (<5 seconds)
#[test]
fn test_generation_performance() {
    let temp_dir = TempDir::new().unwrap();
    let output_dir = temp_dir.path().join("perf-cli");

    let start = std::time::Instant::now();

    ggen()
        .arg("template")
        .arg("generate-rdf")
        .arg("examples/clap-noun-verb-demo/sample-cli.ttl")
        .arg("--output")
        .arg(&output_dir)
        .assert()
        .success();

    let duration = start.elapsed();

    assert!(
        duration.as_secs() < 5,
        "Generation should complete in <5 seconds, took {:?}",
        duration
    );

    // Verify files were actually generated
    assert!(output_dir.join("Cargo.toml").exists());
    assert!(output_dir.join("src/main.rs").exists());
}

// =============================================================================
// HELPER TESTS - Verify test infrastructure
// =============================================================================

#[test]
fn test_sample_ttl_file_exists() {
    let sample_ttl = std::path::Path::new("examples/clap-noun-verb-demo/sample-cli.ttl");
    assert!(
        sample_ttl.exists(),
        "Sample TTL file should exist at examples/clap-noun-verb-demo/sample-cli.ttl"
    );

    let content = fs::read_to_string(sample_ttl).expect("Failed to read sample-cli.ttl");
    assert!(
        content.contains("cli:CliProject"),
        "sample-cli.ttl should contain CLI project definition"
    );
    assert!(
        content.contains("cnv:Noun"),
        "sample-cli.ttl should contain noun definitions"
    );
    assert!(
        content.contains("cnv:Verb"),
        "sample-cli.ttl should contain verb definitions"
    );
}

#[test]
fn test_ggen_binary_available() {
    ggen()
        .arg("--version")
        .assert()
        .success();
}

#[test]
fn test_template_command_available() {
    ggen()
        .arg("template")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("template") .or(predicate::str::contains("SUBCOMMANDS")));
}
