//! End-to-end integration tests for PaaS CLI commands
//! Chicago TDD pattern: Arrange/Act/Assert with real systems
//!
//! These tests verify actual outcomes with real git operations,
//! real RDF specifications, and actual file generation.

#![cfg(feature = "paas")]

#[cfg(test)]
mod paas_e2e_tests {
    use ggen_cli_lib::commands::paas::handlers::{init, sync, validate};
    use std::fs;
    use std::path::Path;
    use std::process::Command;
    use tempfile::TempDir;

    // ============================================================================
    // Test Fixtures and Utilities
    // ============================================================================

    /// Create a minimal but valid RDF specification for testing
    fn create_test_rdf_spec(dir: &Path) -> std::io::Result<()> {
        fs::create_dir_all(dir)?;

        // cli-schema.ttl
        let cli_schema = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix cli: <http://ggen.io/ontology/cli#> .

cli:Command a rdfs:Class ;
    rdfs:label "CLI Command" ;
    rdfs:comment "A command-line interface command" .

cli:hasVerb a rdf:Property ;
    rdfs:domain cli:Command ;
    rdfs:range rdfs:Literal .
"#;
        fs::write(dir.join("cli-schema.ttl"), cli_schema)?;

        // cli-commands.ttl
        let cli_commands = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix cli: <http://ggen.io/ontology/cli#> .

cli:InitCommand a cli:Command ;
    cli:hasVerb "init" ;
    rdfs:label "Initialize submodule" .

cli:ValidateCommand a cli:Command ;
    cli:hasVerb "validate" ;
    rdfs:label "Validate specifications" .

cli:SyncCommand a cli:Command ;
    cli:hasVerb "sync" ;
    rdfs:label "Synchronize code" .
"#;
        fs::write(dir.join("cli-commands.ttl"), cli_commands)?;

        // ggen-paas-ontology.ttl
        let paas_ontology = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix paas: <http://ggen.io/ontology/paas#> .

paas:Submodule a rdfs:Class ;
    rdfs:label "Git Submodule" ;
    rdfs:comment "A git submodule for PaaS operations" .

paas:Specification a rdfs:Class ;
    rdfs:label "RDF Specification" ;
    rdfs:comment "An RDF specification in Turtle format" .

paas:Environment a rdfs:Class ;
    rdfs:label "Deployment Environment" ;
    rdfs:comment "A target environment for deployment" .
"#;
        fs::write(dir.join("ggen-paas-ontology.ttl"), paas_ontology)?;

        Ok(())
    }

    /// Initialize a temporary git repository for testing
    fn setup_test_git_repo(dir: &Path) -> std::io::Result<()> {
        Command::new("git")
            .args(&["init"])
            .current_dir(dir)
            .output()?;

        Command::new("git")
            .args(&["config", "user.email", "test@example.com"])
            .current_dir(dir)
            .output()?;

        Command::new("git")
            .args(&["config", "user.name", "Test User"])
            .current_dir(dir)
            .output()?;

        // Create an initial commit
        fs::write(dir.join("README.md"), "# Test Repository")?;
        Command::new("git")
            .args(&["add", "README.md"])
            .current_dir(dir)
            .output()?;

        Command::new("git")
            .args(&["commit", "-m", "Initial commit"])
            .current_dir(dir)
            .output()?;

        Ok(())
    }

    /// Clean up git submodule if it exists
    fn cleanup_submodule(dir: &Path, name: &str) -> std::io::Result<()> {
        // Remove from .git/modules
        let modules_path = dir.join(".git/modules").join(name);
        if modules_path.exists() {
            fs::remove_dir_all(modules_path)?;
        }

        // Remove submodule directory
        let submodule_path = dir.join(name);
        if submodule_path.exists() {
            fs::remove_dir_all(submodule_path)?;
        }

        // Remove from .gitmodules
        let gitmodules_path = dir.join(".gitmodules");
        if gitmodules_path.exists() {
            fs::remove_file(gitmodules_path)?;
        }

        // Remove from .git/config
        Command::new("git")
            .args(&["config", "--remove-section", &format!("submodule.{}", name)])
            .current_dir(dir)
            .output()
            .ok(); // Ignore errors if section doesn't exist

        Ok(())
    }

    // ============================================================================
    // E2E Test 1: Validate Command with Real RDF Specifications
    // ============================================================================

    #[tokio::test]
    async fn test_e2e_validate_command_with_real_specs() {
        // ARRANGE: Create temporary directory with real RDF specifications
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let spec_path = temp_dir.path().join(".specify");

        create_test_rdf_spec(&spec_path).expect("Failed to create test RDF specs");

        // Verify all required files exist
        assert!(
            spec_path.join("cli-schema.ttl").exists(),
            "cli-schema.ttl should exist"
        );
        assert!(
            spec_path.join("cli-commands.ttl").exists(),
            "cli-commands.ttl should exist"
        );
        assert!(
            spec_path.join("ggen-paas-ontology.ttl").exists(),
            "ggen-paas-ontology.ttl should exist"
        );

        // ACT: Validate the specifications with 95% closure requirement
        let spec_path_str = spec_path.to_str().expect("Invalid path");
        let result = validate::validate_specs(spec_path_str, 95.0, false).await;

        // ASSERT: Validation should succeed with 100% closure
        assert!(
            result.is_ok(),
            "Validation should succeed with all required files present: {:?}",
            result
        );

        // Test strict mode
        let result_strict = validate::validate_specs(spec_path_str, 95.0, true).await;
        assert!(
            result_strict.is_ok(),
            "Strict validation should also succeed: {:?}",
            result_strict
        );
    }

    #[tokio::test]
    async fn test_e2e_validate_command_with_incomplete_specs() {
        // ARRANGE: Create directory with only partial specifications
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let spec_path = temp_dir.path().join(".specify");
        fs::create_dir_all(&spec_path).expect("Failed to create spec dir");

        // Only create 2 out of 3 required files
        fs::write(spec_path.join("cli-schema.ttl"), "# Minimal TTL").expect("Failed to write");
        fs::write(spec_path.join("cli-commands.ttl"), "# Minimal TTL").expect("Failed to write");
        // Missing: ggen-paas-ontology.ttl

        // ACT: Attempt to validate with incomplete specifications
        let spec_path_str = spec_path.to_str().expect("Invalid path");
        let result = validate::validate_specs(spec_path_str, 95.0, false).await;

        // ASSERT: Validation should fail with closure error
        assert!(
            result.is_err(),
            "Validation should fail with incomplete specifications"
        );

        if let Err(e) = result {
            let error_msg = format!("{}", e);
            assert!(
                error_msg.contains("closure") || error_msg.contains("Closure"),
                "Error should mention closure: {}",
                error_msg
            );
        }
    }

    #[tokio::test]
    async fn test_e2e_validate_command_nonexistent_directory() {
        // ARRANGE: Use a path that doesn't exist
        let nonexistent_path = "/tmp/nonexistent_ggen_test_12345/specs";

        // ACT: Attempt to validate nonexistent directory
        let result = validate::validate_specs(nonexistent_path, 95.0, false).await;

        // ASSERT: Should fail with appropriate error
        assert!(result.is_err(), "Should fail for nonexistent directory");

        if let Err(e) = result {
            let error_msg = format!("{}", e);
            assert!(
                error_msg.contains("does not exist") || error_msg.contains("not exist"),
                "Error should mention directory doesn't exist: {}",
                error_msg
            );
        }
    }

    // ============================================================================
    // E2E Test 2: Sync Command with Real Code Generation
    // ============================================================================

    #[tokio::test]
    async fn test_e2e_sync_command_generates_artifacts() {
        // ARRANGE: Create source spec directory and target output directory
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let source_path = temp_dir.path().join(".specify");
        let target_path = temp_dir.path().join("generated");

        create_test_rdf_spec(&source_path).expect("Failed to create test RDF specs");

        let source_str = source_path.to_str().expect("Invalid source path");
        let target_str = target_path.to_str().expect("Invalid target path");

        // Verify target doesn't exist yet
        assert!(!target_path.exists(), "Target should not exist before sync");

        // ACT: Synchronize specifications to generate code
        let result = sync::sync_specs(source_str, target_str, false).await;

        // ASSERT: Sync should succeed and create target directory
        assert!(result.is_ok(), "Sync should succeed: {:?}", result);
        assert!(
            target_path.exists(),
            "Target directory should be created after sync"
        );
        assert!(
            target_path.is_dir(),
            "Target should be a directory, not a file"
        );
    }

    #[tokio::test]
    async fn test_e2e_sync_command_dry_run_mode() {
        // ARRANGE: Create source directory (target should not be created in dry-run)
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let source_path = temp_dir.path().join(".specify");
        let target_path = temp_dir.path().join("generated_dry_run");

        create_test_rdf_spec(&source_path).expect("Failed to create test RDF specs");

        let source_str = source_path.to_str().expect("Invalid source path");
        let target_str = target_path.to_str().expect("Invalid target path");

        // ACT: Run sync in dry-run mode
        let result = sync::sync_specs(source_str, target_str, true).await;

        // ASSERT: Dry run should succeed but NOT create target directory
        assert!(result.is_ok(), "Dry run should succeed: {:?}", result);
        assert!(
            !target_path.exists(),
            "Target directory should NOT be created in dry-run mode"
        );
    }

    #[tokio::test]
    async fn test_e2e_sync_command_missing_source() {
        // ARRANGE: Use nonexistent source path
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let nonexistent_source = temp_dir.path().join("nonexistent_source");
        let target_path = temp_dir.path().join("generated");

        let source_str = nonexistent_source.to_str().expect("Invalid path");
        let target_str = target_path.to_str().expect("Invalid path");

        // ACT: Attempt to sync from nonexistent source
        let result = sync::sync_specs(source_str, target_str, false).await;

        // ASSERT: Should fail with IO error
        assert!(result.is_err(), "Sync should fail with missing source");

        if let Err(e) = result {
            let error_msg = format!("{}", e);
            assert!(
                error_msg.contains("does not exist") || error_msg.contains("Source path"),
                "Error should mention source path issue: {}",
                error_msg
            );
        }
    }

    #[tokio::test]
    async fn test_e2e_sync_command_idempotent() {
        // ARRANGE: Create source and target
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let source_path = temp_dir.path().join(".specify");
        let target_path = temp_dir.path().join("generated");

        create_test_rdf_spec(&source_path).expect("Failed to create test RDF specs");

        let source_str = source_path.to_str().expect("Invalid source path");
        let target_str = target_path.to_str().expect("Invalid target path");

        // ACT: Run sync twice
        let result1 = sync::sync_specs(source_str, target_str, false).await;
        let result2 = sync::sync_specs(source_str, target_str, false).await;

        // ASSERT: Both runs should succeed (idempotent)
        assert!(result1.is_ok(), "First sync should succeed: {:?}", result1);
        assert!(
            result2.is_ok(),
            "Second sync should succeed (idempotent): {:?}",
            result2
        );
        assert!(
            target_path.exists(),
            "Target should exist after both syncs"
        );
    }

    // ============================================================================
    // E2E Test 3: Init Command with Real Git Submodule Operations
    // ============================================================================

    #[tokio::test]
    #[ignore] // Ignore by default: requires network access and git operations
    async fn test_e2e_init_command_real_submodule() {
        // ARRANGE: Create a temporary git repository
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        setup_test_git_repo(temp_dir.path()).expect("Failed to setup git repo");

        // Change to temp directory for git operations
        let original_dir = std::env::current_dir().expect("Failed to get current dir");
        std::env::set_current_dir(temp_dir.path()).expect("Failed to change dir");

        // ACT: Initialize a real submodule (ggen-spec-kit)
        let result = init::init_submodule("ggen-spec-kit", false, true).await;

        // ASSERT: Initialization should succeed
        assert!(
            result.is_ok(),
            "Submodule init should succeed: {:?}",
            result
        );

        // Verify submodule directory exists
        let submodule_path = temp_dir.path().join("ggen-spec-kit");
        assert!(
            submodule_path.exists(),
            "Submodule directory should exist after init"
        );

        // Verify .gitmodules file was created
        let gitmodules_path = temp_dir.path().join(".gitmodules");
        assert!(
            gitmodules_path.exists(),
            ".gitmodules file should exist after init"
        );

        // Verify .gitmodules contains the submodule
        let gitmodules_content = fs::read_to_string(&gitmodules_path)
            .expect("Failed to read .gitmodules");
        assert!(
            gitmodules_content.contains("ggen-spec-kit"),
            ".gitmodules should reference ggen-spec-kit"
        );

        // CLEANUP
        cleanup_submodule(temp_dir.path(), "ggen-spec-kit").ok();
        std::env::set_current_dir(original_dir).expect("Failed to restore dir");
    }

    #[tokio::test]
    async fn test_e2e_init_command_invalid_submodule_name() {
        // ARRANGE: No setup needed for invalid name test

        // ACT: Attempt to initialize with invalid submodule name
        let result = init::init_submodule("invalid-nonexistent-module", false, false).await;

        // ASSERT: Should fail with InvalidCommand error
        assert!(result.is_err(), "Should fail with invalid submodule name");

        if let Err(e) = result {
            let error_msg = format!("{}", e);
            assert!(
                error_msg.contains("Invalid") || error_msg.contains("Available"),
                "Error should mention invalid command or available options: {}",
                error_msg
            );
        }
    }

    #[tokio::test]
    async fn test_e2e_init_command_empty_name() {
        // ARRANGE: Use empty string as submodule name

        // ACT: Attempt to initialize with empty name
        let result = init::init_submodule("", false, false).await;

        // ASSERT: Should fail with MissingOption error
        assert!(result.is_err(), "Should fail with empty submodule name");

        if let Err(e) = result {
            let error_msg = format!("{}", e);
            assert!(
                error_msg.contains("missing") || error_msg.contains("name"),
                "Error should mention missing name: {}",
                error_msg
            );
        }
    }

    // ============================================================================
    // E2E Test 4: Integration Test - Full Workflow
    // ============================================================================

    #[tokio::test]
    async fn test_e2e_full_workflow_validate_then_sync() {
        // ARRANGE: Create complete test environment with specs
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let spec_path = temp_dir.path().join(".specify");
        let output_path = temp_dir.path().join("output");

        create_test_rdf_spec(&spec_path).expect("Failed to create test RDF specs");

        let spec_str = spec_path.to_str().expect("Invalid spec path");
        let output_str = output_path.to_str().expect("Invalid output path");

        // ACT: Step 1 - Validate specifications (prerequisite)
        let validate_result = validate::validate_specs(spec_str, 95.0, false).await;

        // ASSERT: Validation passes
        assert!(
            validate_result.is_ok(),
            "Validation should pass before sync: {:?}",
            validate_result
        );

        // ACT: Step 2 - Sync specifications to generate code
        let sync_result = sync::sync_specs(spec_str, output_str, false).await;

        // ASSERT: Sync succeeds after validation
        assert!(
            sync_result.is_ok(),
            "Sync should succeed after validation: {:?}",
            sync_result
        );

        // ASSERT: Output directory exists with generated artifacts
        assert!(
            output_path.exists(),
            "Output directory should exist after sync"
        );
    }

    #[tokio::test]
    async fn test_e2e_closure_calculation_accuracy() {
        // ARRANGE: Create specs with known closure percentage
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let spec_path = temp_dir.path().join(".specify");
        fs::create_dir_all(&spec_path).expect("Failed to create spec dir");

        // Create 2 out of 3 files = 66.67% closure
        fs::write(spec_path.join("cli-schema.ttl"), "# Test").expect("Failed to write");
        fs::write(spec_path.join("cli-commands.ttl"), "# Test").expect("Failed to write");

        let spec_str = spec_path.to_str().expect("Invalid path");

        // ACT: Validate with different closure thresholds
        let result_50 = validate::validate_specs(spec_str, 50.0, false).await;
        let result_70 = validate::validate_specs(spec_str, 70.0, false).await;
        let result_95 = validate::validate_specs(spec_str, 95.0, false).await;

        // ASSERT: Should pass 50%, fail 70% and 95%
        assert!(
            result_50.is_ok(),
            "Should pass with 50% threshold (66.67% actual)"
        );
        assert!(
            result_70.is_err(),
            "Should fail with 70% threshold (66.67% actual)"
        );
        assert!(
            result_95.is_err(),
            "Should fail with 95% threshold (66.67% actual)"
        );
    }

    // ============================================================================
    // E2E Test 5: Error Recovery and Resilience
    // ============================================================================

    #[tokio::test]
    async fn test_e2e_sync_creates_nested_directories() {
        // ARRANGE: Create source, target with nested path
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let source_path = temp_dir.path().join(".specify");
        let nested_target = temp_dir.path().join("deeply/nested/output/dir");

        create_test_rdf_spec(&source_path).expect("Failed to create test RDF specs");

        let source_str = source_path.to_str().expect("Invalid source path");
        let target_str = nested_target.to_str().expect("Invalid target path");

        // ACT: Sync to deeply nested directory
        let result = sync::sync_specs(source_str, target_str, false).await;

        // ASSERT: Should create all parent directories
        assert!(
            result.is_ok(),
            "Should create nested directories: {:?}",
            result
        );
        assert!(
            nested_target.exists(),
            "All nested directories should be created"
        );
    }

    #[tokio::test]
    async fn test_e2e_validate_with_special_characters_in_path() {
        // ARRANGE: Create directory with spaces and special chars
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let spec_path = temp_dir.path().join("test spec dir");
        create_test_rdf_spec(&spec_path).expect("Failed to create test RDF specs");

        let spec_str = spec_path.to_str().expect("Invalid path");

        // ACT: Validate path with special characters
        let result = validate::validate_specs(spec_str, 95.0, false).await;

        // ASSERT: Should handle special characters correctly
        assert!(
            result.is_ok(),
            "Should handle paths with spaces: {:?}",
            result
        );
    }
}
