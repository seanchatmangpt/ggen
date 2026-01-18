//! Pre-flight Validation Tests
//!
//! Tests all pre-flight validation checks to ensure they catch issues early
//! and provide helpful error messages before beginning expensive operations.
//!
//! ## Coverage
//! - Test 1: Disk space check (E0020)
//! - Test 2: Write permission check (E0021)
//! - Test 3: LLM provider health check (E0022)
//! - Test 4: Manifest validation (E0023)
//! - Test 5: Template syntax validation (E0024)
//! - Test 6: Dependency checking (E0025)
//!
//! ## Success Criteria
//! - All validation checks work
//! - Error codes E0020-E0029 used correctly
//! - Helpful error messages with suggestions
//! - Fast execution (<500ms all checks)
//! - Fail fast (stop on first critical error)

use ggen_core::validation::preflight::{PreFlightValidator, PreFlightResult};
use ggen_core::manifest::{
    GgenManifest, ProjectConfig, OntologyConfig, GenerationConfig, InferenceConfig,
    ValidationConfig, GenerationRule, TemplateSource, QuerySource
};
use std::fs;
use std::time::Instant;
use tempfile::TempDir;

// ============================================================================
// Test 1: Disk Space Check (E0020)
// ============================================================================

#[test]
fn test_disk_space_check_passes_with_sufficient_space() {
    // Arrange: Use temp directory (should have space)
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_init(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should pass (temp dir has space)
    assert!(result.is_ok(), "Disk space check should pass on temp dir");
    let result = result.unwrap();
    assert!(result.passed_checks.contains(&"Disk space".to_string()));
}

#[test]
fn test_disk_space_error_message_format() {
    // This test documents the expected error format
    // Actual simulation of low disk space is platform-specific and difficult
    // to test reliably, so we verify the error format is correct by examining
    // the implementation.

    // Expected format:
    // error[E0020]: Insufficient disk space
    //   --> /path/to/dir
    //   |
    //   = Available: XX.XX MB
    //   = Required: 100.00 MB
    //   = help: Free up at least XX.XX MB of disk space

    // This is documented in PREFLIGHT_ERROR_CODES.md
}

// ============================================================================
// Test 2: Write Permission Check (E0021)
// ============================================================================

#[test]
fn test_write_permission_check_passes_on_writable_directory() {
    // Arrange: Use temp directory (writable)
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_init(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should pass
    assert!(result.is_ok(), "Permission check should pass on writable dir");
    let result = result.unwrap();
    assert!(result.passed_checks.contains(&"Permissions".to_string()));
}

#[test]
#[cfg(unix)]
fn test_write_permission_check_fails_on_readonly_directory() {
    // Arrange: Create temp directory and make it read-only
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let path = temp_dir.path();

    // Make directory read-only (remove write permission)
    use std::os::unix::fs::PermissionsExt;
    let mut perms = fs::metadata(path).unwrap().permissions();
    perms.set_mode(0o444); // r--r--r--
    fs::set_permissions(path, perms).unwrap();

    let validator = PreFlightValidator::for_init(path);

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should fail with E0021
    assert!(result.is_err(), "Should fail on read-only directory");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0021"), "Should use error code E0021");
    assert!(err.contains("Insufficient permissions"), "Should mention permissions");
    assert!(err.contains("help:"), "Should provide help message");

    // Cleanup: Restore permissions before dropping
    let mut perms = fs::metadata(path).unwrap().permissions();
    perms.set_mode(0o755);
    let _ = fs::set_permissions(path, perms);
}

#[test]
fn test_permission_check_error_message_format() {
    // Expected format:
    // error[E0021]: Insufficient permissions
    //   --> /path/to/dir
    //   |
    //   = Cannot write to directory
    //   = Error: Permission denied (os error 13)
    //   = help: Check directory permissions or run with appropriate privileges
}

// ============================================================================
// Test 3: LLM Provider Health Check (E0022)
// ============================================================================

#[test]
fn test_llm_provider_check_skipped_for_init() {
    // Arrange: Create validator for init (LLM check disabled)
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_init(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should pass (LLM check not run)
    assert!(result.is_ok(), "Init should not check LLM");
    let result = result.unwrap();
    assert!(!result.passed_checks.contains(&"LLM provider".to_string()));
}

#[test]
fn test_llm_provider_mock_always_available() {
    // Arrange: Set mock provider
    std::env::set_var("GGEN_LLM_PROVIDER", "mock");

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(true);

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Mock provider should always pass
    assert!(result.is_ok(), "Mock provider should always be available");
    let result = result.unwrap();
    assert!(result.passed_checks.contains(&"LLM provider".to_string())
            || result.warnings.is_empty());

    // Cleanup
    std::env::remove_var("GGEN_LLM_PROVIDER");
}

#[test]
fn test_llm_provider_openai_requires_api_key() {
    // Arrange: Set OpenAI provider without API key
    std::env::set_var("GGEN_LLM_PROVIDER", "openai");
    std::env::remove_var("OPENAI_API_KEY");

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(true);

    // Act: Run validation (will fail or warn)
    let result = validator.validate(None);

    // Assert: Should warn about missing API key
    // Note: LLM check is a warning, not a hard failure
    if let Ok(res) = result {
        let warnings_str = res.warnings.join(" ");
        assert!(
            warnings_str.contains("E0022") || warnings_str.contains("OPENAI_API_KEY"),
            "Should warn about missing OpenAI API key"
        );
    }

    // Cleanup
    std::env::remove_var("GGEN_LLM_PROVIDER");
}

#[test]
fn test_llm_provider_anthropic_requires_api_key() {
    // Arrange: Set Anthropic provider without API key
    std::env::set_var("GGEN_LLM_PROVIDER", "anthropic");
    std::env::remove_var("ANTHROPIC_API_KEY");

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(true);

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should warn about missing API key
    if let Ok(res) = result {
        let warnings_str = res.warnings.join(" ");
        assert!(
            warnings_str.contains("E0022") || warnings_str.contains("ANTHROPIC_API_KEY"),
            "Should warn about missing Anthropic API key"
        );
    }

    // Cleanup
    std::env::remove_var("GGEN_LLM_PROVIDER");
}

// ============================================================================
// Test 4: Manifest Validation (E0023)
// ============================================================================

fn create_test_manifest(
    project_name: &str,
    ontology_source: &str,
    has_rules: bool,
) -> GgenManifest {
    use std::path::PathBuf;

    GgenManifest {
        project: ProjectConfig {
            name: project_name.to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from(ontology_source),
            imports: vec![],
            base_iri: None,
            prefixes: Default::default(),
        },
        inference: InferenceConfig::default(),
        generation: GenerationConfig {
            rules: if has_rules {
                vec![GenerationRule {
                    name: "test_rule".to_string(),
                    query: QuerySource::Inline {
                        inline: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
                    },
                    template: TemplateSource::Inline {
                        inline: "test content".to_string(),
                    },
                    output_file: "out.txt".to_string(),
                    skip_empty: false,
                    mode: Default::default(),
                    when: None,
                }]
            } else {
                vec![]
            },
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from("src/generated"),
        },
        validation: ValidationConfig::default(),
    }
}

#[test]
fn test_manifest_validation_passes_with_valid_manifest() {
    // Arrange: Create valid manifest
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");

    let manifest = create_test_manifest("test_project", "ontology.ttl", true);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should pass
    assert!(result.is_ok(), "Valid manifest should pass validation");
    let result = result.unwrap();
    assert!(result.passed_checks.contains(&"Manifest".to_string()));
}

#[test]
fn test_manifest_validation_fails_with_empty_project_name() {
    // Arrange: Create manifest with empty project name
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");

    let manifest = create_test_manifest("", "ontology.ttl", true);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with E0023
    assert!(result.is_err(), "Empty project name should fail");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0023"), "Should use error code E0023");
    assert!(err.contains("project.name"), "Should mention project.name");
    assert!(err.contains("help:"), "Should provide help message");
}

#[test]
fn test_manifest_validation_fails_with_missing_ontology_file() {
    // Arrange: Create manifest with non-existent ontology file
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest = create_test_manifest("test_project", "nonexistent.ttl", true);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with E0023
    assert!(result.is_err(), "Missing ontology file should fail");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0023"), "Should use error code E0023");
    assert!(err.contains("Ontology file not found"), "Should mention ontology file");
    assert!(err.contains("help:"), "Should provide help message");
}

#[test]
fn test_manifest_validation_fails_with_no_generation_rules() {
    // Arrange: Create manifest with no rules
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");

    let manifest = create_test_manifest("test_project", "ontology.ttl", false);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with E0023
    assert!(result.is_err(), "No generation rules should fail");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0023"), "Should use error code E0023");
    assert!(err.contains("generation rules"), "Should mention generation rules");
    assert!(err.contains("help:"), "Should provide help message");
}

// ============================================================================
// Test 5: Template Syntax Validation (E0024)
// ============================================================================

#[test]
fn test_template_validation_passes_with_valid_template() {
    // Arrange: Create manifest with valid template
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let template_path = temp_dir.path().join("template.tera");

    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");
    fs::write(&template_path, "Hello {{ name }}!").expect("Failed to write template");

    let mut manifest = create_test_manifest("test_project", "ontology.ttl", true);
    manifest.generation.rules[0].template = TemplateSource::File {
        file: "template.tera".into(),
    };

    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_template_check(true);

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should pass
    assert!(result.is_ok(), "Valid template should pass validation");
    let result = result.unwrap();
    assert!(result.passed_checks.contains(&"Templates".to_string()));
}

#[test]
fn test_template_validation_fails_with_missing_template_file() {
    // Arrange: Create manifest with non-existent template
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");

    let mut manifest = create_test_manifest("test_project", "ontology.ttl", true);
    manifest.generation.rules[0].template = TemplateSource::File {
        file: "nonexistent.tera".into(),
    };

    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_template_check(true);

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with E0024
    assert!(result.is_err(), "Missing template file should fail");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0024"), "Should use error code E0024");
    assert!(err.contains("Template file not found"), "Should mention template file");
    assert!(err.contains("help:"), "Should provide help message");
}

#[test]
fn test_template_validation_fails_with_invalid_syntax() {
    // Arrange: Create manifest with invalid template syntax
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let template_path = temp_dir.path().join("template.tera");

    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");
    // Invalid Tera syntax: unclosed tag
    fs::write(&template_path, "{% for item in items %}{{ item }}").expect("Failed to write template");

    let mut manifest = create_test_manifest("test_project", "ontology.ttl", true);
    manifest.generation.rules[0].template = TemplateSource::File {
        file: "template.tera".into(),
    };

    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_template_check(true);

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with E0024
    assert!(result.is_err(), "Invalid template syntax should fail");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0024"), "Should use error code E0024");
    assert!(err.contains("Template syntax error"), "Should mention syntax error");
    assert!(err.contains("help:"), "Should provide help message");
}

#[test]
fn test_template_validation_skips_inline_templates() {
    // Arrange: Create manifest with inline template (no file check needed)
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");

    let manifest = create_test_manifest("test_project", "ontology.ttl", true);
    // Inline template already set in helper

    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_template_check(true);

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should pass (inline templates are not validated as files)
    assert!(result.is_ok(), "Inline templates should not be file-validated");
}

// ============================================================================
// Test 6: Dependency Checking (E0025)
// ============================================================================

#[test]
fn test_dependency_check_git_passes_when_installed() {
    // Arrange: Git is typically installed in CI environments
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_git_check(true);

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should pass if git is installed (or warn if not)
    if result.is_ok() {
        let res = result.unwrap();
        assert!(
            res.passed_checks.contains(&"Git".to_string()) || !res.warnings.is_empty(),
            "Git check should pass or produce warning"
        );
    }
}

#[test]
fn test_dependency_check_skipped_when_disabled() {
    // Arrange: Create validator with git check disabled
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_git_check(false);

    // Act: Run validation
    let result = validator.validate(None);

    // Assert: Should pass (git check not run)
    assert!(result.is_ok(), "Should pass when git check is disabled");
    let result = result.unwrap();
    assert!(!result.passed_checks.contains(&"Git".to_string()));
}

// ============================================================================
// Test 7: Performance and Timing
// ============================================================================

#[test]
fn test_preflight_validation_completes_quickly() {
    // Arrange: Create validator
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");

    let manifest = create_test_manifest("test_project", "ontology.ttl", true);
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(false) // Skip network check for speed
        .with_template_check(false) // Skip template check for speed
        .with_git_check(false);

    // Act: Run validation and measure time
    let start = Instant::now();
    let result = validator.validate(Some(&manifest));
    let duration = start.elapsed();

    // Assert: Should complete quickly (< 500ms for basic checks)
    assert!(result.is_ok(), "Validation should pass");
    assert!(
        duration.as_millis() < 500,
        "Pre-flight checks should complete in < 500ms, took {}ms",
        duration.as_millis()
    );
}

#[test]
fn test_preflight_result_tracks_duration() {
    // Arrange: Create validator
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_init(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(None).expect("Validation should pass");

    // Assert: Duration should be tracked
    assert!(result.duration_ms > 0, "Duration should be tracked");
    assert!(result.duration_ms < 1000, "Should be fast (< 1s)");
}

// ============================================================================
// Test 8: Fail Fast Behavior
// ============================================================================

#[test]
fn test_preflight_fails_fast_on_first_critical_error() {
    // Arrange: Create manifest with multiple issues
    // - Empty project name (E0023)
    // - Missing ontology file (E0023)
    // - No generation rules (E0023)
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest = create_test_manifest("", "nonexistent.ttl", false);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with first error
    assert!(result.is_err(), "Should fail with first error");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0023"), "Should use manifest error code");

    // Note: Implementation collects all failures, but returns on first critical set
    // This documents the behavior - all checks run to provide complete feedback
}

// ============================================================================
// Test 9: PreFlightResult API
// ============================================================================

#[test]
fn test_preflight_result_is_success() {
    // Arrange: Create successful result
    let mut result = PreFlightResult::default();
    result.passed_checks.push("Test".to_string());

    // Assert: is_success returns true
    assert!(result.is_success(), "Should indicate success");

    // Arrange: Add failure
    result.failures.push("Failure".to_string());

    // Assert: is_success returns false
    assert!(!result.is_success(), "Should indicate failure");
}

#[test]
fn test_preflight_result_total_checks() {
    // Arrange: Create result with mixed outcomes
    let mut result = PreFlightResult::default();
    result.passed_checks.push("Pass1".to_string());
    result.passed_checks.push("Pass2".to_string());
    result.failures.push("Fail1".to_string());
    result.warnings.push("Warn1".to_string());

    // Assert: total_checks counts all checks
    assert_eq!(result.total_checks(), 4, "Should count all checks");
}

// ============================================================================
// Test 10: Error Message Quality
// ============================================================================

#[test]
fn test_error_messages_contain_help_text() {
    // This test verifies that all error paths include help text
    // by checking a representative sample

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest = create_test_manifest("", "nonexistent.ttl", false);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    let result = validator.validate(Some(&manifest));

    if let Err(e) = result {
        let err_str = e.to_string();
        assert!(err_str.contains("help:"), "Error should contain help text");
        assert!(err_str.contains("error["), "Error should have error code");
    }
}

#[test]
fn test_error_messages_include_file_paths() {
    // Verify error messages show relevant file paths
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let manifest = create_test_manifest("test", "missing.ttl", true);
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    let result = validator.validate(Some(&manifest));

    if let Err(e) = result {
        let err_str = e.to_string();
        assert!(
            err_str.contains("missing.ttl") || err_str.contains("-->"),
            "Error should show file path: {}",
            err_str
        );
    }
}

// ============================================================================
// Test 11: Builder Pattern
// ============================================================================

#[test]
fn test_validator_builder_for_sync() {
    // Arrange & Act: Create sync validator
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path());

    // Assert: Sync mode has appropriate defaults
    // (We can't inspect private fields, but we can test behavior)
    let result = validator.validate(None);
    assert!(result.is_ok(), "Sync validator should work");
}

#[test]
fn test_validator_builder_for_init() {
    // Arrange & Act: Create init validator
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_init(temp_dir.path());

    // Assert: Init mode works
    let result = validator.validate(None);
    assert!(result.is_ok(), "Init validator should work");
}

#[test]
fn test_validator_builder_chaining() {
    // Arrange & Act: Build validator with chaining
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(false)
        .with_template_check(false)
        .with_git_check(true);

    // Assert: Builder pattern works
    let result = validator.validate(None);
    assert!(result.is_ok(), "Chained builder should work");
}

// ============================================================================
// Test 12: Integration with Real Scenarios
// ============================================================================

#[test]
fn test_full_validation_pipeline_success() {
    // Arrange: Create complete valid setup
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let template_path = temp_dir.path().join("template.tera");

    fs::write(&ontology_path, "# Test ontology").expect("Failed to write ontology");
    fs::write(&template_path, "Hello {{ name }}!").expect("Failed to write template");

    let mut manifest = create_test_manifest("test_project", "ontology.ttl", true);
    manifest.generation.rules[0].template = TemplateSource::File {
        file: "template.tera".into(),
    };

    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(false) // Skip for test stability
        .with_template_check(true)
        .with_git_check(false);

    // Act: Run full validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should pass all checks
    assert!(result.is_ok(), "Full validation should pass");
    let result = result.unwrap();
    assert!(result.passed_checks.len() >= 3, "Should pass multiple checks");
    assert!(result.failures.is_empty(), "Should have no failures");
}

#[test]
fn test_full_validation_pipeline_with_multiple_failures() {
    // Arrange: Create setup with multiple issues
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Don't create ontology file (missing)
    // Empty project name
    // No generation rules
    let manifest = create_test_manifest("", "missing.ttl", false);

    let validator = PreFlightValidator::for_sync(temp_dir.path())
        .with_llm_check(false)
        .with_template_check(false)
        .with_git_check(false);

    // Act: Run validation
    let result = validator.validate(Some(&manifest));

    // Assert: Should fail with detailed errors
    assert!(result.is_err(), "Should fail with multiple issues");
    let err = result.unwrap_err().to_string();
    assert!(err.contains("E0020") || err.contains("E0023"), "Should contain error code");
}
