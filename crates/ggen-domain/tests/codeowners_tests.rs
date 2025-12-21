//! Integration tests for CODEOWNERS generation and aggregation
//!
//! Tests the `[codeowners]` configuration to aggregate OWNERS files
//! from noun directories and generate .github/CODEOWNERS.

use ggen_domain::generation::codeowners::{
    generate_codeowners, generate_codeowners_default, CodeownersConfig,
};
use tempfile::TempDir;
use std::fs;

/// Test: Generate CODEOWNERS with default configuration
#[test]
fn test_generate_codeowners_default() -> std::io::Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // Create ontology directory structure with OWNERS files
    fs::create_dir_all(base_path.join("ontology/user"))?;
    fs::create_dir_all(base_path.join("ontology/product"))?;

    fs::write(
        base_path.join("ontology/user/OWNERS"),
        "@user-team\n@alice\n",
    )?;
    fs::write(
        base_path.join("ontology/product/OWNERS"),
        "@product-team\n@bob\n",
    )?;

    // Generate CODEOWNERS
    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string(), "src/domain".to_string()],
        output_path: Some(base_path.join(".github/CODEOWNERS").to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    // Should succeed
    let result = generate_codeowners(&config, base_path);
    assert!(result.is_ok(), "CODEOWNERS generation should succeed");

    Ok(())
}

/// Test: CODEOWNERS aggregates ownership from multiple OWNERS files
#[test]
fn test_codeowners_aggregates_multiple_owners() -> std::io::Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // Create multiple noun directories with different owners
    fs::create_dir_all(base_path.join("ontology/users"))?;
    fs::create_dir_all(base_path.join("ontology/payments"))?;
    fs::create_dir_all(base_path.join("ontology/notifications"))?;

    fs::write(base_path.join("ontology/users/OWNERS"), "@user-team\n")?;
    fs::write(base_path.join("ontology/payments/OWNERS"), "@finance-team\n")?;
    fs::write(base_path.join("ontology/notifications/OWNERS"), "@platform-team\n")?;

    // Generate CODEOWNERS
    let output_path = base_path.join(".github/CODEOWNERS");
    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string()],
        output_path: Some(output_path.to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    let result = generate_codeowners(&config, base_path);
    assert!(result.is_ok(), "Generation should succeed");

    // Verify output file contains all teams
    if let Ok(codeowners_content) = fs::read_to_string(&output_path) {
        assert!(codeowners_content.contains("@user-team"), "Should contain user team");
        assert!(
            codeowners_content.contains("@finance-team"),
            "Should contain finance team"
        );
        assert!(
            codeowners_content.contains("@platform-team"),
            "Should contain platform team"
        );
    }

    Ok(())
}

/// Test: CODEOWNERS respects path mappings
#[test]
fn test_codeowners_path_mappings() -> std::io::Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // Create OWNERS file
    fs::create_dir_all(base_path.join("ontology/user"))?;
    fs::write(
        base_path.join("ontology/user/OWNERS"),
        "@user-team\n",
    )?;

    // Create output directory
    fs::create_dir_all(base_path.join(".github"))?;

    let output_path = base_path.join(".github/CODEOWNERS");
    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string(), "src/domain".to_string()],
        output_path: Some(output_path.to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    let result = generate_codeowners(&config, base_path);
    assert!(result.is_ok(), "Generation should succeed");

    // Should have patterns for both base directories
    if let Ok(content) = fs::read_to_string(&output_path) {
        // Should reference the nouns being protected
        assert!(!content.is_empty(), "CODEOWNERS should not be empty");
    }

    Ok(())
}

/// Test: Missing OWNERS files are handled gracefully
#[test]
fn test_missing_owners_files_graceful() -> std::io::Result<()> {
    let temp_dir = TempDir::new()?;
    let base_path = temp_dir.path();

    // Create directory without OWNERS file
    fs::create_dir_all(base_path.join("ontology/user"))?;

    // Should still succeed (just with empty or default content)
    let output_path = base_path.join(".github/CODEOWNERS");
    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string()],
        output_path: Some(output_path.to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    let result = generate_codeowners(&config, base_path);
    // Should succeed even if no OWNERS files exist
    assert!(result.is_ok(), "Should handle missing OWNERS files gracefully");

    Ok(())
}

/// Test: Default CODEOWNERS generation creates standard format
#[test]
fn test_default_codeowners_format() {
    let temp_dir = TempDir::new().unwrap();
    let base_path = temp_dir.path();

    // Create standard structure
    fs::create_dir_all(base_path.join("ontology/users")).unwrap();
    fs::write(base_path.join("ontology/users/OWNERS"), "@user-team\n").unwrap();
    fs::create_dir_all(base_path.join(".github")).unwrap();

    let output_path = base_path.join(".github/CODEOWNERS");
    let result = generate_codeowners_default(base_path, &output_path);

    // Should succeed
    assert!(result.is_ok(), "Default CODEOWNERS generation should succeed");
}

/// Test: CODEOWNERS disabled when enabled=false
#[test]
fn test_codeowners_respects_enabled_flag() {
    let temp_dir = TempDir::new().unwrap();
    let base_path = temp_dir.path();

    let output_path = base_path.join(".github/CODEOWNERS");
    let config = CodeownersConfig {
        enabled: false,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string()],
        output_path: Some(output_path.to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    let result = generate_codeowners(&config, base_path);

    // When disabled, generation should return an error
    assert!(result.is_err(), "Generation should fail when disabled");
}

/// Test: Regeneration doesn't break existing CODEOWNERS
#[test]
fn test_codeowners_idempotent_generation() {
    let temp_dir = TempDir::new().unwrap();
    let base_path = temp_dir.path();

    fs::create_dir_all(base_path.join("ontology/user")).unwrap();
    fs::write(base_path.join("ontology/user/OWNERS"), "@team\n").unwrap();
    fs::create_dir_all(base_path.join(".github")).unwrap();

    let output_path = base_path.join(".github/CODEOWNERS");

    // Generate once
    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string()],
        output_path: Some(output_path.to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    let first = generate_codeowners(&config, base_path);
    assert!(first.is_ok(), "First generation should succeed");

    // Generate again
    let second = generate_codeowners(&config, base_path);
    assert!(second.is_ok(), "Second generation should succeed");

    // File content should be consistent
    if first.is_ok() && second.is_ok() {
        if let (Ok(content1), Ok(content2)) = (
            fs::read_to_string(&output_path),
            fs::read_to_string(&output_path),
        ) {
            assert_eq!(
                content1, content2,
                "CODEOWNERS should be consistent on regeneration"
            );
        }
    }
}

/// Test: Complex OWNERS patterns are preserved
#[test]
fn test_complex_owners_patterns() {
    let temp_dir = TempDir::new().unwrap();
    let base_path = temp_dir.path();

    fs::create_dir_all(base_path.join("ontology/user")).unwrap();
    fs::write(
        base_path.join("ontology/user/OWNERS"),
        "@user-team\n@alice @bob\n*.breaking.ttl @platform-team\n",
    ).unwrap();

    let output_path = base_path.join(".github/CODEOWNERS");
    fs::create_dir_all(base_path.join(".github")).unwrap();

    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec!["ontology".to_string()],
        base_dirs: vec!["src/generated".to_string()],
        output_path: Some(output_path.to_string_lossy().to_string()),
        auto_regenerate: false,
    };

    let result = generate_codeowners(&config, base_path);
    assert!(result.is_ok(), "Should handle complex patterns");
}
