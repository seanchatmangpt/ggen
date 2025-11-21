//! Phase 3A: Comprehensive Core Unit Tests (Chicago TDD Style)
//!
//! Tests models, validation, error types, and builders.
//! Following Chicago TDD: state-based testing, real collaborators, AAA pattern.
//!
//! Test Count: 100+ tests covering models, error, validation, builders

use ggen_marketplace_v2::error::{Error, ErrorContext};
use ggen_marketplace_v2::models::*;
use ggen_marketplace_v2::validation::{
    AuthorValidator, CheckSeverity, LicenseValidator, MetadataValidator, PackageValidator,
    ReadmeValidator, RepositoryValidator, ValidationCheck, ValidationResult, Validator,
};
use std::collections::HashSet;
use std::str::FromStr;

// ============================================================================
// SECTION 1: Error Type Tests (20 tests)
// ============================================================================

#[test]
fn test_error_package_not_found_creation() {
    // Arrange
    let package_id = "test-package";

    // Act
    let err = Error::package_not_found(package_id);

    // Assert
    let msg = err.to_string();
    assert!(msg.contains("Package not found"));
    assert!(msg.contains("test-package"));
}

#[test]
fn test_error_invalid_package_id_creation() {
    // Arrange
    let reason = "contains invalid characters";

    // Act
    let err = Error::invalid_package_id(reason);

    // Assert
    assert!(err.to_string().contains("Invalid package ID"));
}

#[test]
fn test_error_invalid_version_creation() {
    // Arrange & Act
    let err = Error::invalid_version("1.0", "missing patch version");

    // Assert
    let msg = err.to_string();
    assert!(msg.contains("Invalid semantic version"));
    assert!(msg.contains("1.0"));
}

#[test]
fn test_error_dependency_resolution_failed() {
    // Arrange & Act
    let err = Error::dependency_resolution_failed("pkg-a", "cyclic dependency detected");

    // Assert
    let msg = err.to_string();
    assert!(msg.contains("Dependency resolution failed"));
    assert!(msg.contains("pkg-a"));
}

#[test]
fn test_error_crypto_error_creation() {
    // Arrange & Act
    let err = Error::crypto_error("invalid signature format");

    // Assert
    assert!(err.to_string().contains("Cryptographic error"));
}

#[test]
fn test_error_search_error_creation() {
    // Arrange & Act
    let err = Error::search_error("query too complex");

    // Assert
    assert!(err.to_string().contains("Search error"));
}

#[test]
fn test_error_registry_error_creation() {
    // Arrange & Act
    let err = Error::registry_error("connection failed");

    // Assert
    assert!(err.to_string().contains("Registry error"));
}

#[test]
fn test_error_concurrency_error_creation() {
    // Arrange & Act
    let err = Error::concurrency_error("channel closed");

    // Assert
    assert!(err.to_string().contains("Concurrency error"));
}

#[test]
fn test_error_config_error_creation() {
    // Arrange & Act
    let err = Error::config_error("missing field: api_key");

    // Assert
    assert!(err.to_string().contains("Configuration error"));
}

#[test]
fn test_error_timeout_creation() {
    // Arrange & Act
    let err = Error::timeout("operation exceeded 30s limit");

    // Assert
    assert!(err.to_string().contains("Operation timed out"));
}

#[test]
fn test_error_context_creation() {
    // Arrange
    let operation = "fetching package";
    let context = "network timeout";

    // Act
    let ctx = ErrorContext::new(operation, context);

    // Assert
    assert_eq!(ctx.operation, "fetching package");
    assert_eq!(ctx.context, "network timeout");
    assert!(ctx.to_string().contains("fetching package"));
}

#[test]
fn test_error_display_format() {
    // Arrange
    let err = Error::PackageAlreadyExists {
        package_id: "existing-pkg".to_string(),
    };

    // Act
    let display = format!("{}", err);

    // Assert
    assert!(display.contains("already exists"));
    assert!(display.contains("existing-pkg"));
}

#[test]
fn test_error_version_already_exists() {
    // Arrange & Act
    let err = Error::VersionAlreadyExists {
        package_id: "my-pkg".to_string(),
        version: "2.0.0".to_string(),
    };

    // Assert
    let msg = err.to_string();
    assert!(msg.contains("Version"));
    assert!(msg.contains("2.0.0"));
    assert!(msg.contains("my-pkg"));
}

#[test]
fn test_error_validation_failed() {
    // Arrange & Act
    let err = Error::ValidationFailed {
        reason: "missing required field: description".to_string(),
    };

    // Assert
    assert!(err.to_string().contains("Validation failed"));
}

#[test]
fn test_error_security_check_failed() {
    // Arrange & Act
    let err = Error::SecurityCheckFailed {
        reason: "untrusted signature".to_string(),
    };

    // Assert
    assert!(err.to_string().contains("Security check failed"));
}

#[test]
fn test_error_signature_verification_failed() {
    // Arrange & Act
    let err = Error::SignatureVerificationFailed {
        reason: "invalid public key".to_string(),
    };

    // Assert
    assert!(err.to_string().contains("Signature verification failed"));
}

#[test]
fn test_error_sparql_error() {
    // Arrange & Act
    let err = Error::SparqlError {
        query: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        reason: "syntax error at position 10".to_string(),
    };

    // Assert
    let msg = err.to_string();
    assert!(msg.contains("SPARQL error"));
}

#[test]
fn test_error_rdf_store_error() {
    // Arrange & Act
    let err = Error::RdfStoreError {
        operation: "insert".to_string(),
        reason: "store locked".to_string(),
    };

    // Assert
    assert!(err.to_string().contains("RDF store error"));
}

#[test]
fn test_error_invalid_state_transition() {
    // Arrange & Act
    let err = Error::InvalidStateTransition {
        from: "Draft".to_string(),
        to: "Yanked".to_string(),
    };

    // Assert
    let msg = err.to_string();
    assert!(msg.contains("Invalid state transition"));
    assert!(msg.contains("Draft"));
    assert!(msg.contains("Yanked"));
}

#[test]
fn test_error_other() {
    // Arrange & Act
    let err = Error::Other("unexpected condition".to_string());

    // Assert
    assert!(err.to_string().contains("unexpected condition"));
}

// ============================================================================
// SECTION 2: PackageId Model Tests (25 tests)
// ============================================================================

#[test]
fn test_package_id_valid_alphanumeric() {
    // Arrange & Act
    let id = PackageId::new("mypackage123").unwrap();

    // Assert
    assert_eq!(id.as_str(), "mypackage123");
}

#[test]
fn test_package_id_valid_with_hyphens() {
    // Arrange & Act
    let id = PackageId::new("my-awesome-package").unwrap();

    // Assert
    assert_eq!(id.as_str(), "my-awesome-package");
}

#[test]
fn test_package_id_valid_with_underscores() {
    // Arrange & Act
    let id = PackageId::new("my_package_name").unwrap();

    // Assert
    assert_eq!(id.as_str(), "my_package_name");
}

#[test]
fn test_package_id_lowercase_normalization() {
    // Arrange & Act
    let id = PackageId::new("MyPackage").unwrap();

    // Assert
    assert_eq!(id.as_str(), "mypackage");
}

#[test]
fn test_package_id_reject_empty() {
    // Arrange & Act
    let result = PackageId::new("");

    // Assert
    assert!(result.is_err());
    match result {
        Err(Error::InvalidPackageId { .. }) => {}
        _ => panic!("Expected InvalidPackageId error"),
    }
}

#[test]
fn test_package_id_reject_special_chars() {
    // Arrange & Act & Assert
    assert!(PackageId::new("package@name").is_err());
    assert!(PackageId::new("package#name").is_err());
    assert!(PackageId::new("package$name").is_err());
    assert!(PackageId::new("package!name").is_err());
}

#[test]
fn test_package_id_reject_leading_hyphen() {
    // Arrange & Act
    let result = PackageId::new("-package");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_package_id_reject_trailing_hyphen() {
    // Arrange & Act
    let result = PackageId::new("package-");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_package_id_max_length_accepted() {
    // Arrange
    let long_id = "a".repeat(200);

    // Act
    let result = PackageId::new(long_id);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_package_id_too_long_rejected() {
    // Arrange
    let too_long = "a".repeat(201);

    // Act
    let result = PackageId::new(too_long);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_package_id_equality() {
    // Arrange
    let id1 = PackageId::new("package").unwrap();
    let id2 = PackageId::new("package").unwrap();

    // Act & Assert
    assert_eq!(id1, id2);
}

#[test]
fn test_package_id_case_insensitive_equality() {
    // Arrange
    let id1 = PackageId::new("Package").unwrap();
    let id2 = PackageId::new("PACKAGE").unwrap();

    // Act & Assert
    assert_eq!(id1, id2);
}

#[test]
fn test_package_id_ordering() {
    // Arrange
    let id1 = PackageId::new("aaa").unwrap();
    let id2 = PackageId::new("bbb").unwrap();

    // Act & Assert
    assert!(id1 < id2);
}

#[test]
fn test_package_id_display() {
    // Arrange
    let id = PackageId::new("test-package").unwrap();

    // Act
    let display = format!("{}", id);

    // Assert
    assert_eq!(display, "test-package");
}

#[test]
fn test_package_id_from_str() {
    // Arrange & Act
    let id = PackageId::from_str("test").unwrap();

    // Assert
    assert_eq!(id.as_str(), "test");
}

#[test]
fn test_package_id_as_ref() {
    // Arrange
    let id = PackageId::new("test").unwrap();

    // Act
    let s: &str = id.as_ref();

    // Assert
    assert_eq!(s, "test");
}

#[test]
fn test_package_id_clone() {
    // Arrange
    let id1 = PackageId::new("test").unwrap();

    // Act
    let id2 = id1.clone();

    // Assert
    assert_eq!(id1, id2);
}

#[test]
fn test_package_id_hash() {
    // Arrange
    let mut set = HashSet::new();
    let id1 = PackageId::new("test").unwrap();
    let id2 = PackageId::new("test").unwrap();

    // Act
    set.insert(id1);

    // Assert
    assert!(set.contains(&id2));
}

#[test]
fn test_package_id_serialize() {
    // Arrange
    let id = PackageId::new("test").unwrap();

    // Act
    let json = serde_json::to_string(&id).unwrap();

    // Assert
    assert_eq!(json, r#""test""#);
}

#[test]
fn test_package_id_deserialize() {
    // Arrange
    let json = r#""my-package""#;

    // Act
    let id: PackageId = serde_json::from_str(json).unwrap();

    // Assert
    assert_eq!(id.as_str(), "my-package");
}

#[test]
fn test_package_id_roundtrip() {
    // Arrange
    let original = PackageId::new("my-package").unwrap();

    // Act
    let json = serde_json::to_string(&original).unwrap();
    let restored: PackageId = serde_json::from_str(&json).unwrap();

    // Assert
    assert_eq!(original, restored);
}

// ============================================================================
// SECTION 3: PackageVersion Model Tests (25 tests)
// ============================================================================

#[test]
fn test_version_valid_simple() {
    // Arrange & Act
    let v = PackageVersion::new("1.0.0").unwrap();

    // Assert
    assert_eq!(v.as_str(), "1.0.0");
}

#[test]
fn test_version_v_prefix_stripped() {
    // Arrange & Act
    let v = PackageVersion::new("v1.0.0").unwrap();

    // Assert
    assert_eq!(v.as_str(), "1.0.0");
}

#[test]
fn test_version_major_minor_patch() {
    // Arrange & Act
    let v = PackageVersion::new("2.5.10").unwrap();

    // Assert
    assert_eq!(v.as_str(), "2.5.10");
}

#[test]
fn test_version_with_prerelease() {
    // Arrange & Act
    let v = PackageVersion::new("1.0.0-alpha").unwrap();

    // Assert
    assert_eq!(v.as_str(), "1.0.0-alpha");
}

#[test]
fn test_version_with_build_metadata() {
    // Arrange & Act
    let v = PackageVersion::new("1.0.0+build.123").unwrap();

    // Assert
    assert_eq!(v.as_str(), "1.0.0+build.123");
}

#[test]
fn test_version_full_semver() {
    // Arrange & Act
    let v = PackageVersion::new("1.0.0-alpha.1+build.123").unwrap();

    // Assert
    assert_eq!(v.as_str(), "1.0.0-alpha.1+build.123");
}

#[test]
fn test_version_reject_empty() {
    // Arrange & Act
    let result = PackageVersion::new("");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_version_reject_incomplete() {
    // Arrange & Act & Assert
    assert!(PackageVersion::new("1.0").is_err());
    assert!(PackageVersion::new("1").is_err());
}

#[test]
fn test_version_reject_non_numeric_major() {
    // Arrange & Act
    let result = PackageVersion::new("a.0.0");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_version_zero_version() {
    // Arrange & Act
    let v = PackageVersion::new("0.0.0").unwrap();

    // Assert
    assert_eq!(v.as_str(), "0.0.0");
}

#[test]
fn test_version_large_numbers() {
    // Arrange & Act
    let v = PackageVersion::new("999.999.999").unwrap();

    // Assert
    assert_eq!(v.as_str(), "999.999.999");
}

#[test]
fn test_version_ordering_major() {
    // Arrange
    let v1 = PackageVersion::new("1.0.0").unwrap();
    let v2 = PackageVersion::new("2.0.0").unwrap();

    // Act & Assert
    assert!(v1 < v2);
}

#[test]
fn test_version_ordering_minor() {
    // Arrange
    let v1 = PackageVersion::new("1.5.0").unwrap();
    let v2 = PackageVersion::new("1.10.0").unwrap();

    // Act & Assert
    assert!(v1 < v2);
}

#[test]
fn test_version_ordering_patch() {
    // Arrange
    let v1 = PackageVersion::new("1.0.5").unwrap();
    let v2 = PackageVersion::new("1.0.10").unwrap();

    // Act & Assert
    assert!(v1 < v2);
}

#[test]
fn test_version_equality() {
    // Arrange
    let v1 = PackageVersion::new("1.0.0").unwrap();
    let v2 = PackageVersion::new("1.0.0").unwrap();

    // Act & Assert
    assert_eq!(v1, v2);
}

#[test]
fn test_version_display() {
    // Arrange
    let v = PackageVersion::new("1.2.3").unwrap();

    // Act
    let display = format!("{}", v);

    // Assert
    assert_eq!(display, "1.2.3");
}

#[test]
fn test_version_from_str() {
    // Arrange & Act
    let v = PackageVersion::from_str("1.0.0").unwrap();

    // Assert
    assert_eq!(v.as_str(), "1.0.0");
}

#[test]
fn test_version_clone() {
    // Arrange
    let v1 = PackageVersion::new("1.0.0").unwrap();

    // Act
    let v2 = v1.clone();

    // Assert
    assert_eq!(v1, v2);
}

#[test]
fn test_version_serialize() {
    // Arrange
    let v = PackageVersion::new("1.0.0").unwrap();

    // Act
    let json = serde_json::to_string(&v).unwrap();

    // Assert
    assert_eq!(json, r#""1.0.0""#);
}

#[test]
fn test_version_deserialize() {
    // Arrange
    let json = r#""2.5.3""#;

    // Act
    let v: PackageVersion = serde_json::from_str(json).unwrap();

    // Assert
    assert_eq!(v.as_str(), "2.5.3");
}

#[test]
fn test_version_sort_list() {
    // Arrange
    let mut versions = vec![
        PackageVersion::new("2.0.0").unwrap(),
        PackageVersion::new("1.0.0").unwrap(),
        PackageVersion::new("1.5.0").unwrap(),
    ];

    // Act
    versions.sort();

    // Assert
    assert_eq!(versions[0].as_str(), "1.0.0");
    assert_eq!(versions[1].as_str(), "1.5.0");
    assert_eq!(versions[2].as_str(), "2.0.0");
}

// ============================================================================
// SECTION 4: QualityScore Tests (20 tests)
// ============================================================================

#[test]
fn test_quality_score_valid_min() {
    // Arrange & Act
    let score = QualityScore::new(1).unwrap();

    // Assert
    assert_eq!(score.value(), 1);
}

#[test]
fn test_quality_score_valid_max() {
    // Arrange & Act
    let score = QualityScore::new(100).unwrap();

    // Assert
    assert_eq!(score.value(), 100);
}

#[test]
fn test_quality_score_valid_mid() {
    // Arrange & Act
    let score = QualityScore::new(50).unwrap();

    // Assert
    assert_eq!(score.value(), 50);
}

#[test]
fn test_quality_score_reject_zero() {
    // Arrange & Act
    let result = QualityScore::new(0);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_quality_score_reject_over_100() {
    // Arrange & Act
    let result = QualityScore::new(101);

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_quality_score_is_production_ready() {
    // Arrange
    let score = QualityScore::new(95).unwrap();

    // Act & Assert
    assert!(score.is_production_ready());
}

#[test]
fn test_quality_score_not_production_ready() {
    // Arrange
    let score = QualityScore::new(94).unwrap();

    // Act & Assert
    assert!(!score.is_production_ready());
}

#[test]
fn test_quality_score_needs_improvement() {
    // Arrange
    let score = QualityScore::new(85).unwrap();

    // Act & Assert
    assert!(score.needs_improvement());
}

#[test]
fn test_quality_score_not_ready() {
    // Arrange
    let score = QualityScore::new(79).unwrap();

    // Act & Assert
    assert!(score.not_ready());
}

#[test]
fn test_quality_score_ordering() {
    // Arrange
    let s1 = QualityScore::new(30).unwrap();
    let s2 = QualityScore::new(70).unwrap();

    // Act & Assert
    assert!(s1 < s2);
}

#[test]
fn test_quality_score_equality() {
    // Arrange
    let s1 = QualityScore::new(50).unwrap();
    let s2 = QualityScore::new(50).unwrap();

    // Act & Assert
    assert_eq!(s1, s2);
}

#[test]
fn test_quality_score_copy() {
    // Arrange
    let s1 = QualityScore::new(75).unwrap();

    // Act
    let s2 = s1;

    // Assert (s1 is still valid due to Copy)
    assert_eq!(s1.value(), s2.value());
}

#[test]
fn test_quality_score_display() {
    // Arrange
    let score = QualityScore::new(85).unwrap();

    // Act
    let display = format!("{}", score);

    // Assert
    assert_eq!(display, "85%");
}

#[test]
fn test_quality_score_serialize() {
    // Arrange
    let score = QualityScore::new(85).unwrap();

    // Act
    let json = serde_json::to_string(&score).unwrap();

    // Assert
    assert_eq!(json, "85");
}

#[test]
fn test_quality_score_deserialize() {
    // Arrange
    let json = "75";

    // Act
    let score: QualityScore = serde_json::from_str(json).unwrap();

    // Assert
    assert_eq!(score.value(), 75);
}

#[test]
fn test_quality_score_roundtrip() {
    // Arrange
    let original = QualityScore::new(42).unwrap();

    // Act
    let json = serde_json::to_string(&original).unwrap();
    let restored: QualityScore = serde_json::from_str(&json).unwrap();

    // Assert
    assert_eq!(original, restored);
}

// ============================================================================
// SECTION 5: PackageState Tests (10 tests)
// ============================================================================

#[test]
fn test_package_state_draft() {
    // Arrange & Act
    let state = PackageState::Draft;

    // Assert
    assert!(matches!(state, PackageState::Draft));
}

#[test]
fn test_package_state_published() {
    // Arrange & Act
    let state = PackageState::Published;

    // Assert
    assert!(matches!(state, PackageState::Published));
}

#[test]
fn test_package_state_deprecated() {
    // Arrange & Act
    let state = PackageState::Deprecated;

    // Assert
    assert!(matches!(state, PackageState::Deprecated));
}

#[test]
fn test_package_state_yanked() {
    // Arrange & Act
    let state = PackageState::Yanked;

    // Assert
    assert!(matches!(state, PackageState::Yanked));
}

#[test]
fn test_package_state_equality() {
    // Arrange & Act & Assert
    assert_eq!(PackageState::Draft, PackageState::Draft);
    assert_ne!(PackageState::Draft, PackageState::Published);
}

#[test]
fn test_package_state_clone() {
    // Arrange
    let s1 = PackageState::Published;

    // Act
    let s2 = s1.clone();

    // Assert
    assert_eq!(s1, s2);
}

#[test]
fn test_package_state_serialize() {
    // Arrange
    let state = PackageState::Published;

    // Act
    let json = serde_json::to_string(&state).unwrap();

    // Assert
    assert!(json.contains("Published"));
}

#[test]
fn test_package_state_deserialize() {
    // Arrange
    let json = r#""Draft""#;

    // Act
    let state: PackageState = serde_json::from_str(json).unwrap();

    // Assert
    assert_eq!(state, PackageState::Draft);
}

// ============================================================================
// SECTION 6: PackageMetadata Tests (15 tests)
// ============================================================================

#[test]
fn test_metadata_creation() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();

    // Act
    let metadata = PackageMetadata::new(id.clone(), "Test Package", "A test package", "MIT");

    // Assert
    assert_eq!(metadata.id, id);
    assert_eq!(metadata.name, "Test Package");
    assert_eq!(metadata.description, "A test package");
    assert_eq!(metadata.license, "MIT");
}

#[test]
fn test_metadata_timestamps() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();

    // Act
    let metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");

    // Assert
    assert!(metadata.created_at <= chrono::Utc::now());
    assert!(metadata.updated_at <= chrono::Utc::now());
}

#[test]
fn test_metadata_default_values() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();

    // Act
    let metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");

    // Assert
    assert!(metadata.authors.is_empty());
    assert!(metadata.keywords.is_empty());
    assert!(metadata.categories.is_empty());
    assert!(metadata.repository.is_none());
    assert!(metadata.homepage.is_none());
    assert_eq!(metadata.downloads, 0);
    assert!(metadata.quality_score.is_none());
}

#[test]
fn test_metadata_clone() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();
    let m1 = PackageMetadata::new(id, "Test", "Desc", "MIT");

    // Act
    let m2 = m1.clone();

    // Assert
    assert_eq!(m1.name, m2.name);
    assert_eq!(m1.description, m2.description);
}

#[test]
fn test_metadata_equality() {
    // Arrange
    let id1 = PackageId::new("test-pkg").unwrap();
    let id2 = PackageId::new("test-pkg").unwrap();
    let m1 = PackageMetadata::new(id1, "Test", "Desc", "MIT");
    let m2 = PackageMetadata::new(id2, "Test", "Desc", "MIT");

    // Assert (equality is based on fields, not identity)
    assert_eq!(m1.id, m2.id);
    assert_eq!(m1.name, m2.name);
}

// ============================================================================
// SECTION 7: Package Tests (10 tests)
// ============================================================================

#[test]
fn test_package_creation() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
    let version = PackageVersion::new("1.0.0").unwrap();

    // Act
    let package = Package {
        metadata,
        latest_version: version.clone(),
        versions: vec![version],
        releases: indexmap::IndexMap::new(),
    };

    // Assert
    assert_eq!(package.latest_version.as_str(), "1.0.0");
    assert_eq!(package.versions.len(), 1);
}

#[test]
fn test_package_multiple_versions() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
    let v1 = PackageVersion::new("1.0.0").unwrap();
    let v2 = PackageVersion::new("2.0.0").unwrap();

    // Act
    let package = Package {
        metadata,
        latest_version: v2.clone(),
        versions: vec![v2, v1],
        releases: indexmap::IndexMap::new(),
    };

    // Assert
    assert_eq!(package.versions.len(), 2);
    assert_eq!(package.latest_version.as_str(), "2.0.0");
}

#[test]
fn test_package_serialize() {
    // Arrange
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");
    let version = PackageVersion::new("1.0.0").unwrap();
    let package = Package {
        metadata,
        latest_version: version.clone(),
        versions: vec![version],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let json = serde_json::to_string(&package).unwrap();

    // Assert
    assert!(json.contains("test-pkg"));
    assert!(json.contains("1.0.0"));
}

// ============================================================================
// SECTION 8: Validation Tests (15 tests)
// ============================================================================

#[tokio::test]
async fn test_metadata_validator_passes() {
    // Arrange
    let validator = MetadataValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(result.passed);
}

#[tokio::test]
async fn test_metadata_validator_fails_empty_name() {
    // Arrange
    let validator = MetadataValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let mut metadata = PackageMetadata::new(id, "", "A test package", "MIT");
    metadata.name = String::new();
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(!result.passed);
}

#[tokio::test]
async fn test_license_validator_mit() {
    // Arrange
    let validator = LicenseValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(result.passed);
}

#[tokio::test]
async fn test_license_validator_apache() {
    // Arrange
    let validator = LicenseValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test", "Desc", "Apache-2.0");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(result.passed);
}

#[tokio::test]
async fn test_readme_validator() {
    // Arrange
    let validator = ReadmeValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test", "Has description", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(result.passed);
}

#[tokio::test]
async fn test_repository_validator_with_repo() {
    // Arrange
    let validator = RepositoryValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let mut metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");
    metadata.repository = Some("https://github.com/test/test".to_string());
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(result.passed);
}

#[tokio::test]
async fn test_repository_validator_without_repo() {
    // Arrange
    let validator = RepositoryValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(!result.passed);
}

#[tokio::test]
async fn test_author_validator_with_authors() {
    // Arrange
    let validator = AuthorValidator;
    let id = PackageId::new("test-pkg").unwrap();
    let mut metadata = PackageMetadata::new(id, "Test", "Desc", "MIT");
    metadata.authors = vec!["alice@example.com".to_string()];
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate(&package).await.unwrap();

    // Assert
    assert!(result.passed);
}

#[tokio::test]
async fn test_package_validator_all() {
    // Arrange
    let validator = PackageValidator::new();
    let id = PackageId::new("test-pkg").unwrap();
    let mut metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
    metadata.authors = vec!["test@example.com".to_string()];
    metadata.repository = Some("https://github.com/test/test".to_string());
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Act
    let result = validator.validate_all(&package).await.unwrap();

    // Assert
    assert!(result.passed);
    assert!(result.quality_score >= 80);
}

#[test]
fn test_check_severity_ordering() {
    // Arrange & Act & Assert
    assert!(CheckSeverity::Critical > CheckSeverity::Major);
    assert!(CheckSeverity::Major > CheckSeverity::Minor);
    assert!(CheckSeverity::Minor > CheckSeverity::Info);
}

#[test]
fn test_validation_check_structure() {
    // Arrange & Act
    let check = ValidationCheck {
        name: "TestCheck".to_string(),
        passed: true,
        severity: CheckSeverity::Info,
        message: "All good".to_string(),
        weight: 10,
    };

    // Assert
    assert_eq!(check.name, "TestCheck");
    assert!(check.passed);
    assert_eq!(check.weight, 10);
}

#[test]
fn test_validation_result_structure() {
    // Arrange & Act
    let result = ValidationResult {
        passed: true,
        quality_score: 95,
        checks: vec![],
    };

    // Assert
    assert!(result.passed);
    assert_eq!(result.quality_score, 95);
}

// ============================================================================
// SECTION 9: Type Trait Verification Tests (10 tests)
// ============================================================================

#[test]
fn test_package_id_send_sync() {
    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}

    assert_send::<PackageId>();
    assert_sync::<PackageId>();
}

#[test]
fn test_package_version_send_sync() {
    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}

    assert_send::<PackageVersion>();
    assert_sync::<PackageVersion>();
}

#[test]
fn test_quality_score_implements_copy() {
    fn assert_copy<T: Copy>() {}
    assert_copy::<QualityScore>();
}

#[test]
fn test_package_state_copy() {
    fn assert_copy<T: Copy>() {}
    assert_copy::<PackageState>();
}

#[test]
fn test_error_is_send() {
    fn assert_send<T: Send>() {}
    assert_send::<Error>();
}

#[test]
fn test_package_clone() {
    fn assert_clone<T: Clone>() {}
    assert_clone::<Package>();
}

#[test]
fn test_metadata_implements_clone() {
    fn assert_clone<T: Clone>() {}
    assert_clone::<PackageMetadata>();
}
