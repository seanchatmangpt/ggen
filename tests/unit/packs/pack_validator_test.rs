//! Pack Validator Tests - Unit Level
//!
//! Tests the PackValidator component:
//! - Validating pack manifests
//! - Checking completeness
//! - Scoring packs

// Re-pointed (T053): `validate_pack`/`show_pack`/`ValidationResult`/
// `ValidationCheck` have a real, WORKING port destination in
// `ggen_marketplace::packs_registry::{validate,metadata}` (T025).
// `score_pack`/`PackScore`/`score::DimensionScores` are pointed at their
// correct eventual home, `ggen_marketplace::packs_registry::score`, but this
// does NOT currently resolve (same as it did NOT resolve via
// `ggen_core::domain::packs::score_pack` before this change either --
// confirmed via `cargo check`: `ggen-core/src/domain/packs/mod.rs` also
// lacks `pub mod score;`, so this was pre-existing dead code, not something
// this migration broke): `packs_registry/score.rs` exists on disk in the
// T025 port but is NOT declared `pub mod score;` in
// `crates/ggen-marketplace/src/packs_registry/mod.rs` (confirmed: it's one
// of several files -- also `advanced_resolver.rs`, `cloud_distribution.rs`,
// `composer.rs`, `installer.rs`, `sparql_executor.rs`, `template_generator.rs`
// -- physically present but not wired into the module tree). Fixing that
// one-line wiring gap requires editing ggen-marketplace source, out of this
// task's file scope (root package only) -- flagged in tasks.md for whoever
// owns that crate next. This file is not part of any compiled `cargo test`
// target today regardless (verified via `cargo metadata`'s test-target
// list), so nothing regresses either way.
use ggen_marketplace::packs_registry::metadata::show_pack;
use ggen_marketplace::packs_registry::score::score_pack;
use ggen_marketplace::packs_registry::validate::validate_pack;

#[test]
fn test_validate_pack_structure() {
    // Act
    let result = validate_pack("startup-essentials");

    // Assert
    match result {
        Ok(validation) => {
            assert_eq!(validation.pack_id, "startup-essentials");
            assert!(validation.score >= 0.0 && validation.score <= 100.0);
            assert!(validation.checks.is_empty() == false, "Should have validation checks");
        }
        Err(e) => {
            // Expected if pack doesn't exist
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory")
            );
        }
    }
}

#[test]
fn test_validate_nonexistent_pack_fails() {
    // Act
    let result = validate_pack("nonexistent-pack-xyz");

    // Assert
    assert!(result.is_err(), "Should fail for nonexistent pack");
    let error = result.unwrap_err();
    assert!(
        error.to_string().contains("not found"),
        "Error should indicate pack not found"
    );
}

#[test]
fn test_validate_pack_has_checks() {
    // Act
    let result = validate_pack("startup-essentials");

    // Assert
    if let Ok(validation) = result {
        // Should have various checks
        assert!(validation.checks.is_empty() == false);

        // Each check should have a name and message
        for check in &validation.checks {
            assert!(!check.name.is_empty(), "Check should have a name");
            assert!(!check.message.is_empty(), "Check should have a message");
        }
    }
}

#[test]
fn test_validate_pack_errors_and_warnings() {
    // Act
    let result = validate_pack("startup-essentials");

    // Assert
    if let Ok(validation) = result {
        // Errors and warnings should be lists
        assert!(validation.errors.len() >= 0);
        assert!(validation.warnings.len() >= 0);

        // If valid=true, should have no errors
        if validation.valid {
            assert_eq!(
                validation.errors.len(),
                0,
                "Valid pack should have no errors"
            );
        }
    }
}

#[test]
fn test_score_pack_returns_dimensions() {
    // Arrange - First get pack
    let pack_result = show_pack("startup-essentials");

    // Act
    if let Ok(pack) = pack_result {
        let score_result = score_pack(&pack);

        // Assert
        match score_result {
            Ok(score) => {
                assert_eq!(score.pack_id, "startup-essentials");
                assert!(score.total_score <= 100, "Total score should be <= 100");
                assert!(!score.maturity_level.is_empty(), "Should have maturity level");

                // Check dimension scores
                assert!(score.scores.documentation <= 100);
                assert!(score.scores.completeness <= 100);
                assert!(score.scores.quality <= 100);
                assert!(score.scores.usability <= 100);

                // Should have feedback
                assert!(score.feedback.len() >= 0);
            }
            Err(_e) => {
                // Expected if scoring logic not fully implemented
            }
        }
    }
}

#[test]
fn test_score_pack_production_ready_affects_score() {
    // Arrange
    let pack_result = show_pack("startup-essentials");

    // Act & Assert
    if let Ok(pack) = pack_result {
        if let Ok(score) = score_pack(&pack) {
            // Production ready packs should have reasonable scores
            if pack.production_ready {
                assert!(
                    score.total_score > 50,
                    "Production ready pack should have score > 50"
                );
            }
        }
    }
}

#[test]
fn test_score_pack_with_metadata() {
    // Arrange
    let pack_result = show_pack("data-science-toolkit");

    // Act
    if let Ok(pack) = pack_result {
        // Data science toolkit has metadata
        if let Ok(score) = score_pack(&pack) {
            // Should consider metadata in scoring
            assert!(score.scores.completeness > 0);
            assert!(score.scores.documentation > 0);
        }
    }
}

#[test]
fn test_score_maturity_levels() {
    // Test that maturity levels are calculated correctly
    let pack_result = show_pack("startup-essentials");

    if let Ok(pack) = pack_result {
        if let Ok(score) = score_pack(&pack) {
            // Maturity level should be one of the expected values
            let valid_levels = vec!["Alpha", "Beta", "Production", "Mature", "Experimental"];
            assert!(
                valid_levels.iter().any(|&level| score.maturity_level.contains(level)),
                "Maturity level should be one of: {:?}, got: {}",
                valid_levels,
                score.maturity_level
            );
        }
    }
}

#[test]
fn test_validate_pack_with_dependencies() {
    // Test validation of a pack that might have dependencies
    let result = validate_pack("startup-essentials");

    if let Ok(validation) = result {
        // Validation should check dependencies
        let has_dep_check = validation
            .checks
            .iter()
            .any(|c| c.name.to_lowercase().contains("depend"));

        // If pack has dependencies, should validate them
        // This is a weak assertion since not all packs have dependencies
        assert!(validation.checks.is_empty() == false);
    }
}

#[test]
fn test_validation_result_serialization() {
    use ggen_marketplace::packs_registry::validate::ValidationResult;

    // Arrange
    let validation = ValidationResult {
        pack_id: "test-pack".to_string(),
        valid: true,
        score: 85.5,
        errors: vec![],
        warnings: vec!["Minor warning".to_string()],
        checks: vec![ggen_marketplace::packs_registry::validate::ValidationCheck {
            name: "test_check".to_string(),
            passed: true,
            message: "Check passed".to_string(),
        }],
    };

    // Act
    let json = serde_json::to_string(&validation).expect("Should serialize");
    let deserialized: ValidationResult = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_id, "test-pack");
    assert_eq!(deserialized.valid, true);
    assert_eq!(deserialized.score, 85.5);
    assert_eq!(deserialized.warnings.len(), 1);
    assert_eq!(deserialized.checks.len(), 1);
}

#[test]
fn test_pack_score_serialization() {
    use ggen_marketplace::packs_registry::score::PackScore;

    // Arrange
    let score = PackScore {
        pack_id: "test-pack".to_string(),
        total_score: 75,
        maturity_level: "Beta".to_string(),
        scores: ggen_marketplace::packs_registry::score::DimensionScores {
            documentation: 20,
            completeness: 25,
            quality: 20,
            usability: 10,
        },
        feedback: vec!["Good documentation".to_string(), "Needs more tests".to_string()],
    };

    // Act
    let json = serde_json::to_string(&score).expect("Should serialize");
    let deserialized: PackScore = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_id, "test-pack");
    assert_eq!(deserialized.total_score, 75);
    assert_eq!(deserialized.maturity_level, "Beta");
    assert_eq!(deserialized.feedback.len(), 2);
}
