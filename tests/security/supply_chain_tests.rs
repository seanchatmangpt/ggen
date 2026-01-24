// supply_chain_tests.rs - Integration tests for supply chain security
//
// Week 8: Dependency security and supply chain protection
// Chicago TDD: State-based testing with real collaborators and behavior verification

use ggen_utils::supply_chain::{
    check_license_compliance, detect_typosquatting, levenshtein_distance,
    verify_checksum, Dependency, SupplyChainConfig,
};

#[test]
fn test_levenshtein_distance_zero_for_identical_strings() {
    // Arrange
    let s1 = "tokio";
    let s2 = "tokio";

    // Act
    let distance = levenshtein_distance(s1, s2);

    // Assert
    assert_eq!(distance, 0, "Identical strings should have distance 0");
}

#[test]
fn test_levenshtein_distance_one_for_single_char_difference() {
    // Arrange
    let s1 = "tokio";
    let s2 = "toklo";  // Changed 'i' to 'l'

    // Act
    let distance = levenshtein_distance(s1, s2);

    // Assert
    assert_eq!(distance, 1, "Single character difference should have distance 1");
}

#[test]
fn test_levenshtein_distance_handles_insertion() {
    // Arrange
    let s1 = "serde";
    let s2 = "serdes";  // Added 's' at end

    // Act
    let distance = levenshtein_distance(s1, s2);

    // Assert
    assert_eq!(distance, 1, "Single insertion should have distance 1");
}

#[test]
fn test_levenshtein_distance_handles_deletion() {
    // Arrange
    let s1 = "clapper";
    let s2 = "clap";  // Deleted "per"

    // Act
    let distance = levenshtein_distance(s1, s2);

    // Assert
    assert_eq!(distance, 3, "Three deletions should have distance 3");
}

#[test]
fn test_typosquatting_detection_finds_suffix_pattern() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "tokio_rs".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("abc123".to_string()),
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = detect_typosquatting(&dependencies, &config)
        .expect("Typosquatting detection should succeed");

    // Assert
    assert_eq!(
        result.suspicious_dependencies.len(),
        1,
        "Should detect one suspicious dependency"
    );
    assert_eq!(
        result.suspicious_dependencies[0].name,
        "tokio_rs"
    );
    assert!(
        result.suspicious_dependencies[0].reason.contains("suffix"),
        "Should identify suffix pattern"
    );
}

#[test]
fn test_typosquatting_detection_finds_prefix_pattern() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "rust_serde".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("def456".to_string()),
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = detect_typosquatting(&dependencies, &config)
        .expect("Typosquatting detection should succeed");

    // Assert
    assert_eq!(
        result.suspicious_dependencies.len(),
        1,
        "Should detect one suspicious dependency"
    );
    assert!(
        result.suspicious_dependencies[0].reason.contains("prefix"),
        "Should identify prefix pattern"
    );
}

#[test]
fn test_typosquatting_detection_finds_similar_names() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "serda".to_string(),  // Typo of "serde"
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("ghi789".to_string()),
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = detect_typosquatting(&dependencies, &config)
        .expect("Typosquatting detection should succeed");

    // Assert
    assert_eq!(
        result.suspicious_dependencies.len(),
        1,
        "Should detect one suspicious dependency"
    );
    assert_eq!(
        result.suspicious_dependencies[0].similar_to,
        Some("serde".to_string()),
        "Should identify similar crate"
    );
    assert_eq!(
        result.suspicious_dependencies[0].distance,
        Some(1),
        "Should calculate correct distance"
    );
}

#[test]
fn test_typosquatting_detection_ignores_popular_crates() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "serde".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("jkl012".to_string()),
        },
        Dependency {
            name: "tokio".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("mno345".to_string()),
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = detect_typosquatting(&dependencies, &config)
        .expect("Typosquatting detection should succeed");

    // Assert
    assert!(
        result.suspicious_dependencies.is_empty(),
        "Should not flag popular crates as suspicious"
    );
}

#[test]
fn test_license_compliance_allows_mit() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "good-crate".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: Some("pqr678".to_string()),
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = check_license_compliance(&dependencies, &config)
        .expect("License compliance check should succeed");

    // Assert
    assert!(result.compliant, "MIT license should be compliant");
    assert!(result.violations.is_empty(), "Should have no violations");
    assert_eq!(result.license_distribution.get("MIT"), Some(&1));
}

#[test]
fn test_license_compliance_denies_gpl3() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "bad-crate".to_string(),
            version: "1.0.0".to_string(),
            license: Some("GPL-3.0".to_string()),
            checksum: Some("stu901".to_string()),
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = check_license_compliance(&dependencies, &config)
        .expect("License compliance check should succeed");

    // Assert
    assert!(!result.compliant, "GPL-3.0 should not be compliant");
    assert_eq!(result.violations.len(), 1, "Should have one violation");
    assert_eq!(result.violations[0].license, "GPL-3.0");
    assert!(result.violations[0].reason.contains("denied"));
}

#[test]
fn test_license_compliance_tracks_distribution() {
    // Arrange
    let dependencies = vec![
        Dependency {
            name: "crate1".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: None,
        },
        Dependency {
            name: "crate2".to_string(),
            version: "1.0.0".to_string(),
            license: Some("MIT".to_string()),
            checksum: None,
        },
        Dependency {
            name: "crate3".to_string(),
            version: "1.0.0".to_string(),
            license: Some("Apache-2.0".to_string()),
            checksum: None,
        },
    ];
    let config = SupplyChainConfig::default();

    // Act
    let result = check_license_compliance(&dependencies, &config)
        .expect("License compliance check should succeed");

    // Assert
    assert_eq!(result.license_distribution.get("MIT"), Some(&2));
    assert_eq!(result.license_distribution.get("Apache-2.0"), Some(&1));
}

#[test]
fn test_checksum_verification_succeeds_on_match() {
    // Arrange
    let dependency = Dependency {
        name: "verified-crate".to_string(),
        version: "1.0.0".to_string(),
        license: Some("MIT".to_string()),
        checksum: Some("abc123def456".to_string()),
    };
    let expected = "abc123def456";

    // Act
    let result = verify_checksum(&dependency, expected)
        .expect("Checksum verification should succeed");

    // Assert
    assert!(result, "Matching checksums should verify successfully");
}

#[test]
fn test_checksum_verification_fails_on_mismatch() {
    // Arrange
    let dependency = Dependency {
        name: "tampered-crate".to_string(),
        version: "1.0.0".to_string(),
        license: Some("MIT".to_string()),
        checksum: Some("abc123def456".to_string()),
    };
    let expected = "xyz789uvw012";

    // Act
    let result = verify_checksum(&dependency, expected)
        .expect("Checksum verification should succeed");

    // Assert
    assert!(!result, "Mismatched checksums should fail verification");
}

#[test]
fn test_checksum_verification_errors_on_missing_checksum() {
    // Arrange
    let dependency = Dependency {
        name: "no-checksum-crate".to_string(),
        version: "1.0.0".to_string(),
        license: Some("MIT".to_string()),
        checksum: None,
    };
    let expected = "abc123def456";

    // Act
    let result = verify_checksum(&dependency, expected);

    // Assert
    assert!(result.is_err(), "Missing checksum should return error");
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("no checksum"), "Error should mention missing checksum");
}
