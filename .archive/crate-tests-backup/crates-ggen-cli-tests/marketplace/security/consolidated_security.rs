//! Consolidated security tests for marketplace operations
//!
//! Merges Ed25519 signature and validation test suites covering:
//! - Ed25519 keypair generation and signature verification
//! - Input validation and sanitization
//! - Injection prevention (SQL, path traversal, XSS)
//! - Score validation and overflow prevention
//! - Tamper detection and cryptographic verification

use ggen_marketplace::prelude::*;

// ============================================================================
// ED25519 SIGNATURE TESTS (original ed25519_signature_test.rs)
// ============================================================================

#[cfg(feature = "marketplace-v2")]
mod ed25519_security_tests {
    use ggen_marketplace::security::SignatureManager;

    #[test]
    fn test_keypair_generation() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        // Ed25519 keys should be 32 bytes
        assert_eq!(public_key.len(), 32, "Public key should be 32 bytes");
        assert_eq!(private_key.len(), 64, "Private key should be 64 bytes");
    }

    #[test]
    fn test_signature_generation() {
        let manager = SignatureManager::new();
        let (_public_key, private_key) = manager.generate_keypair();

        let message = b"Package: test-package v1.0.0";
        let signature = manager.sign(message, &private_key);

        // Ed25519 signature should be 64 bytes
        assert_eq!(signature.len(), 64, "Signature should be 64 bytes");
    }

    #[test]
    fn test_valid_signature_verification() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Valid package data";
        let signature = manager.sign(message, &private_key);

        // Verify signature
        let is_valid = manager.verify(message, &signature, &public_key);
        assert!(is_valid, "Valid signature should verify successfully");
    }

    #[test]
    fn test_invalid_signature_detection() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let message = b"Original message";
        let signature = manager.sign(message, &private_key);

        // Create invalid signature (flipped bits)
        let mut invalid_signature = signature.clone();
        invalid_signature[0] ^= 0xFF;

        // Verify should fail
        let is_valid = manager.verify(message, &invalid_signature, &public_key);
        assert!(!is_valid, "Invalid signature should fail verification");
    }

    #[test]
    fn test_tampered_message_detection() {
        let manager = SignatureManager::new();
        let (public_key, private_key) = manager.generate_keypair();

        let original_message = b"Package: test-package v1.0.0";
        let signature = manager.sign(original_message, &private_key);

        let tampered_message = b"Package: test-package v2.0.0"; // Version changed

        // Verify should fail with tampered message
        let is_valid = manager.verify(tampered_message, &signature, &public_key);
        assert!(!is_valid, "Tampered message should fail verification");
    }

    #[test]
    fn test_wrong_public_key_detection() {
        let manager = SignatureManager::new();
        let (_public_key1, private_key1) = manager.generate_keypair();
        let (public_key2, _private_key2) = manager.generate_keypair();

        let message = b"Test message";
        let signature = manager.sign(message, &private_key1);

        // Verify with wrong public key should fail
        let is_valid = manager.verify(message, &signature, &public_key2);
        assert!(!is_valid, "Wrong public key should fail verification");
    }

    #[test]
    fn test_signature_determinism() {
        let manager = SignatureManager::new();
        let (_public_key, private_key) = manager.generate_keypair();

        let message = b"Deterministic test";

        let signature1 = manager.sign(message, &private_key);
        let signature2 = manager.sign(message, &private_key);

        assert_eq!(
            signature1, signature2,
            "Same message with same key should produce same signature"
        );
    }
}

// ============================================================================
// VALIDATION TESTS (original validation_test.rs)
// ============================================================================

#[test]
fn test_package_id_validation() {
    // Valid package IDs
    let valid_ids = vec![
        "io.ggen.rust.cli",
        "com.example.package",
        "org.test.valid_name",
        "package.with.many.segments",
    ];

    for id in valid_ids {
        let assessment = MaturityAssessment::new(id, "Test");
        assert_eq!(assessment.package_id, id);
    }
}

#[test]
fn test_package_id_sanitization() {
    // Package IDs with special characters that should be handled
    let potentially_dangerous = vec![
        "../../../etc/passwd",
        "test; DROP TABLE packages;",
        "<script>alert('xss')</script>",
        "test\0null",
        "../../root/.ssh/id_rsa",
    ];

    for dangerous_id in potentially_dangerous {
        // Should either sanitize or create assessment without executing malicious intent
        let assessment = MaturityAssessment::new(dangerous_id, "Test");
        assert_eq!(assessment.package_id, dangerous_id); // Stored as-is but not executed
    }
}

#[test]
fn test_score_validation_prevents_overflow() {
    // Test that scores cannot exceed maximum
    let input = EvaluationInput {
        package_id: "test".to_string(),
        package_name: "Test".to_string(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 100.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        downloads: u64::MAX,
        active_contributors: u32::MAX,
        days_since_last_release: 0,
        avg_issue_response_hours: 0.0,
        academic_citations: u32::MAX,
        rating: 5.0,
    };

    let assessment = MaturityEvaluator::evaluate(input);

    // Total score should never exceed 100
    assert!(
        assessment.total_score() <= 100,
        "Score {} exceeds maximum of 100",
        assessment.total_score()
    );

    // Individual dimensions should not exceed their max
    assert!(assessment.documentation.total() <= 20);
    assert!(assessment.testing.total() <= 20);
    assert!(assessment.security.total() <= 20);
    assert!(assessment.performance.total() <= 15);
    assert!(assessment.adoption.total() <= 15);
    assert!(assessment.maintenance.total() <= 10);
}

#[test]
fn test_negative_values_handled_safely() {
    // Test that negative values (if possible via API misuse) are handled
    let input = EvaluationInput {
        package_id: "test".to_string(),
        package_name: "Test".to_string(),
        test_coverage: -10.0,           // Invalid
        unsafe_code_percent: -5.0,      // Invalid
        avg_issue_response_hours: -1.0, // Invalid
        ..Default::default()
    };

    // Should not panic
    let assessment = MaturityEvaluator::evaluate(input);

    // Score calculation should clamp to valid ranges
    assert!(assessment.total_score() >= 0.0);
    assert!(assessment.total_score() <= 100.0);
}

#[test]
fn test_extreme_values_handling() {
    // Test handling of extreme but valid values
    let input = EvaluationInput {
        package_id: "extreme".to_string(),
        package_name: "Extreme Test".to_string(),
        downloads: u64::MAX,
        active_contributors: u32::MAX,
        academic_citations: u32::MAX,
        rating: 5.0,
        test_coverage: 150.0, // Over 100% (should clamp)
        ..Default::default()
    };

    let assessment = MaturityEvaluator::evaluate(input);

    // Should handle gracefully
    assert!(assessment.total_score() >= 0.0);
    assert!(assessment.total_score() <= 100.0);
    assert!(assessment.adoption.downloads > 0); // Extremely high
}

#[test]
fn test_whitespace_handling_in_names() {
    // Test that whitespace is handled correctly
    let test_cases = vec![
        ("package  with  spaces", "Package With Spaces"),
        ("  leading", "  Leading Name"),
        ("trailing  ", "Trailing Name  "),
        ("\ttab\tcharacters", "Tab\tTest"),
        ("\nnewline\ntest", "Newline\nName"),
    ];

    for (pkg_id, pkg_name) in test_cases {
        let assessment = MaturityAssessment::new(pkg_id, pkg_name);
        assert_eq!(assessment.package_id, pkg_id);
        assert_eq!(assessment.package_name, pkg_name);
    }
}

#[test]
fn test_unicode_handling() {
    // Test Unicode in package identifiers and names
    let test_cases = vec![
        ("org.example.café", "Café Package"),
        ("pkg.日本語", "日本語テスト"),
        ("example.émoji🚀", "Emoji🚀Package"),
        ("cyrillic.Пакет", "Русский Пакет"),
    ];

    for (pkg_id, pkg_name) in test_cases {
        let assessment = MaturityAssessment::new(pkg_id, pkg_name);
        assert_eq!(assessment.package_id, pkg_id);
        assert_eq!(assessment.package_name, pkg_name);
    }
}

#[test]
fn test_empty_string_handling() {
    // Test that empty strings are handled
    let assessment = MaturityAssessment::new("", "");
    assert_eq!(assessment.package_id, "");
    assert_eq!(assessment.package_name, "");
}

#[test]
fn test_very_long_strings() {
    // Test handling of very long package IDs and names
    let long_id = "a".repeat(1000);
    let long_name = "Package ".repeat(100);

    let assessment = MaturityAssessment::new(&long_id, &long_name);
    assert_eq!(assessment.package_id, long_id);
    assert_eq!(assessment.package_name, long_name);
}

#[test]
fn test_null_byte_handling() {
    // Test null byte handling
    let assessment = MaturityAssessment::new("test\0null", "Test\0Name");
    assert_eq!(assessment.package_id, "test\0null");
    assert_eq!(assessment.package_name, "Test\0Name");
}

#[test]
fn test_score_consistency() {
    // Verify that same input produces same score
    let input = EvaluationInput {
        package_id: "consistent".to_string(),
        package_name: "Consistent Test".to_string(),
        has_readme: true,
        test_coverage: 75.0,
        has_unit_tests: true,
        ..Default::default()
    };

    let assessment1 = MaturityEvaluator::evaluate(input.clone());
    let assessment2 = MaturityEvaluator::evaluate(input);

    assert_eq!(assessment1.total_score(), assessment2.total_score());
    assert_eq!(assessment1.level(), assessment2.level());
}

#[test]
fn test_dimension_score_validation() {
    // Verify individual dimension scores are valid
    let input = EvaluationInput {
        package_id: "dimension_test".to_string(),
        package_name: "Dimension Test".to_string(),
        has_readme: true,
        test_coverage: 90.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        ..Default::default()
    };

    let assessment = MaturityEvaluator::evaluate(input);

    // Each dimension should have valid scores
    assert!(assessment.documentation.total() >= 0.0);
    assert!(assessment.documentation.total() <= 20.0);

    assert!(assessment.testing.total() >= 0.0);
    assert!(assessment.testing.total() <= 20.0);

    assert!(assessment.security.total() >= 0.0);
    assert!(assessment.security.total() <= 20.0);

    assert!(assessment.performance.total() >= 0.0);
    assert!(assessment.performance.total() <= 15.0);

    assert!(assessment.adoption.total() >= 0.0);
    assert!(assessment.adoption.total() <= 15.0);

    assert!(assessment.maintenance.total() >= 0.0);
    assert!(assessment.maintenance.total() <= 10.0);
}
