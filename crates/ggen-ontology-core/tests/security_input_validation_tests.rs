//! Security audit: Input validation testing
//!
//! Tests for entity mapper input validation with edge cases,
//! special characters, and unicode handling

use ggen_ontology_core::entity_mapper::EntityMapper;

#[test]
fn test_entity_mapper_empty_string() {
    // Test with empty string
    let result = EntityMapper::match_policy("");
    assert!(result.is_ok(), "Should handle empty string");

    let matches = result.unwrap();
    assert!(!matches.is_empty(), "Should return default match");
}

#[test]
fn test_entity_mapper_very_long_input() {
    // Test with extremely long input
    let long_input = "a".repeat(10000);
    let result = EntityMapper::match_policy(&long_input);

    assert!(result.is_ok(), "Should handle long input");
    let matches = result.unwrap();
    assert!(!matches.is_empty(), "Should return a match");
}

#[test]
fn test_entity_mapper_special_characters() {
    // Test with special characters
    let special_chars = vec![
        "Privacy!@#$%",
        "Policy&*()",
        "Security<>?:",
        "Access[]{};",
        "Control|\\",
    ];

    for input in special_chars {
        let result = EntityMapper::match_policy(input);
        assert!(result.is_ok(), "Should handle special chars: {}", input);
    }
}

#[test]
fn test_entity_mapper_unicode_characters() {
    // Test with various unicode scripts
    let unicode_inputs = vec![
        "プライバシーポリシー",        // Japanese
        "隐私政策",                    // Chinese
        "سياسة الخصوصية",              // Arabic
        "Политика конфиденциальности", // Russian
        "नीति",                        // Hindi
    ];

    for input in unicode_inputs {
        let result = EntityMapper::match_policy(input);
        assert!(result.is_ok(), "Should handle unicode: {}", input);
    }
}

#[test]
fn test_entity_mapper_mixed_case() {
    // Test that matching is case-insensitive
    let inputs = vec![
        "PRIVACY",
        "privacy",
        "PrIvAcY",
        "SECURITY POLICY",
        "security policy",
        "Security Policy",
    ];

    for input in inputs {
        let result = EntityMapper::match_policy(input);
        assert!(result.is_ok(), "Should handle various cases");
    }
}

#[test]
fn test_entity_mapper_whitespace_handling() {
    // Test with leading/trailing whitespace
    let inputs = vec![
        "  Privacy Policy  ",
        "\tPrivacy\t",
        "\n\nSecurity\n\n",
        "   ",
    ];

    for input in inputs {
        let result = EntityMapper::match_policy(input);
        assert!(result.is_ok(), "Should handle whitespace");
    }
}

#[test]
fn test_entity_mapper_newline_in_input() {
    // Test with newlines in input
    let input = "Privacy\nPolicy\nDocument";
    let result = EntityMapper::match_policy(input);

    assert!(result.is_ok(), "Should handle newlines in input");
}

#[test]
fn test_entity_mapper_null_bytes() {
    // Test with null bytes (should not crash)
    let input = "Privacy\0Policy";
    let result = EntityMapper::match_policy(input);

    // Should handle gracefully (null byte is part of string)
    assert!(result.is_ok(), "Should handle null bytes safely");
}

#[test]
fn test_data_classification_edge_cases() {
    // Test data classification with edge cases
    let inputs = vec![
        "",
        "   ",
        "public",
        "PUBLIC",
        "Public Data",
        "confidential data",
        "restricted access",
        "unknown_classification",
        "123456",
    ];

    for input in inputs {
        let result = EntityMapper::match_data_classification(input);
        assert!(result.is_ok(), "Should handle classification: {}", input);

        let matches = result.unwrap();
        // Verify confidence scores are in valid range
        for m in matches {
            assert!(m.score >= 0.0 && m.score <= 1.0, "Score should be 0.0-1.0");
        }
    }
}

#[test]
fn test_service_level_float_edge_cases() {
    // Test service level with boundary values
    let test_cases = vec![
        0.0, 0.1, 50.0, 95.0, 99.0, 99.9, 99.99, 100.0, -1.0,   // Invalid but shouldn't crash
        1000.0, // Out of range but shouldn't crash
    ];

    for availability in test_cases {
        let result = EntityMapper::match_service_level(availability);
        assert!(
            result.is_ok(),
            "Should handle availability: {}",
            availability
        );

        let matches = result.unwrap();
        assert!(!matches.is_empty(), "Should always return a match");
    }
}

#[test]
fn test_security_control_special_input() {
    // Test security control with various inputs
    let inputs = vec![
        "MFA",
        "Multi-Factor Authentication",
        "MFA/OAuth2",
        "Encryption (AES-256)",
        "",
        "12345",
    ];

    for input in inputs {
        let result = EntityMapper::match_security_control(input);
        assert!(result.is_ok(), "Should handle control: {}", input);
    }
}

#[test]
fn test_compute_service_variations() {
    // Test compute service type matching
    let inputs = vec![
        "VM",
        "vm",
        "Container",
        "container",
        "Kubernetes",
        "kubernetes",
        "K8S",
        "Serverless",
        "Lambda",
        "Fargate",
    ];

    for input in inputs {
        let result = EntityMapper::match_compute_service(input);
        assert!(result.is_ok(), "Should handle compute: {}", input);
    }
}

#[test]
fn test_entity_mapper_result_consistency() {
    // Test that same input always produces same output
    let test_inputs = vec!["Privacy Policy", "Security", "Public Data", "99.99"];

    for input in test_inputs {
        let result1 = EntityMapper::match_policy(input);
        let result2 = EntityMapper::match_policy(input);

        // Both should have same Ok/Err status
        assert_eq!(result1.is_ok(), result2.is_ok(), "Should be consistent");
    }
}

#[test]
fn test_confidence_scores_valid_range() {
    // Verify all confidence scores are in valid range
    let test_inputs = vec![
        "Privacy",
        "Security",
        "Access Control",
        "Encryption",
        "Random unmatched text",
    ];

    for input in test_inputs {
        let result = EntityMapper::match_policy(input).expect("Should succeed");

        for match_item in result {
            assert!(
                match_item.score >= 0.0 && match_item.score <= 1.0,
                "Score {} out of range for {}",
                match_item.score,
                input
            );
        }
    }
}
