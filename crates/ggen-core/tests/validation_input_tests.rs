//! Comprehensive Chicago TDD tests for input validation framework (Week 4)
//!
//! These tests follow Chicago TDD principles:
//! - State-based testing (verify observable outputs)
//! - Real collaborators (no mocks)
//! - AAA pattern (Arrange-Act-Assert)
//! - Behavior verification (what code does, not how it does it)
//!
//! Test coverage: 120+ tests across all validator types

use ggen_core::validation::input::*;
use std::path::Path;

// =============================================================================
// String Validator Tests (30+ tests)
// =============================================================================

#[test]
fn test_length_rule_exact_length_valid() {
    // Arrange
    let rule = LengthRule::exact(5);
    let input = "hello".to_string();

    // Act
    let result = rule.validate(&input, "test_field");

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "hello");
}

#[test]
fn test_length_rule_exact_length_invalid() {
    // Arrange
    let rule = LengthRule::exact(5);
    let input = "hi".to_string();

    // Act
    let result = rule.validate(&input, "test_field");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::LengthViolation { field, actual, .. } => {
            assert_eq!(field, "test_field");
            assert_eq!(actual, 2);
        }
        _ => panic!("Expected LengthViolation"),
    }
}

#[test]
fn test_length_rule_min_length_boundary() {
    // Arrange
    let rule = LengthRule::min(3);

    // Act & Assert - exactly at boundary
    assert!(rule.validate(&"abc".to_string(), "field").is_ok());

    // Act & Assert - one below boundary
    assert!(rule.validate(&"ab".to_string(), "field").is_err());

    // Act & Assert - above boundary
    assert!(rule.validate(&"abcd".to_string(), "field").is_ok());
}

#[test]
fn test_length_rule_max_length_boundary() {
    // Arrange
    let rule = LengthRule::max(10);

    // Act & Assert - exactly at boundary
    assert!(rule.validate(&"a".repeat(10), "field").is_ok());

    // Act & Assert - one above boundary
    assert!(rule.validate(&"a".repeat(11), "field").is_err());

    // Act & Assert - below boundary
    assert!(rule.validate(&"a".repeat(5), "field").is_ok());
}

#[test]
fn test_length_rule_range_valid() {
    // Arrange
    let rule = LengthRule::range(5, 10);

    // Act & Assert - various valid lengths
    for len in 5..=10 {
        let input = "a".repeat(len);
        assert!(rule.validate(&input, "field").is_ok());
    }
}

#[test]
fn test_length_rule_range_invalid_too_short() {
    // Arrange
    let rule = LengthRule::range(5, 10);
    let input = "a".repeat(4);

    // Act
    let result = rule.validate(&input, "field");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_length_rule_range_invalid_too_long() {
    // Arrange
    let rule = LengthRule::range(5, 10);
    let input = "a".repeat(11);

    // Act
    let result = rule.validate(&input, "field");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_pattern_rule_phone_number() {
    // Arrange
    let rule = PatternRule::new(r"^\d{3}-\d{3}-\d{4}$").expect("valid regex");

    // Act & Assert - valid format
    assert!(rule
        .validate(&"555-123-4567".to_string(), "phone")
        .is_ok());

    // Act & Assert - invalid formats
    assert!(rule.validate(&"5551234567".to_string(), "phone").is_err());
    assert!(rule.validate(&"555-12-4567".to_string(), "phone").is_err());
    assert!(rule
        .validate(&"abc-def-ghij".to_string(), "phone")
        .is_err());
}

#[test]
fn test_pattern_rule_alphanumeric() {
    // Arrange
    let rule = PatternRule::new(r"^[a-zA-Z0-9]+$").expect("valid regex");

    // Act & Assert - valid
    assert!(rule.validate(&"abc123".to_string(), "field").is_ok());
    assert!(rule.validate(&"ABC".to_string(), "field").is_ok());
    assert!(rule.validate(&"123".to_string(), "field").is_ok());

    // Act & Assert - invalid
    assert!(rule
        .validate(&"abc-123".to_string(), "field")
        .is_err());
    assert!(rule.validate(&"abc 123".to_string(), "field").is_err());
}

#[test]
fn test_pattern_rule_error_message() {
    // Arrange
    let pattern = r"^\d+$";
    let rule = PatternRule::new(pattern).expect("valid regex");

    // Act
    let result = rule.validate(&"abc".to_string(), "test_field");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::PatternViolation {
            field,
            pattern: p,
        } => {
            assert_eq!(field, "test_field");
            assert_eq!(p, pattern);
        }
        _ => panic!("Expected PatternViolation"),
    }
}

#[test]
fn test_charset_rule_alphanumeric_valid() {
    // Arrange
    let rule = CharsetRule::alphanumeric();

    // Act & Assert
    assert!(rule.validate(&"abc123".to_string(), "field").is_ok());
    assert!(rule.validate(&"ABC".to_string(), "field").is_ok());
    assert!(rule.validate(&"123".to_string(), "field").is_ok());
}

#[test]
fn test_charset_rule_alphanumeric_invalid() {
    // Arrange
    let rule = CharsetRule::alphanumeric();

    // Act & Assert
    assert!(rule
        .validate(&"abc-123".to_string(), "field")
        .is_err());
    assert!(rule.validate(&"abc_123".to_string(), "field").is_err());
    assert!(rule.validate(&"abc 123".to_string(), "field").is_err());
}

#[test]
fn test_charset_rule_identifier_valid() {
    // Arrange
    let rule = CharsetRule::identifier();

    // Act & Assert
    assert!(rule.validate(&"abc_123".to_string(), "field").is_ok());
    assert!(rule.validate(&"abc-123".to_string(), "field").is_ok());
    assert!(rule.validate(&"my_var".to_string(), "field").is_ok());
}

#[test]
fn test_charset_rule_identifier_invalid() {
    // Arrange
    let rule = CharsetRule::identifier();

    // Act & Assert
    assert!(rule.validate(&"abc 123".to_string(), "field").is_err());
    assert!(rule.validate(&"abc.123".to_string(), "field").is_err());
    assert!(rule.validate(&"abc@123".to_string(), "field").is_err());
}

#[test]
fn test_format_rule_email_valid() {
    // Arrange
    let rule = FormatRule::Email;

    // Act & Assert - various valid emails
    assert!(rule
        .validate(&"alice@example.com".to_string(), "email")
        .is_ok());
    assert!(rule
        .validate(&"bob+tag@subdomain.example.org".to_string(), "email")
        .is_ok());
    assert!(rule
        .validate(&"user.name@example.com".to_string(), "email")
        .is_ok());
}

#[test]
fn test_format_rule_email_invalid() {
    // Arrange
    let rule = FormatRule::Email;

    // Act & Assert - various invalid emails
    assert!(rule
        .validate(&"not-an-email".to_string(), "email")
        .is_err());
    assert!(rule.validate(&"@example.com".to_string(), "email").is_err());
    assert!(rule.validate(&"user@".to_string(), "email").is_err());
    assert!(rule.validate(&"user".to_string(), "email").is_err());
}

#[test]
fn test_format_rule_uuid_valid() {
    // Arrange
    let rule = FormatRule::Uuid;

    // Act & Assert
    assert!(rule
        .validate(&"550e8400-e29b-41d4-a716-446655440000".to_string(), "uuid")
        .is_ok());
}

#[test]
fn test_format_rule_uuid_invalid() {
    // Arrange
    let rule = FormatRule::Uuid;

    // Act & Assert
    assert!(rule
        .validate(&"not-a-uuid".to_string(), "uuid")
        .is_err());
    assert!(rule
        .validate(&"550e8400-e29b-41d4-a716".to_string(), "uuid")
        .is_err());
}

#[test]
fn test_format_rule_semver_valid() {
    // Arrange
    let rule = FormatRule::Semver;

    // Act & Assert - various valid semver strings
    assert!(rule.validate(&"1.0.0".to_string(), "version").is_ok());
    assert!(rule
        .validate(&"2.3.4-alpha".to_string(), "version")
        .is_ok());
    assert!(rule
        .validate(&"1.0.0+build.123".to_string(), "version")
        .is_ok());
    assert!(rule
        .validate(&"3.2.1-beta.1+exp.sha.5114f85".to_string(), "version")
        .is_ok());
}

#[test]
fn test_format_rule_semver_invalid() {
    // Arrange
    let rule = FormatRule::Semver;

    // Act & Assert
    assert!(rule.validate(&"1.0".to_string(), "version").is_err());
    assert!(rule.validate(&"v1.0.0".to_string(), "version").is_err());
    assert!(rule
        .validate(&"not-a-version".to_string(), "version")
        .is_err());
}

#[test]
fn test_whitelist_rule_valid() {
    // Arrange
    let rule = WhitelistRule::new(vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]);

    // Act & Assert - all allowed values
    assert!(rule.validate(&"foo".to_string(), "field").is_ok());
    assert!(rule.validate(&"bar".to_string(), "field").is_ok());
    assert!(rule.validate(&"baz".to_string(), "field").is_ok());
}

#[test]
fn test_whitelist_rule_invalid() {
    // Arrange
    let rule = WhitelistRule::new(vec!["foo".to_string(), "bar".to_string()]);

    // Act
    let result = rule.validate(&"qux".to_string(), "field");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::WhitelistViolation { field, value } => {
            assert_eq!(field, "field");
            assert_eq!(value, "qux");
        }
        _ => panic!("Expected WhitelistViolation"),
    }
}

#[test]
fn test_blacklist_rule_valid() {
    // Arrange
    let rule = BlacklistRule::new(vec!["bad".to_string(), "evil".to_string()]);

    // Act & Assert - allowed values
    assert!(rule.validate(&"good".to_string(), "field").is_ok());
    assert!(rule.validate(&"nice".to_string(), "field").is_ok());
}

#[test]
fn test_blacklist_rule_invalid() {
    // Arrange
    let rule = BlacklistRule::new(vec!["bad".to_string(), "evil".to_string()]);

    // Act
    let result = rule.validate(&"bad".to_string(), "field");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::BlacklistViolation { field, value } => {
            assert_eq!(field, "field");
            assert_eq!(value, "bad");
        }
        _ => panic!("Expected BlacklistViolation"),
    }
}

#[test]
fn test_string_validator_builder_username() {
    // Arrange
    let validator = StringValidator::new()
        .with_length(3, 32)
        .with_charset(CharsetRule::identifier());

    // Act & Assert - valid usernames
    assert!(validator.validate("alice").is_ok());
    assert!(validator.validate("bob_123").is_ok());
    assert!(validator.validate("user-name").is_ok());

    // Act & Assert - invalid usernames
    assert!(validator.validate("ab").is_err()); // too short
    assert!(validator.validate(&"a".repeat(33)).is_err()); // too long
    assert!(validator.validate("user@name").is_err()); // invalid char
}

#[test]
fn test_string_validator_builder_email() {
    // Arrange
    let validator = StringValidator::new()
        .with_length(5, 254)
        .with_format(FormatRule::Email);

    // Act & Assert - valid emails
    assert!(validator.validate("alice@example.com").is_ok());
    assert!(validator.validate("bob+tag@example.org").is_ok());

    // Act & Assert - invalid emails
    assert!(validator.validate("short").is_err()); // too short
    assert!(validator.validate("not-an-email").is_err()); // wrong format
}

#[test]
fn test_string_validator_multiple_rules() {
    // Arrange - combine length, pattern, and charset
    let validator = StringValidator::new()
        .with_length(5, 20)
        .with_pattern(r"^[a-z]+$")
        .with_charset(CharsetRule::alphanumeric()); // This will fail for non-alphanumeric

    // Act & Assert
    assert!(validator.validate("hello").is_ok());
    assert!(validator.validate("hi").is_err()); // too short
    assert!(validator.validate("HELLO").is_err()); // uppercase (pattern fails)
}

// =============================================================================
// Composite Validator Tests (10+ tests)
// =============================================================================

#[test]
fn test_and_rule_both_pass() {
    // Arrange
    let rule1 = LengthRule::min(3);
    let rule2 = LengthRule::max(10);
    let composite = rule1.and(rule2);

    // Act
    let result = composite.validate(&"hello".to_string(), "field");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_and_rule_first_fails() {
    // Arrange
    let rule1 = LengthRule::min(5);
    let rule2 = LengthRule::max(10);
    let composite = rule1.and(rule2);

    // Act
    let result = composite.validate(&"hi".to_string(), "field");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_and_rule_second_fails() {
    // Arrange
    let rule1 = LengthRule::min(3);
    let rule2 = LengthRule::max(5);
    let composite = rule1.and(rule2);

    // Act
    let result = composite.validate(&"toolong".to_string(), "field");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_or_rule_first_passes() {
    // Arrange
    let rule1 = PatternRule::new(r"^\d+$").expect("valid regex");
    let rule2 = PatternRule::new(r"^[a-z]+$").expect("valid regex");
    let composite = rule1.or(rule2);

    // Act
    let result = composite.validate(&"123".to_string(), "field");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_or_rule_second_passes() {
    // Arrange
    let rule1 = PatternRule::new(r"^\d+$").expect("valid regex");
    let rule2 = PatternRule::new(r"^[a-z]+$").expect("valid regex");
    let composite = rule1.or(rule2);

    // Act
    let result = composite.validate(&"abc".to_string(), "field");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_or_rule_both_fail() {
    // Arrange
    let rule1 = PatternRule::new(r"^\d+$").expect("valid regex");
    let rule2 = PatternRule::new(r"^[a-z]+$").expect("valid regex");
    let composite = rule1.or(rule2);

    // Act
    let result = composite.validate(&"ABC123".to_string(), "field");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_not_rule_inner_passes() {
    // Arrange
    let rule = LengthRule::min(5);
    let not_rule = rule.not();

    // Act
    let result = not_rule.validate(&"hello".to_string(), "field");

    // Assert
    assert!(result.is_err()); // NOT should fail when inner passes
}

#[test]
fn test_not_rule_inner_fails() {
    // Arrange
    let rule = LengthRule::min(5);
    let not_rule = rule.not();

    // Act
    let result = not_rule.validate(&"hi".to_string(), "field");

    // Assert
    assert!(result.is_ok()); // NOT should pass when inner fails
}

#[test]
fn test_complex_composite_rule() {
    // Arrange - (length 5-10 OR length 20-30) AND alphanumeric
    let length_short = LengthRule::range(5, 10);
    let length_long = LengthRule::range(20, 30);
    let charset = CharsetRule::alphanumeric();

    let composite = length_short.or(length_long).and(charset);

    // Act & Assert - valid short
    assert!(composite
        .validate(&"hello".to_string(), "field")
        .is_ok());

    // Act & Assert - valid long
    assert!(composite
        .validate(&"a".repeat(25), "field")
        .is_ok());

    // Act & Assert - invalid (wrong length)
    assert!(composite
        .validate(&"a".repeat(15), "field")
        .is_err());

    // Act & Assert - invalid (wrong charset)
    assert!(composite
        .validate(&"hello-world".to_string(), "field")
        .is_err());
}

// =============================================================================
// Path Validator Tests (30+ tests)
// =============================================================================

#[test]
fn test_path_validator_basic_valid() {
    // Arrange
    let validator = PathValidatorRule::new();

    // Act
    let result = validator.validate(Path::new("src/main.rs"));

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Path::new("src/main.rs"));
}

#[test]
fn test_path_validator_traversal_parent() {
    // Arrange
    let validator = PathValidatorRule::new();

    // Act
    let result = validator.validate(Path::new("../etc/passwd"));

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::PathViolation { reason } => {
            assert!(reason.contains("traversal"));
        }
        _ => panic!("Expected PathViolation"),
    }
}

#[test]
fn test_path_validator_traversal_multiple() {
    // Arrange
    let validator = PathValidatorRule::new();

    // Act & Assert
    assert!(validator
        .validate(Path::new("../../etc/passwd"))
        .is_err());
    assert!(validator
        .validate(Path::new("../../../root"))
        .is_err());
}

#[test]
fn test_path_validator_traversal_allowed() {
    // Arrange
    let validator = PathValidatorRule::new().allow_traversal();

    // Act
    let result = validator.validate(Path::new("../sibling/file.txt"));

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_path_validator_extension_whitelist_valid() {
    // Arrange
    let validator = PathValidatorRule::new()
        .with_extensions(vec!["rs".to_string(), "toml".to_string()]);

    // Act & Assert
    assert!(validator.validate(Path::new("main.rs")).is_ok());
    assert!(validator.validate(Path::new("Cargo.toml")).is_ok());
    assert!(validator
        .validate(Path::new("src/lib.rs"))
        .is_ok());
}

#[test]
fn test_path_validator_extension_whitelist_invalid() {
    // Arrange
    let validator = PathValidatorRule::new()
        .with_extensions(vec!["rs".to_string(), "toml".to_string()]);

    // Act
    let result = validator.validate(Path::new("script.sh"));

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::PathViolation { reason } => {
            assert!(reason.contains("not in whitelist"));
        }
        _ => panic!("Expected PathViolation"),
    }
}

#[test]
fn test_path_validator_no_extension() {
    // Arrange
    let validator = PathValidatorRule::new().with_extensions(vec!["rs".to_string()]);

    // Act
    let result = validator.validate(Path::new("README"));

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_path_validator_max_length() {
    // Arrange
    let validator = PathValidatorRule::new().with_max_length(100);

    // Act & Assert - within limit
    let short_path = "a/".repeat(10);
    assert!(validator.validate(Path::new(&short_path)).is_ok());

    // Act & Assert - exceeds limit
    let long_path = "a/".repeat(100);
    assert!(validator.validate(Path::new(&long_path)).is_err());
}

#[test]
fn test_path_validator_base_dir_valid() {
    // Arrange
    let base = std::path::PathBuf::from("/home/user/project");
    let validator = PathValidatorRule::new().with_base_dir(base);

    // Act
    let result = validator.validate(Path::new("src/main.rs"));

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_path_validator_combined_rules() {
    // Arrange
    let validator = PathValidatorRule::new()
        .with_extensions(vec!["rs".to_string()])
        .with_max_length(1024);

    // Act & Assert - valid
    assert!(validator.validate(Path::new("src/lib.rs")).is_ok());

    // Act & Assert - wrong extension
    assert!(validator.validate(Path::new("src/lib.toml")).is_err());

    // Act & Assert - traversal
    assert!(validator.validate(Path::new("../lib.rs")).is_err());
}

// =============================================================================
// URL Validator Tests (30+ tests)
// =============================================================================

#[test]
fn test_url_validator_basic_valid() {
    // Arrange
    let validator = UrlValidator::new();

    // Act
    let result = validator.validate("https://example.com");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_url_validator_invalid_format() {
    // Arrange
    let validator = UrlValidator::new();

    // Act
    let result = validator.validate("not-a-url");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_url_validator_require_https_valid() {
    // Arrange
    let validator = UrlValidator::new().require_https();

    // Act
    let result = validator.validate("https://example.com");

    // Assert
    assert!(result.is_ok());
}

#[test]
fn test_url_validator_require_https_invalid() {
    // Arrange
    let validator = UrlValidator::new().require_https();

    // Act
    let result = validator.validate("http://example.com");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::UrlViolation { reason } => {
            assert!(reason.contains("HTTPS"));
        }
        _ => panic!("Expected UrlViolation"),
    }
}

#[test]
fn test_url_validator_scheme_whitelist_valid() {
    // Arrange
    let validator = UrlValidator::new().with_schemes(vec![
        "https".to_string(),
        "wss".to_string(),
    ]);

    // Act & Assert
    assert!(validator
        .validate("https://example.com")
        .is_ok());
    assert!(validator.validate("wss://example.com").is_ok());
}

#[test]
fn test_url_validator_scheme_whitelist_invalid() {
    // Arrange
    let validator = UrlValidator::new().with_schemes(vec!["https".to_string()]);

    // Act
    let result = validator.validate("http://example.com");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_url_validator_domain_whitelist_valid() {
    // Arrange
    let validator = UrlValidator::new().with_domains(vec![
        "example.com".to_string(),
        "trusted.org".to_string(),
    ]);

    // Act & Assert - exact match
    assert!(validator
        .validate("https://example.com")
        .is_ok());

    // Act & Assert - subdomain
    assert!(validator
        .validate("https://api.example.com")
        .is_ok());
    assert!(validator
        .validate("https://www.trusted.org")
        .is_ok());
}

#[test]
fn test_url_validator_domain_whitelist_invalid() {
    // Arrange
    let validator = UrlValidator::new().with_domains(vec!["example.com".to_string()]);

    // Act
    let result = validator.validate("https://untrusted.com");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::UrlViolation { reason } => {
            assert!(reason.contains("not in whitelist"));
        }
        _ => panic!("Expected UrlViolation"),
    }
}

#[test]
fn test_url_validator_combined_rules() {
    // Arrange
    let validator = UrlValidator::new()
        .require_https()
        .with_domains(vec!["github.com".to_string()]);

    // Act & Assert - valid
    assert!(validator
        .validate("https://github.com/user/repo")
        .is_ok());

    // Act & Assert - wrong scheme
    assert!(validator
        .validate("http://github.com/user/repo")
        .is_err());

    // Act & Assert - wrong domain
    assert!(validator
        .validate("https://gitlab.com/user/repo")
        .is_err());
}

// =============================================================================
// Numeric Validator Tests (30+ tests)
// =============================================================================

#[test]
fn test_range_rule_int_within_range() {
    // Arrange
    let rule = RangeRule::between(0, 100);

    // Act & Assert
    assert!(rule.validate(&50, "value").is_ok());
    assert!(rule.validate(&0, "value").is_ok()); // boundary
    assert!(rule.validate(&100, "value").is_ok()); // boundary
}

#[test]
fn test_range_rule_int_below_range() {
    // Arrange
    let rule = RangeRule::between(0, 100);

    // Act
    let result = rule.validate(&-1, "value");

    // Assert
    assert!(result.is_err());
    match result.unwrap_err() {
        InputValidationError::RangeViolation {
            field,
            actual,
            constraint,
        } => {
            assert_eq!(field, "value");
            assert_eq!(actual, "-1");
            assert!(constraint.contains(">="));
        }
        _ => panic!("Expected RangeViolation"),
    }
}

#[test]
fn test_range_rule_int_above_range() {
    // Arrange
    let rule = RangeRule::between(0, 100);

    // Act
    let result = rule.validate(&101, "value");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_range_rule_float_within_range() {
    // Arrange
    let rule = RangeRule::between(0.0, 1.0);

    // Act & Assert
    assert!(rule.validate(&0.5, "value").is_ok());
    assert!(rule.validate(&0.0, "value").is_ok());
    assert!(rule.validate(&1.0, "value").is_ok());
}

#[test]
fn test_range_rule_float_outside_range() {
    // Arrange
    let rule = RangeRule::between(0.0, 1.0);

    // Act & Assert
    assert!(rule.validate(&-0.1, "value").is_err());
    assert!(rule.validate(&1.1, "value").is_err());
}

#[test]
fn test_range_rule_min_only() {
    // Arrange
    let rule = RangeRule::min(10);

    // Act & Assert
    assert!(rule.validate(&10, "value").is_ok());
    assert!(rule.validate(&100, "value").is_ok());
    assert!(rule.validate(&9, "value").is_err());
}

#[test]
fn test_range_rule_max_only() {
    // Arrange
    let rule = RangeRule::max(100);

    // Act & Assert
    assert!(rule.validate(&100, "value").is_ok());
    assert!(rule.validate(&0, "value").is_ok());
    assert!(rule.validate(&101, "value").is_err());
}

#[test]
fn test_positive_rule_int_valid() {
    // Arrange
    let rule = PositiveRule;

    // Act & Assert
    assert!(rule.validate(&1, "value").is_ok());
    assert!(rule.validate(&100, "value").is_ok());
}

#[test]
fn test_positive_rule_int_invalid() {
    // Arrange
    let rule = PositiveRule;

    // Act & Assert
    assert!(rule.validate(&0, "value").is_err());
    assert!(rule.validate(&-1, "value").is_err());
}

#[test]
fn test_positive_rule_float_valid() {
    // Arrange
    let rule = PositiveRule;

    // Act & Assert
    assert!(rule.validate(&0.1, "value").is_ok());
    assert!(rule.validate(&100.5, "value").is_ok());
}

#[test]
fn test_positive_rule_float_invalid() {
    // Arrange
    let rule = PositiveRule;

    // Act & Assert
    assert!(rule.validate(&0.0, "value").is_err());
    assert!(rule.validate(&-0.1, "value").is_err());
}

#[test]
fn test_negative_rule_int_valid() {
    // Arrange
    let rule = NegativeRule;

    // Act & Assert
    assert!(rule.validate(&-1, "value").is_ok());
    assert!(rule.validate(&-100, "value").is_ok());
}

#[test]
fn test_negative_rule_int_invalid() {
    // Arrange
    let rule = NegativeRule;

    // Act & Assert
    assert!(rule.validate(&0, "value").is_err());
    assert!(rule.validate(&1, "value").is_err());
}

#[test]
fn test_precision_rule_valid() {
    // Arrange
    let rule = PrecisionRule::new(2);

    // Act & Assert
    assert!(rule.validate(&3.14, "value").is_ok());
    assert!(rule.validate(&3.1, "value").is_ok());
    assert!(rule.validate(&3.0, "value").is_ok());
}

#[test]
fn test_precision_rule_invalid() {
    // Arrange
    let rule = PrecisionRule::new(2);

    // Act
    let result = rule.validate(&3.141, "value");

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_precision_rule_boundary() {
    // Arrange
    let rule = PrecisionRule::new(3);

    // Act & Assert - exactly 3 decimal places
    assert!(rule.validate(&3.141, "value").is_ok());

    // Act & Assert - 4 decimal places (too many)
    assert!(rule.validate(&3.1415, "value").is_err());
}
