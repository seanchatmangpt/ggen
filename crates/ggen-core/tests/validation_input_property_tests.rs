//! Property-based tests for input validation (Week 4)
//!
//! These tests use proptest to fuzz validators with random inputs and verify invariants.
//!
//! Test coverage: 10+ property tests

use ggen_core::validation::input::*;
use proptest::prelude::*;

// =============================================================================
// Length Rule Properties
// =============================================================================

proptest! {
    #[test]
    fn test_length_rule_min_property(s in "\\PC*", min in 1usize..100) {
        let rule = LengthRule::min(min);
        let result = rule.validate(&s, "test");

        if s.len() >= min {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }

    #[test]
    fn test_length_rule_max_property(s in "\\PC*", max in 1usize..100) {
        let rule = LengthRule::max(max);
        let result = rule.validate(&s, "test");

        if s.len() <= max {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }

    #[test]
    fn test_length_rule_range_property(
        s in "\\PC*",
        min in 1usize..50,
        max in 51usize..100
    ) {
        let rule = LengthRule::range(min, max);
        let result = rule.validate(&s, "test");

        if s.len() >= min && s.len() <= max {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }
}

// =============================================================================
// Charset Rule Properties
// =============================================================================

proptest! {
    #[test]
    fn test_charset_alphanumeric_property(s in "[a-zA-Z0-9]*") {
        let rule = CharsetRule::alphanumeric();
        let result = rule.validate(&s, "test");
        prop_assert!(result.is_ok());
    }

    #[test]
    fn test_charset_alphanumeric_rejects_special_chars(
        s in "[a-zA-Z0-9]*[^a-zA-Z0-9]+[a-zA-Z0-9]*"
    ) {
        let rule = CharsetRule::alphanumeric();
        let result = rule.validate(&s, "test");
        prop_assert!(result.is_err());
    }

    #[test]
    fn test_charset_identifier_accepts_valid(s in "[a-zA-Z0-9_-]+") {
        let rule = CharsetRule::identifier();
        let result = rule.validate(&s, "test");
        prop_assert!(result.is_ok());
    }
}

// =============================================================================
// Whitelist/Blacklist Properties
// =============================================================================

proptest! {
    #[test]
    fn test_whitelist_accepts_listed_values(
        values in prop::collection::vec("[a-z]+", 1..10),
        idx in 0usize..10
    ) {
        prop_assume!(!values.is_empty());
        let idx = idx % values.len();
        let rule = WhitelistRule::new(values.clone());
        let result = rule.validate(&values[idx], "test");
        prop_assert!(result.is_ok());
    }

    #[test]
    fn test_blacklist_rejects_listed_values(
        forbidden in prop::collection::vec("[a-z]+", 1..10),
        idx in 0usize..10
    ) {
        prop_assume!(!forbidden.is_empty());
        let idx = idx % forbidden.len();
        let rule = BlacklistRule::new(forbidden.clone());
        let result = rule.validate(&forbidden[idx], "test");
        prop_assert!(result.is_err());
    }
}

// =============================================================================
// Numeric Range Properties
// =============================================================================

proptest! {
    #[test]
    fn test_range_rule_int_property(
        value in -1000i64..1000,
        min in -100i64..0,
        max in 1i64..100
    ) {
        let rule = RangeRule::between(min, max);
        let result = rule.validate(&value, "test");

        if value >= min && value <= max {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }

    #[test]
    fn test_positive_rule_int_property(value in -1000i64..1000) {
        let rule = PositiveRule;
        let result = rule.validate(&value, "test");

        if value > 0 {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }

    #[test]
    fn test_negative_rule_int_property(value in -1000i64..1000) {
        let rule = NegativeRule;
        let result = rule.validate(&value, "test");

        if value < 0 {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }
}

// =============================================================================
// Composite Rule Properties
// =============================================================================

proptest! {
    #[test]
    fn test_and_rule_property(
        s in "\\PC*",
        min in 1usize..50,
        max in 51usize..100
    ) {
        let rule1 = LengthRule::min(min);
        let rule2 = LengthRule::max(max);
        let composite = rule1.and(rule2);

        let result = composite.validate(&s, "test");

        if s.len() >= min && s.len() <= max {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }

    #[test]
    fn test_or_rule_property(s in "\\PC*", min1 in 1usize..30, min2 in 31usize..50) {
        let rule1 = LengthRule::min(min1);
        let rule2 = LengthRule::min(min2);
        let composite = rule1.or(rule2);

        let result = composite.validate(&s, "test");

        if s.len() >= min1 {
            prop_assert!(result.is_ok());
        }
    }
}

// =============================================================================
// Path Validator Properties
// =============================================================================

proptest! {
    #[test]
    fn test_path_validator_rejects_traversal(
        segments in prop::collection::vec("[a-z]+", 1..5)
    ) {
        let validator = PathValidatorRule::new();

        // Create path with ..
        let path_str = format!("../{}", segments.join("/"));
        let path = std::path::Path::new(&path_str);

        let result = validator.validate(path);
        prop_assert!(result.is_err());
    }

    #[test]
    fn test_path_validator_accepts_safe_paths(
        segments in prop::collection::vec("[a-z]+", 1..5),
        ext in "[a-z]{2,4}"
    ) {
        let validator = PathValidatorRule::new();

        let path_str = format!("{}.{}", segments.join("/"), ext);
        let path = std::path::Path::new(&path_str);

        let result = validator.validate(path);
        prop_assert!(result.is_ok());
    }

    #[test]
    fn test_path_validator_extension_whitelist_property(
        filename in "[a-z]+",
        ext in "[a-z]{2,4}"
    ) {
        let validator = PathValidatorRule::new()
            .with_extensions(vec![ext.clone()]);

        let path_str = format!("{}.{}", filename, ext);
        let path = std::path::Path::new(&path_str);

        let result = validator.validate(path);
        prop_assert!(result.is_ok());
    }
}

// =============================================================================
// URL Validator Properties
// =============================================================================

proptest! {
    #[test]
    fn test_url_validator_https_requirement_property(
        domain in "[a-z]{3,10}\\.[a-z]{2,3}",
        path in "[a-z/]*"
    ) {
        let validator = UrlValidator::new().require_https();

        let https_url = format!("https://{}/{}", domain, path);
        let http_url = format!("http://{}/{}", domain, path);

        prop_assert!(validator.validate(&https_url).is_ok());
        prop_assert!(validator.validate(&http_url).is_err());
    }

    #[test]
    fn test_url_validator_domain_whitelist_property(
        domain in "[a-z]{3,10}\\.[a-z]{2,3}",
        path in "[a-z/]*"
    ) {
        let validator = UrlValidator::new()
            .with_domains(vec![domain.clone()]);

        let url = format!("https://{}/{}", domain, path);
        let result = validator.validate(&url);

        prop_assert!(result.is_ok());
    }
}

// =============================================================================
// String Validator Builder Properties
// =============================================================================

proptest! {
    #[test]
    fn test_string_validator_builder_preserves_constraints(
        s in "[a-zA-Z0-9_-]{5,20}"
    ) {
        let validator = StringValidator::new()
            .with_length(5, 20)
            .with_charset(CharsetRule::identifier());

        let result = validator.validate(&s);
        prop_assert!(result.is_ok());
    }

    #[test]
    fn test_string_validator_rejects_violations(
        s in "[a-zA-Z0-9_-]{1,4}"
    ) {
        let validator = StringValidator::new()
            .with_length(5, 20)
            .with_charset(CharsetRule::identifier());

        let result = validator.validate(&s);
        prop_assert!(result.is_err());
    }
}
