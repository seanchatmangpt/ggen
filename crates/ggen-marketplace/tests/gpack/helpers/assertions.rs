//! Assertion helpers for gpack tests
//!
//! Provides domain-specific assertion macros and functions
//! for verifying gpack behavior.
//!
//! Feature: 014-marketplace-gpack T003

/// Assert that a manifest is valid
#[macro_export]
macro_rules! assert_manifest_valid {
    ($manifest:expr) => {
        match $manifest.validate() {
            Ok(_) => {}
            Err(e) => panic!("Expected valid manifest, but got error: {:?}", e),
        }
    };
}

/// Assert that a manifest is invalid with a specific error message
#[macro_export]
macro_rules! assert_manifest_invalid {
    ($manifest:expr, $expected_msg:expr) => {
        match $manifest.validate() {
            Ok(_) => panic!("Expected invalid manifest, but validation passed"),
            Err(e) => {
                let err_str = e.to_string();
                assert!(
                    err_str.contains($expected_msg),
                    "Expected error containing '{}', but got: {}",
                    $expected_msg,
                    err_str
                );
            }
        }
    };
}

/// Assert that a version satisfies a constraint
pub fn assert_version_satisfies(version: &str, constraint: &str) {
    let version = semver::Version::parse(version)
        .unwrap_or_else(|e| panic!("Invalid version '{}': {}", version, e));
    let req = semver::VersionReq::parse(constraint)
        .unwrap_or_else(|e| panic!("Invalid constraint '{}': {}", constraint, e));

    assert!(
        req.matches(&version),
        "Version {} should satisfy constraint {}",
        version,
        constraint
    );
}

/// Assert that a version does NOT satisfy a constraint
pub fn assert_version_not_satisfies(version: &str, constraint: &str) {
    let version = semver::Version::parse(version)
        .unwrap_or_else(|e| panic!("Invalid version '{}': {}", version, e));
    let req = semver::VersionReq::parse(constraint)
        .unwrap_or_else(|e| panic!("Invalid constraint '{}': {}", constraint, e));

    assert!(
        !req.matches(&version),
        "Version {} should NOT satisfy constraint {}",
        version,
        constraint
    );
}

/// Assert checksum matches
pub fn assert_checksum_matches(data: &[u8], expected: &str) {
    use sha2::{Sha256, Digest};
    let actual = hex::encode(Sha256::digest(data));
    assert_eq!(
        actual, expected,
        "Checksum mismatch: expected {}, got {}",
        expected, actual
    );
}

/// Assert FMEA RPN is within acceptable threshold
pub fn assert_rpn_acceptable(rpn: u32, threshold: u32) {
    assert!(
        rpn <= threshold,
        "RPN {} exceeds acceptable threshold of {}",
        rpn,
        threshold
    );
}

/// Assert FMEA RPN is critical (above threshold)
pub fn assert_rpn_critical(rpn: u32, threshold: u32) {
    assert!(
        rpn > threshold,
        "RPN {} should be above critical threshold of {}",
        rpn,
        threshold
    );
}

/// Assert quality score meets minimum
pub fn assert_quality_score_meets(score: f64, minimum: f64) {
    assert!(
        score >= minimum,
        "Quality score {} does not meet minimum {}",
        score,
        minimum
    );
}

/// Assert cache entry exists and is valid
pub fn assert_cache_hit<T>(result: Option<&T>, msg: &str) {
    assert!(result.is_some(), "Cache miss: {}", msg);
}

/// Assert cache entry does not exist
pub fn assert_cache_miss<T>(result: Option<&T>, msg: &str) {
    assert!(result.is_none(), "Unexpected cache hit: {}", msg);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_satisfies() {
        assert_version_satisfies("1.2.3", "^1.0");
        assert_version_satisfies("1.2.3", ">=1.0,<2.0");
    }

    #[test]
    fn test_version_not_satisfies() {
        assert_version_not_satisfies("2.0.0", "^1.0");
        assert_version_not_satisfies("0.5.0", ">=1.0");
    }

    #[test]
    fn test_rpn_acceptable() {
        assert_rpn_acceptable(50, 100);
        assert_rpn_acceptable(100, 100);
    }

    #[test]
    #[should_panic(expected = "exceeds acceptable threshold")]
    fn test_rpn_unacceptable() {
        assert_rpn_acceptable(150, 100);
    }

    #[test]
    fn test_quality_score_meets() {
        assert_quality_score_meets(0.85, 0.8);
        assert_quality_score_meets(0.8, 0.8);
    }
}
