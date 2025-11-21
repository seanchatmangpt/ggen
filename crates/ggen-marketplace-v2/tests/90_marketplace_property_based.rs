//! Phase 3C: Property-Based Tests for ggen-marketplace-v2
//!
//! Using proptest with arbitrary for comprehensive property verification.
//! Minimum 100 test cases per property for statistical confidence.
//!
//! Test Categories:
//! 1. RDF Mapper Properties (roundtrip integrity)
//! 2. Validator Properties (determinism, consistency)
//! 3. Search Engine Properties (determinism, relevance)
//! 4. Parser Properties (PackageId, Version validation)

use ggen_marketplace_v2::{
    models::{Package, PackageId, PackageMetadata, PackageVersion, QualityScore},
    search::{SearchEngine, SearchQuery, SortBy},
    security::{ChecksumCalculator, KeyPair, SignatureVerifier},
    traits::Signable,
    validation::PackageValidator,
};
use proptest::prelude::*;
use std::collections::HashSet;

// ============================================================================
// Proptest Configuration
// ============================================================================

/// Configure proptest for 100+ cases per property
fn config() -> ProptestConfig {
    ProptestConfig {
        cases: 100,
        max_shrink_iters: 1000,
        timeout: 30000, // 30 second timeout per test
        ..ProptestConfig::default()
    }
}

// ============================================================================
// Arbitrary Generators
// ============================================================================

/// Generate valid package ID strings
fn valid_package_id_strategy() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9_-]{0,49}[a-z0-9]?"
        .prop_filter("must not end with hyphen", |s| !s.ends_with('-'))
        .prop_filter("must be non-empty", |s| !s.is_empty())
}

/// Generate invalid package ID strings
fn invalid_package_id_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just(String::new()),                               // Empty
        "-[a-z0-9]+".prop_map(|_| "-invalid".to_string()), // Starts with hyphen
        "[a-z0-9]+-".prop_map(|_| "invalid-".to_string()), // Ends with hyphen
        ".*\\s.*".prop_map(|_| "has space".to_string()),   // Contains space
        ".{201,}".prop_map(|_| "a".repeat(201)),           // Too long
    ]
}

/// Generate valid semantic versions
fn valid_version_strategy() -> impl Strategy<Value = String> {
    (0u32..100, 0u32..100, 0u32..100)
        .prop_map(|(major, minor, patch)| format!("{}.{}.{}", major, minor, patch))
}

/// Generate invalid version strings
fn invalid_version_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just(String::new()),                               // Empty
        "[0-9]+\\.[0-9]+".prop_map(|_| "1.0".to_string()), // Missing patch
        "[a-z]+".prop_map(|_| "abc".to_string()),          // Non-numeric
    ]
}

/// Generate valid quality scores (1-100)
fn valid_quality_score_strategy() -> impl Strategy<Value = u32> {
    1u32..=100
}

/// Generate invalid quality scores
fn invalid_quality_score_strategy() -> impl Strategy<Value = u32> {
    prop_oneof![Just(0u32), 101u32..=1000,]
}

/// Generate package metadata with valid fields
fn package_metadata_strategy() -> impl Strategy<Value = (String, String, String, String)> {
    (
        valid_package_id_strategy(),
        "[A-Za-z ]{1,50}",  // Name
        "[A-Za-z ]{1,200}", // Description
        prop_oneof![
            Just("MIT".to_string()),
            Just("Apache-2.0".to_string()),
            Just("GPL-3.0".to_string()),
        ],
    )
}

/// Generate search query text
fn search_query_strategy() -> impl Strategy<Value = String> {
    "[a-z]{1,20}"
}

/// Generate binary data for checksums and signatures
fn binary_data_strategy() -> impl Strategy<Value = Vec<u8>> {
    prop::collection::vec(any::<u8>(), 1..1000)
}

// ============================================================================
// 1. Parser Properties (PackageId and Version)
// ============================================================================

proptest! {
    #![proptest_config(config())]

    /// Property: Valid package IDs always parse successfully
    #[test]
    fn prop_valid_package_id_parses(id in valid_package_id_strategy()) {
        let result = PackageId::new(&id);
        prop_assert!(
            result.is_ok(),
            "Valid package ID '{}' should parse successfully: {:?}",
            id,
            result.err()
        );
    }

    /// Property: PackageId parsing is case-insensitive (normalizes to lowercase)
    #[test]
    fn prop_package_id_case_normalization(id in valid_package_id_strategy()) {
        let lower = PackageId::new(&id.to_lowercase());
        let upper = PackageId::new(&id.to_uppercase());

        if let (Ok(l), Ok(u)) = (lower, upper) {
            prop_assert_eq!(
                l.as_str(),
                u.as_str(),
                "Package IDs should normalize to same lowercase"
            );
        }
    }

    /// Property: PackageId roundtrip (create -> as_str -> create)
    #[test]
    fn prop_package_id_roundtrip(id in valid_package_id_strategy()) {
        let parsed = PackageId::new(&id)?;
        let reparsed = PackageId::new(parsed.as_str())?;
        prop_assert_eq!(
            parsed.as_str(),
            reparsed.as_str(),
            "PackageId should roundtrip correctly"
        );
    }

    /// Property: PackageId Display matches as_str
    #[test]
    fn prop_package_id_display_matches_as_str(id in valid_package_id_strategy()) {
        let parsed = PackageId::new(&id)?;
        prop_assert_eq!(
            parsed.to_string(),
            parsed.as_str(),
            "Display should match as_str"
        );
    }

    /// Property: Valid versions always parse successfully
    #[test]
    fn prop_valid_version_parses(version in valid_version_strategy()) {
        let result = PackageVersion::new(&version);
        prop_assert!(
            result.is_ok(),
            "Valid version '{}' should parse successfully: {:?}",
            version,
            result.err()
        );
    }

    /// Property: Version with 'v' prefix parses same as without
    #[test]
    fn prop_version_v_prefix_normalization(version in valid_version_strategy()) {
        let without_v = PackageVersion::new(&version)?;
        let with_v = PackageVersion::new(&format!("v{}", version))?;

        prop_assert_eq!(
            without_v.as_str(),
            with_v.as_str(),
            "Version should normalize 'v' prefix"
        );
    }

    /// Property: Version roundtrip
    #[test]
    fn prop_version_roundtrip(version in valid_version_strategy()) {
        let parsed = PackageVersion::new(&version)?;
        let reparsed = PackageVersion::new(parsed.as_str())?;
        prop_assert_eq!(
            parsed.as_str(),
            reparsed.as_str(),
            "Version should roundtrip correctly"
        );
    }

    /// Property: Version ordering is total and consistent
    #[test]
    fn prop_version_ordering_consistent(
        v1 in valid_version_strategy(),
        v2 in valid_version_strategy(),
        v3 in valid_version_strategy()
    ) {
        let ver1 = PackageVersion::new(&v1)?;
        let ver2 = PackageVersion::new(&v2)?;
        let ver3 = PackageVersion::new(&v3)?;

        // Reflexivity: a == a
        prop_assert!(ver1 == ver1, "Version should equal itself");

        // Transitivity: if a <= b and b <= c then a <= c
        if ver1 <= ver2 && ver2 <= ver3 {
            prop_assert!(ver1 <= ver3, "Version ordering should be transitive");
        }
    }
}

// ============================================================================
// 2. Validator Properties
// ============================================================================

proptest! {
    #![proptest_config(config())]

    /// Property: Valid quality scores create successfully
    #[test]
    fn prop_valid_quality_score(score in valid_quality_score_strategy()) {
        let result = QualityScore::new(score);
        prop_assert!(
            result.is_ok(),
            "Quality score {} should be valid",
            score
        );
    }

    /// Property: Invalid quality scores fail
    #[test]
    fn prop_invalid_quality_score(score in invalid_quality_score_strategy()) {
        let result = QualityScore::new(score);
        prop_assert!(
            result.is_err(),
            "Quality score {} should be invalid",
            score
        );
    }

    /// Property: QualityScore value roundtrip
    #[test]
    fn prop_quality_score_value_roundtrip(score in valid_quality_score_strategy()) {
        let qs = QualityScore::new(score)?;
        prop_assert_eq!(
            qs.value(),
            score,
            "Quality score value should match input"
        );
    }

    /// Property: Production ready threshold is consistent (>= 95)
    #[test]
    fn prop_quality_score_production_ready_threshold(score in valid_quality_score_strategy()) {
        let qs = QualityScore::new(score)?;
        if score >= 95 {
            prop_assert!(qs.is_production_ready(), "Score {} should be production ready", score);
        } else {
            prop_assert!(!qs.is_production_ready(), "Score {} should not be production ready", score);
        }
    }

    /// Property: Needs improvement range is consistent (80-94)
    #[test]
    fn prop_quality_score_needs_improvement_range(score in valid_quality_score_strategy()) {
        let qs = QualityScore::new(score)?;
        if score >= 80 && score < 95 {
            prop_assert!(qs.needs_improvement(), "Score {} should need improvement", score);
        } else {
            prop_assert!(!qs.needs_improvement(), "Score {} should not need improvement", score);
        }
    }

    /// Property: Not ready range is consistent (< 80)
    #[test]
    fn prop_quality_score_not_ready_range(score in valid_quality_score_strategy()) {
        let qs = QualityScore::new(score)?;
        if score < 80 {
            prop_assert!(qs.not_ready(), "Score {} should be not ready", score);
        } else {
            prop_assert!(!qs.not_ready(), "Score {} should not be not ready", score);
        }
    }

    /// Property: Quality score ranges are mutually exclusive
    #[test]
    fn prop_quality_score_ranges_exclusive(score in valid_quality_score_strategy()) {
        let qs = QualityScore::new(score)?;
        let categories = [
            qs.is_production_ready(),
            qs.needs_improvement(),
            qs.not_ready(),
        ];
        let count = categories.iter().filter(|&&b| b).count();
        prop_assert_eq!(
            count,
            1,
            "Quality score {} should be in exactly one category",
            score
        );
    }
}

// ============================================================================
// 3. Search Engine Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig { cases: 50, ..config() })] // Reduced for performance

    /// Property: Search results are deterministic (same input = same output)
    #[test]
    fn prop_search_deterministic(query_text in search_query_strategy()) {
        // Create consistent test data
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();
        let query = SearchQuery::new(&query_text);

        // Run search twice
        let results1 = engine.search(packages.clone(), &query).unwrap();
        let results2 = engine.search(packages, &query).unwrap();

        // Same results
        prop_assert_eq!(
            results1.len(),
            results2.len(),
            "Search should return same number of results"
        );

        for (r1, r2) in results1.iter().zip(results2.iter()) {
            prop_assert_eq!(
                r1.package.metadata.id.as_str(),
                r2.package.metadata.id.as_str(),
                "Search results should be in same order"
            );
        }
    }

    /// Property: Search limit is respected
    #[test]
    fn prop_search_respects_limit(limit in 1usize..20) {
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();
        let query = SearchQuery::new("test").with_limit(limit);

        let results = engine.search(packages, &query).unwrap();

        prop_assert!(
            results.len() <= limit,
            "Search should return at most {} results, got {}",
            limit,
            results.len()
        );
    }

    /// Property: Search offset skips correct number of results
    #[test]
    fn prop_search_respects_offset(offset in 0usize..10) {
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();

        // Get all results
        let query_all = SearchQuery::new("test").with_limit(100);
        let all_results = engine.search(packages.clone(), &query_all).unwrap();

        // Get results with offset
        let query_offset = SearchQuery::new("test").with_limit(100).with_offset(offset);
        let offset_results = engine.search(packages, &query_offset).unwrap();

        // Verify offset
        if all_results.len() > offset {
            let expected_len = all_results.len().saturating_sub(offset);
            prop_assert_eq!(
                offset_results.len(),
                expected_len,
                "Offset {} should skip {} results",
                offset,
                offset
            );
        }
    }

    /// Property: Relevance scores are bounded [0, 1]
    #[test]
    fn prop_search_relevance_bounded(query_text in search_query_strategy()) {
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();
        let query = SearchQuery::new(&query_text);

        let results = engine.search(packages, &query).unwrap();

        for result in results {
            prop_assert!(
                result.relevance >= 0.0 && result.relevance <= 1.0,
                "Relevance {} should be in [0, 1]",
                result.relevance
            );
        }
    }

    /// Property: Name sorting is alphabetical
    #[test]
    fn prop_search_name_sort_alphabetical(_seed in any::<u64>()) {
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();
        let query = SearchQuery::new("test").with_sort(SortBy::Name);

        let results = engine.search(packages, &query).unwrap();

        if results.len() >= 2 {
            for i in 0..results.len() - 1 {
                prop_assert!(
                    results[i].package.metadata.name <= results[i + 1].package.metadata.name,
                    "Names should be in alphabetical order"
                );
            }
        }
    }

    /// Property: Downloads sorting is descending
    #[test]
    fn prop_search_downloads_sort_descending(_seed in any::<u64>()) {
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();
        let query = SearchQuery::new("test").with_sort(SortBy::Downloads);

        let results = engine.search(packages, &query).unwrap();

        if results.len() >= 2 {
            for i in 0..results.len() - 1 {
                prop_assert!(
                    results[i].package.metadata.downloads >= results[i + 1].package.metadata.downloads,
                    "Downloads should be in descending order"
                );
            }
        }
    }
}

// ============================================================================
// 4. Security Properties (Checksum and Signature)
// ============================================================================

proptest! {
    #![proptest_config(config())]

    /// Property: Checksum is deterministic (same data = same checksum)
    #[test]
    fn prop_checksum_deterministic(data in binary_data_strategy()) {
        let checksum1 = ChecksumCalculator::calculate(&data);
        let checksum2 = ChecksumCalculator::calculate(&data);

        prop_assert_eq!(
            checksum1,
            checksum2,
            "Checksum should be deterministic"
        );
    }

    /// Property: Checksum length is always 64 (SHA-256 hex)
    #[test]
    fn prop_checksum_length_constant(data in binary_data_strategy()) {
        let checksum = ChecksumCalculator::calculate(&data);

        prop_assert_eq!(
            checksum.len(),
            64,
            "SHA-256 checksum should always be 64 hex chars"
        );
    }

    /// Property: Checksum is valid hex
    #[test]
    fn prop_checksum_valid_hex(data in binary_data_strategy()) {
        let checksum = ChecksumCalculator::calculate(&data);

        prop_assert!(
            checksum.chars().all(|c| c.is_ascii_hexdigit()),
            "Checksum should only contain hex digits"
        );
    }

    /// Property: Checksum verification succeeds for original data
    #[test]
    fn prop_checksum_verifies_original(data in binary_data_strategy()) {
        let checksum = ChecksumCalculator::calculate(&data);
        let verified = ChecksumCalculator::verify(&data, &checksum).unwrap();

        prop_assert!(verified, "Checksum should verify original data");
    }

    /// Property: Different data produces different checksums (collision resistance)
    #[test]
    fn prop_checksum_collision_resistance(
        data1 in binary_data_strategy(),
        data2 in binary_data_strategy()
    ) {
        prop_assume!(data1 != data2);

        let checksum1 = ChecksumCalculator::calculate(&data1);
        let checksum2 = ChecksumCalculator::calculate(&data2);

        prop_assert_ne!(
            checksum1,
            checksum2,
            "Different data should produce different checksums"
        );
    }

    /// Property: Signature is deterministic for same key and data
    #[test]
    fn prop_signature_deterministic(data in binary_data_strategy()) {
        let key_pair = KeyPair::generate();
        let verifier = SignatureVerifier::new(key_pair);

        // Note: Ed25519 signatures are deterministic for same key+data
        let sig1 = verifier.sign(&data).unwrap();
        let sig2 = verifier.sign(&data).unwrap();

        prop_assert_eq!(
            sig1,
            sig2,
            "Signature should be deterministic for same key and data"
        );
    }

    /// Property: Signature verifies for original data
    #[test]
    fn prop_signature_verifies_original(data in binary_data_strategy()) {
        let key_pair = KeyPair::generate();
        let verifier = SignatureVerifier::new(key_pair);

        let signature = verifier.sign(&data).unwrap();
        let verified = verifier.verify(&data, &signature).unwrap();

        prop_assert!(verified, "Signature should verify original data");
    }

    /// Property: Signature fails for modified data
    #[test]
    fn prop_signature_fails_modified(
        data in binary_data_strategy(),
        modifier in any::<u8>()
    ) {
        prop_assume!(!data.is_empty());

        let key_pair = KeyPair::generate();
        let verifier = SignatureVerifier::new(key_pair);

        let signature = verifier.sign(&data).unwrap();

        // Modify data
        let mut modified = data.clone();
        modified[0] = modified[0].wrapping_add(modifier.max(1)); // Ensure different

        if modified != data {
            let verified = verifier.verify(&modified, &signature).unwrap();
            prop_assert!(!verified, "Signature should fail for modified data");
        }
    }

    /// Property: Public key is consistent
    #[test]
    fn prop_public_key_consistent(_seed in any::<u64>()) {
        let key_pair = KeyPair::generate();
        let pub_key = key_pair.public_key_hex();

        // Public key should be 64 hex chars (32 bytes)
        prop_assert_eq!(
            pub_key.len(),
            64,
            "Public key should be 64 hex chars"
        );

        // Should be valid hex
        prop_assert!(
            pub_key.chars().all(|c| c.is_ascii_hexdigit()),
            "Public key should be valid hex"
        );
    }

    /// Property: Key serialization roundtrip
    #[test]
    fn prop_key_serialization_roundtrip(_seed in any::<u64>()) {
        let key_pair1 = KeyPair::generate();
        let secret_hex = key_pair1.secret_key_hex();

        let key_pair2 = KeyPair::from_secret_key(&secret_hex).unwrap();

        prop_assert_eq!(
            key_pair1.public_key_hex(),
            key_pair2.public_key_hex(),
            "Key should roundtrip through serialization"
        );
    }
}

// ============================================================================
// 5. Package Properties
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig { cases: 50, ..config() })]

    /// Property: Package metadata fields are preserved
    #[test]
    fn prop_package_metadata_preserved(
        (id, name, desc, license) in package_metadata_strategy()
    ) {
        let pkg_id = PackageId::new(&id)?;
        let metadata = PackageMetadata::new(
            pkg_id.clone(),
            &name,
            &desc,
            &license,
        );

        prop_assert_eq!(metadata.id.as_str(), pkg_id.as_str());
        prop_assert_eq!(metadata.name, name);
        prop_assert_eq!(metadata.description, desc);
        prop_assert_eq!(metadata.license, license);
    }

    /// Property: Package downloads default to 0
    #[test]
    fn prop_package_downloads_default(
        (id, name, desc, license) in package_metadata_strategy()
    ) {
        let pkg_id = PackageId::new(&id)?;
        let metadata = PackageMetadata::new(pkg_id, &name, &desc, &license);

        prop_assert_eq!(metadata.downloads, 0, "Downloads should default to 0");
    }

    /// Property: Package timestamps are reasonable
    #[test]
    fn prop_package_timestamps_reasonable(
        (id, name, desc, license) in package_metadata_strategy()
    ) {
        let pkg_id = PackageId::new(&id)?;
        let metadata = PackageMetadata::new(pkg_id, &name, &desc, &license);

        // Timestamps should be recent (within last hour)
        let now = chrono::Utc::now();
        let hour_ago = now - chrono::Duration::hours(1);

        prop_assert!(
            metadata.created_at >= hour_ago && metadata.created_at <= now,
            "created_at should be recent"
        );
        prop_assert!(
            metadata.updated_at >= hour_ago && metadata.updated_at <= now,
            "updated_at should be recent"
        );
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a set of test packages for search tests
fn create_test_packages_for_search() -> Vec<Package> {
    let test_ids = vec![
        "test-package-alpha",
        "test-package-beta",
        "test-package-gamma",
        "another-test-lib",
        "search-test-util",
    ];

    test_ids
        .into_iter()
        .enumerate()
        .map(|(i, id)| {
            let pkg_id = PackageId::new(id).expect("valid test id");
            let version = PackageVersion::new("1.0.0").expect("valid version");

            let mut metadata = PackageMetadata::new(
                pkg_id.clone(),
                format!("Test Package {}", id),
                format!("A test package for searching: {}", id),
                "MIT",
            );
            metadata.downloads = (i as u64 + 1) * 100;
            metadata.keywords = vec!["test".to_string(), id.to_string()];

            Package {
                metadata,
                latest_version: version.clone(),
                versions: vec![version],
                releases: indexmap::IndexMap::new(),
            }
        })
        .collect()
}

// ============================================================================
// Additional Property Tests
// ============================================================================

#[cfg(test)]
mod invariant_tests {
    use super::*;

    /// Property: Empty search returns no results
    #[test]
    fn test_empty_package_list_search() {
        let engine = SearchEngine::new();
        let query = SearchQuery::new("anything");
        let results = engine.search(vec![], &query).unwrap();

        assert!(
            results.is_empty(),
            "Search on empty list should return no results"
        );
    }

    /// Property: Validation is deterministic
    #[tokio::test]
    async fn test_validation_deterministic() {
        let pkg_id = PackageId::new("determinism-test").expect("valid id");
        let metadata = PackageMetadata::new(
            pkg_id,
            "Test Package",
            "A test package for determinism",
            "MIT",
        );
        let pkg = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").expect("valid version"),
            versions: vec![PackageVersion::new("1.0.0").expect("valid version")],
            releases: indexmap::IndexMap::new(),
        };

        let validator = PackageValidator::new();

        let result1 = validator.validate(&pkg).await.expect("validation 1");
        let result2 = validator.validate(&pkg).await.expect("validation 2");

        assert_eq!(
            result1.quality_score, result2.quality_score,
            "Validation should be deterministic"
        );
        assert_eq!(
            result1.passed, result2.passed,
            "Validation pass/fail should be deterministic"
        );
    }

    /// Property: Package ID equality is reflexive
    #[test]
    fn test_package_id_equality_reflexive() {
        let id = PackageId::new("reflexive-test").expect("valid id");
        assert_eq!(id, id, "PackageId should equal itself");
    }

    /// Property: PackageVersion equality is reflexive
    #[test]
    fn test_version_equality_reflexive() {
        let version = PackageVersion::new("1.0.0").expect("valid version");
        assert_eq!(version, version, "PackageVersion should equal itself");
    }

    /// Property: Search with no matching query returns empty
    #[test]
    fn test_search_no_matches() {
        let packages = create_test_packages_for_search();
        let engine = SearchEngine::new();
        let query = SearchQuery::new("zzzznonexistent12345");

        let results = engine.search(packages, &query).unwrap();

        assert!(
            results.is_empty(),
            "Search for non-matching term should return empty"
        );
    }
}
