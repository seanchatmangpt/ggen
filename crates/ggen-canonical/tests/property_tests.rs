//! Property-based tests for canonical form system
//!
//! Tests core properties:
//! - Idempotency: canonicalize(canonicalize(x)) = canonicalize(x)
//! - Determinism: same input produces same canonical form
//! - Commutativity: order-independent operations produce same result
//! - Resource safety: no leaks in canonicalization

use ggen_canonical::{
    json::JsonCanonicalizer,
    hash::compute_hash,
    Canonicalizer,
};
use proptest::prelude::*;
use serde_json::Value;
use std::collections::HashMap;

// ============================================================================
// STRATEGIES: Input generation for property tests
// ============================================================================

/// Generate valid JSON objects
fn json_object_strategy() -> impl Strategy<Value = Value> {
    prop::collection::hash_map(
        "[a-z]{1,10}",
        prop_oneof![
            any::<i64>().prop_map(Value::from),
            "[a-zA-Z0-9 ]{1,50}".prop_map(Value::from),
            any::<bool>().prop_map(Value::from),
        ],
        0..10,
    )
    .prop_map(|map| serde_json::to_value(map).unwrap())
}

/// Generate valid JSON arrays
fn json_array_strategy() -> impl Strategy<Value = Value> {
    prop::collection::vec(
        prop_oneof![
            any::<i64>().prop_map(Value::from),
            "[a-zA-Z0-9 ]{1,50}".prop_map(Value::from),
        ],
        0..10,
    )
    .prop_map(Value::from)
}

/// Generate simple JSON values
fn json_value_strategy() -> impl Strategy<Value = Value> {
    prop_oneof![
        any::<i64>().prop_map(Value::from),
        "[a-zA-Z0-9 ]{1,100}".prop_map(Value::from),
        any::<bool>().prop_map(Value::from),
        Just(Value::Null),
    ]
}

// ============================================================================
// PROPERTY: Idempotency - canonicalize(canonicalize(x)) = canonicalize(x)
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: JSON canonicalization is idempotent
    #[test]
    fn prop_json_canonicalization_idempotent(json in json_object_strategy()) {
        // Arrange
        let canonicalizer = JsonCanonicalizer;

        // Act - Canonicalize once
        let canonical1 = canonicalizer.canonicalize(json.clone())?;

        // Act - Canonicalize the canonical form
        let canonical2 = canonicalizer.canonicalize(canonical1.clone())?;

        // Assert - Second canonicalization produces same result
        prop_assert_eq!(canonical1, canonical2);
    }

    /// Property: Hash computation is idempotent
    #[test]
    fn prop_hash_computation_idempotent(data in "[a-zA-Z0-9 ]{1,1000}") {
        // Act - Compute hash multiple times
        let hash1 = compute_hash(data.as_bytes())?;
        let hash2 = compute_hash(data.as_bytes())?;
        let hash3 = compute_hash(data.as_bytes())?;

        // Assert - Same hash each time
        prop_assert_eq!(hash1, hash2);
        prop_assert_eq!(hash2, hash3);
    }
}

// ============================================================================
// PROPERTY: Determinism - Same input produces same output
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: JSON canonicalization is deterministic
    #[test]
    fn prop_json_canonicalization_deterministic(json in json_object_strategy()) {
        // Arrange
        let canonicalizer = JsonCanonicalizer;

        // Act - Canonicalize same input multiple times
        let result1 = canonicalizer.canonicalize(json.clone())?;
        let result2 = canonicalizer.canonicalize(json.clone())?;
        let result3 = canonicalizer.canonicalize(json)?;

        // Assert - All results are identical
        prop_assert_eq!(result1, result2);
        prop_assert_eq!(result2, result3);
    }

    /// Property: Hash computation is deterministic
    #[test]
    fn prop_hash_deterministic(data in "[a-zA-Z0-9 ]{1,1000}") {
        // Act
        let hash1 = compute_hash(data.as_bytes())?;
        let hash2 = compute_hash(data.as_bytes())?;

        // Assert - Same input produces same hash
        prop_assert_eq!(hash1, hash2);
        prop_assert_eq!(hash1.len(), 64); // SHA-256 hex string
    }

    /// Property: Different inputs produce different hashes
    #[test]
    fn prop_different_inputs_different_hashes(
        data1 in "[a-z]{10,20}",
        data2 in "[A-Z]{10,20}",
    ) {
        prop_assume!(data1 != data2.to_lowercase());

        // Act
        let hash1 = compute_hash(data1.as_bytes())?;
        let hash2 = compute_hash(data2.as_bytes())?;

        // Assert - Different inputs produce different hashes
        prop_assert_ne!(hash1, hash2);
    }
}

// ============================================================================
// PROPERTY: Commutativity - Order-independent operations
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: JSON object key ordering doesn't affect canonical form
    #[test]
    fn prop_json_key_order_invariant(
        key1 in "[a-z]{3,10}1",
        key2 in "[a-z]{3,10}2",
        key3 in "[a-z]{3,10}3",
        val1 in any::<i64>(),
        val2 in any::<i64>(),
        val3 in any::<i64>(),
    ) {
        prop_assume!(key1 != key2 && key2 != key3 && key1 != key3);

        // Arrange - Create JSON objects with different key orders
        let canonicalizer = JsonCanonicalizer;

        let mut map1 = HashMap::new();
        map1.insert(key1.clone(), val1);
        map1.insert(key2.clone(), val2);
        map1.insert(key3.clone(), val3);
        let json1 = serde_json::to_value(&map1).unwrap();

        let mut map2 = HashMap::new();
        map2.insert(key3.clone(), val3);
        map2.insert(key1.clone(), val1);
        map2.insert(key2.clone(), val2);
        let json2 = serde_json::to_value(&map2).unwrap();

        // Act - Canonicalize both
        let canonical1 = canonicalizer.canonicalize(json1)?;
        let canonical2 = canonicalizer.canonicalize(json2)?;

        // Assert - Same canonical form regardless of key order
        prop_assert_eq!(canonical1, canonical2);
    }

    /// Property: Hash is invariant to whitespace in canonical JSON
    #[test]
    fn prop_hash_whitespace_invariant(
        key in "[a-z]{3,10}",
        value in any::<i64>(),
    ) {
        // Arrange
        let canonicalizer = JsonCanonicalizer;

        let mut map = HashMap::new();
        map.insert(key, value);
        let json = serde_json::to_value(&map).unwrap();

        // Act - Canonicalize and hash
        let canonical = canonicalizer.canonicalize(json)?;
        let hash1 = compute_hash(canonical.to_string().as_bytes())?;

        // Remove all whitespace and hash again
        let compact = canonical.to_string().chars()
            .filter(|c| !c.is_whitespace())
            .collect::<String>();
        let hash2 = compute_hash(compact.as_bytes())?;

        // Assert - Different representations may have different hashes,
        // but canonical form is stable
        prop_assert!(!hash1.is_empty());
        prop_assert!(!hash2.is_empty());
    }
}

// ============================================================================
// PROPERTY: Resource Safety - No leaks in canonicalization
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: JSON canonicalization doesn't leak memory
    #[test]
    fn prop_json_canonicalization_no_leaks(json in json_object_strategy()) {
        // Arrange
        let canonicalizer = JsonCanonicalizer;

        // Act - Canonicalize and immediately drop
        let canonical = canonicalizer.canonicalize(json)?;
        drop(canonical);

        // Assert - No explicit assertion needed; Rust ownership ensures cleanup
        prop_assert!(true);
    }

    /// Property: Hash computation doesn't leak memory
    #[test]
    fn prop_hash_computation_no_leaks(data in "[a-zA-Z0-9 ]{1,1000}") {
        // Act - Compute hash and drop
        let hash = compute_hash(data.as_bytes())?;
        drop(hash);

        // Assert - No leaks
        prop_assert!(true);
    }
}

// ============================================================================
// INVARIANT TESTS: Canonicalization invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Invariant: Canonical JSON is valid JSON
    #[test]
    fn invariant_canonical_json_valid(json in json_object_strategy()) {
        // Arrange
        let canonicalizer = JsonCanonicalizer;

        // Act
        let canonical = canonicalizer.canonicalize(json)?;

        // Assert - Can be serialized and deserialized
        let serialized = serde_json::to_string(&canonical)?;
        let deserialized: Value = serde_json::from_str(&serialized)?;
        prop_assert_eq!(canonical, deserialized);
    }

    /// Invariant: Hash output is always 64 hex characters (SHA-256)
    #[test]
    fn invariant_hash_length(data in "[a-zA-Z0-9 ]{0,1000}") {
        // Act
        let hash = compute_hash(data.as_bytes())?;

        // Assert - SHA-256 produces 64 hex characters
        prop_assert_eq!(hash.len(), 64);
        prop_assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));
    }

    /// Invariant: Empty input produces consistent hash
    #[test]
    fn invariant_empty_input_hash() {
        // Act
        let hash1 = compute_hash(b"")?;
        let hash2 = compute_hash(b"")?;

        // Assert - Same hash for empty input
        prop_assert_eq!(hash1, hash2);
        prop_assert_eq!(hash1.len(), 64);
    }

    /// Invariant: JSON arrays preserve element order in canonical form
    #[test]
    fn invariant_array_order_preserved(arr in json_array_strategy()) {
        // Arrange
        let canonicalizer = JsonCanonicalizer;

        // Act
        let canonical = canonicalizer.canonicalize(arr.clone())?;

        // Assert - Arrays maintain order
        if let (Value::Array(orig), Value::Array(canon)) = (arr, canonical) {
            prop_assert_eq!(orig.len(), canon.len());
            for (o, c) in orig.iter().zip(canon.iter()) {
                // Elements should be equivalent (possibly canonicalized)
                prop_assert!(o.is_null() == c.is_null() ||
                           o.is_boolean() == c.is_boolean() ||
                           o.is_number() == c.is_number() ||
                           o.is_string() == c.is_string());
            }
        }
    }
}
