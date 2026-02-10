//! Determinism tests for canonical system
//!
//! These tests verify that:
//! 1. Same input always produces same output
//! 2. Same input always produces same hash
//! 3. Canonical forms are stable across runs

use ggen_canonical::{hash, json, rust, ttl, Canonicalizer};
use serde_json::json;

#[test]
fn test_json_determinism_multiple_runs() {
    let input = json!({
        "field_z": "value_z",
        "field_a": "value_a",
        "field_m": {
            "nested_z": 1,
            "nested_a": 2
        }
    });

    let canonicalizer = json::JsonCanonicalizer::new();

    // Run 100 times to ensure consistency
    let first = canonicalizer.canonicalize(input.clone()).unwrap();
    for _ in 0..100 {
        let result = canonicalizer.canonicalize(input.clone()).unwrap();
        assert_eq!(first, result, "JSON canonicalization not deterministic");
    }
}

#[test]
fn test_json_hash_determinism() {
    let input = json!({
        "field1": "value1",
        "field2": "value2"
    });

    let canonicalizer = json::JsonCanonicalizer::new();
    let hash1 = canonicalizer.hash(input.clone()).unwrap();
    let hash2 = canonicalizer.hash(input).unwrap();

    assert_eq!(hash1, hash2, "JSON hashes not deterministic");
}

#[test]
fn test_rust_determinism() {
    let code = r#"
fn main() {
    let x = 1;
    println!("Hello");
}
"#;

    let canonicalizer = rust::RustCanonicalizer::new();
    let result1 = canonicalizer.canonicalize(code.to_string()).unwrap();
    let result2 = canonicalizer.canonicalize(code.to_string()).unwrap();

    assert_eq!(result1, result2, "Rust canonicalization not deterministic");
}

#[test]
fn test_rust_hash_determinism() {
    let code = "fn test() { let x = 42; }";

    let canonicalizer = rust::RustCanonicalizer::new();
    let hash1 = canonicalizer.hash(code.to_string()).unwrap();
    let hash2 = canonicalizer.hash(code.to_string()).unwrap();

    assert_eq!(hash1, hash2, "Rust hashes not deterministic");
}

#[test]
fn test_ttl_determinism() {
    let ttl = r#"
        @prefix ex: <http://example.org/> .
        ex:subject1 ex:predicate1 ex:object1 .
        ex:subject2 ex:predicate2 ex:object2 .
    "#;

    let canonicalizer = ttl::TtlCanonicalizer::new();
    let result1 = canonicalizer.canonicalize(ttl.to_string()).unwrap();
    let result2 = canonicalizer.canonicalize(ttl.to_string()).unwrap();

    assert_eq!(result1, result2, "TTL canonicalization not deterministic");
}

#[test]
fn test_ttl_hash_determinism() {
    let ttl = r#"
        @prefix ex: <http://example.org/> .
        ex:s ex:p ex:o .
    "#;

    let canonicalizer = ttl::TtlCanonicalizer::new();
    let hash1 = canonicalizer.hash(ttl.to_string()).unwrap();
    let hash2 = canonicalizer.hash(ttl.to_string()).unwrap();

    assert_eq!(hash1, hash2, "TTL hashes not deterministic");
}

#[test]
fn test_hash_function_determinism() {
    let data = b"test data for hashing";

    let hash1 = hash::compute_hash(&data).unwrap();
    let hash2 = hash::compute_hash(&data).unwrap();

    assert_eq!(hash1, hash2, "Hash computation not deterministic");
}

#[test]
fn test_different_inputs_different_hashes() {
    let data1 = b"input1";
    let data2 = b"input2";

    let hash1 = hash::compute_hash(&data1).unwrap();
    let hash2 = hash::compute_hash(&data2).unwrap();

    assert_ne!(hash1, hash2, "Different inputs should produce different hashes");
}

#[test]
fn test_json_order_independence() {
    let json1 = json!({"a": 1, "b": 2, "c": 3});
    let json2 = json!({"c": 3, "a": 1, "b": 2});

    let canonicalizer = json::JsonCanonicalizer::new();
    let result1 = canonicalizer.canonicalize(json1).unwrap();
    let result2 = canonicalizer.canonicalize(json2).unwrap();

    assert_eq!(result1, result2, "JSON key order should not affect canonical form");
}

#[test]
fn test_multi_hash_determinism() {
    let parts = vec![b"part1".as_slice(), b"part2".as_slice(), b"part3".as_slice()];

    let hash1 = hash::compute_hash_multi(&parts).unwrap();
    let hash2 = hash::compute_hash_multi(&parts).unwrap();

    assert_eq!(hash1, hash2, "Multi-part hash not deterministic");
}

#[test]
fn test_hash_verifier() {
    let data = b"verification test data";
    let hash = hash::compute_hash(&data).unwrap();

    let verifier = hash::HashVerifier::new(hash);
    assert!(verifier.verify(&data).is_ok(), "Hash verification should succeed");

    let wrong_data = b"wrong data";
    assert!(
        verifier.verify(&wrong_data).is_err(),
        "Hash verification should fail for wrong data"
    );
}

#[test]
fn test_ttl_order_independence() {
    let ttl1 = r#"
        @prefix ex: <http://example.org/> .
        ex:s1 ex:p1 ex:o1 .
        ex:s2 ex:p2 ex:o2 .
    "#;

    let ttl2 = r#"
        @prefix ex: <http://example.org/> .
        ex:s2 ex:p2 ex:o2 .
        ex:s1 ex:p1 ex:o1 .
    "#;

    let canonicalizer = ttl::TtlCanonicalizer::new();
    let result1 = canonicalizer.canonicalize(ttl1.to_string()).unwrap();
    let result2 = canonicalizer.canonicalize(ttl2.to_string()).unwrap();

    assert_eq!(result1, result2, "TTL triple order should not affect canonical form");
}

#[test]
fn test_comprehensive_workflow() {
    // Test a complete workflow: canonicalize → hash → verify
    let json_input = json!({
        "project": "ggen",
        "version": "6.0.0",
        "features": ["canonical", "deterministic"]
    });

    let canonicalizer = json::JsonCanonicalizer::new();

    // Canonicalize
    let canonical = canonicalizer.canonicalize(json_input.clone()).unwrap();

    // Hash
    let hash1 = hash::compute_hash(&canonical).unwrap();

    // Verify same input produces same hash
    let canonical2 = canonicalizer.canonicalize(json_input).unwrap();
    let hash2 = hash::compute_hash(&canonical2).unwrap();

    assert_eq!(hash1, hash2, "Complete workflow should be deterministic");

    // Verify hash matches
    let verifier = hash::HashVerifier::new(hash1);
    assert!(verifier.verify(&canonical).is_ok(), "Hash verification should succeed");
}

// Property-based tests for determinism
#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_json_hash_deterministic(s in "[a-z]{1,10}") {
            let json_val = json!({"key": s});
            let canonicalizer = json::JsonCanonicalizer::new();
            let hash1 = canonicalizer.hash(json_val.clone()).unwrap();
            let hash2 = canonicalizer.hash(json_val).unwrap();
            assert_eq!(hash1, hash2);
        }

        #[test]
        fn prop_hash_deterministic(bytes in prop::collection::vec(any::<u8>(), 0..1000)) {
            let hash1 = hash::compute_hash(&bytes).unwrap();
            let hash2 = hash::compute_hash(&bytes).unwrap();
            assert_eq!(hash1, hash2);
        }
    }
}
