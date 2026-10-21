//! CanonicalIdentityLattice courts (wave-1 lane L06 / lane A10).
//!
//! Subject under test: `ggen_config::canonical` (`JsonCanonicalizer`,
//! `canonicalize_json`/`canonicalize_json_str`, `CanonicalError`, `Canonicalizer::hash`
//! delegating to `crate::receipt::hash_data`).
//!
//! Chicago style: real JSON fixtures, real SHA-256 digests, state-based assertions.
//! No mocks — every court exercises the actual canonicalization pipeline.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use ggen_config::canonical::json::{canonicalize_json_str, JsonCanonicalizer};
use ggen_config::canonical::{CanonicalError, Canonicalizer};
use serde_json::{json, Value};

/// Rich nested fixture: objects at three depths, arrays, scalars of every kind.
fn fixture() -> Value {
    json!({
        "zebra": 1,
        "alpha": {
            "zulu": [3, 1, 2, {"k": "v", "a": [true, false]}],
            "mike": null
        },
        "delta": "π unicode ✓ \"quoted\"",
        "charlie": 3.5,
        "bravo": -42
    })
}

/// Compute the sha manifest for a value in a fresh scope.
fn sha_manifest(value: &Value) -> String {
    JsonCanonicalizer::new().hash(value.clone()).unwrap()
}

// --- Court 1: two-pass determinism (IDENTITY_VACUOUS on mismatch) -------------------

#[test]
fn two_pass_determinism_fresh_scopes_produce_identical_sha_manifest() {
    let input = fixture();

    // Two independent passes, deliberately in fresh scopes so no cached state
    // could be shared between them.
    let pass1 = {
        let v = input.clone();
        sha_manifest(&v)
    };
    let pass2 = {
        let v = input.clone();
        sha_manifest(&v)
    };

    assert_eq!(
        pass1, pass2,
        "IDENTITY_VACUOUS: two passes over the identical projection produced \
         different sha manifests — canonicalization is not deterministic"
    );
    assert_eq!(pass1.len(), 64, "manifest must be a sha-256 hex string");
}

#[test]
fn two_pass_determinism_also_holds_for_pretty_and_str_entrypoints() {
    let input = fixture();
    let raw = serde_json::to_string(&input).unwrap();

    let compact_a = JsonCanonicalizer::new()
        .canonicalize(input.clone())
        .unwrap();
    let compact_b = JsonCanonicalizer::new()
        .canonicalize(input.clone())
        .unwrap();
    assert_eq!(
        compact_a, compact_b,
        "IDENTITY_VACUOUS: compact canonicalization diverged between passes"
    );

    let pretty_a = JsonCanonicalizer::new_pretty()
        .canonicalize(input.clone())
        .unwrap();
    let pretty_b = JsonCanonicalizer::new_pretty()
        .canonicalize(input.clone())
        .unwrap();
    assert_eq!(
        pretty_a, pretty_b,
        "IDENTITY_VACUOUS: pretty canonicalization diverged between passes"
    );

    let via_str_a = canonicalize_json_str(&raw).unwrap();
    let via_str_b = canonicalize_json_str(&raw).unwrap();
    assert_eq!(
        via_str_a, via_str_b,
        "IDENTITY_VACUOUS: canonicalize_json_str diverged between passes"
    );
}

// --- Court 2: exhaustive single-field mutation --------------------------------------

#[test]
fn every_single_field_mutation_of_the_fixture_changes_the_digest() {
    let base = sha_manifest(&fixture());

    // fn-pointer mutant array — one mutation per semantically meaningful field.
    // Every mutant MUST move the digest; a mutant that doesn't means the
    // identity lattice is blind to that field (vacuous identity).
    let mutants: [fn(&mut Value); 10] = [
        |v| v["zebra"] = json!(2),
        |v| v["alpha"]["mike"] = json!("not-null"),
        |v| v["alpha"]["zulu"][0] = json!(999),
        |v| v["alpha"]["zulu"][3]["k"] = json!("moved"),
        |v| v["alpha"]["zulu"][3]["a"][0] = json!(false),
        |v| v["delta"] = json!("different"),
        |v| v["charlie"] = json!(3.5001),
        |v| v["bravo"] = json!(-43),
        |v| {
            v["new_field"] = json!("added");
        },
        |v| {
            if let Value::Object(map) = v {
                map.remove("zebra");
            }
        },
    ];

    for (i, m) in mutants.iter().enumerate() {
        let mut mutated = fixture();
        m(&mut mutated);
        assert_ne!(
            sha_manifest(&mutated),
            base,
            "mutant {i} left the digest unchanged — identity is vacuous over this field"
        );
    }
}

// --- Court 3: key-reordering invariance ----------------------------------------------

#[test]
fn key_reordering_produces_identical_canonical_form() {
    // Same object, keys inserted in opposite orders. Canonical output must be
    // byte-identical: key order carries zero identity bits.
    let forward = json!({
        "a": 1,
        "b": 2,
        "c": {"x": 10, "y": 20},
        "d": [1, 2, 3]
    });

    // Rebuild with reversed key insertion order (deep).
    let mut reversed = serde_json::Map::new();
    reversed.insert("d".to_string(), json!([1, 2, 3]));
    let mut inner = serde_json::Map::new();
    inner.insert("y".to_string(), json!(20));
    inner.insert("x".to_string(), json!(10));
    reversed.insert("c".to_string(), Value::Object(inner));
    reversed.insert("b".to_string(), json!(2));
    reversed.insert("a".to_string(), json!(1));
    let reversed = Value::Object(reversed);

    let canon_forward = JsonCanonicalizer::new().hash(forward).unwrap();
    let canon_reversed = JsonCanonicalizer::new().hash(reversed).unwrap();

    assert_eq!(
        canon_forward, canon_reversed,
        "key insertion order leaked into the canonical identity"
    );
    // And pin the actual canonical string: keys sorted, compact.
    let canonical_str = canonicalize_json_str(r#"{"z":1,"a":{"q":1,"b":2}}"#).unwrap();
    assert_eq!(canonical_str, r#"{"a":{"b":2,"q":1},"z":1}"#);
}

// --- Court 4: array order is semantic; invalid input is a typed error ----------------

#[test]
fn semantic_array_reorder_changes_the_digest() {
    let ordered = json!({"log": ["commit-a", "commit-b", "commit-c"]});
    let mut swapped = ordered.clone();
    swapped["log"] = json!(["commit-c", "commit-b", "commit-a"]);

    let d1 = sha_manifest(&ordered);
    let d2 = sha_manifest(&swapped);
    assert_ne!(
        d1, d2,
        "array reorder did not change the digest — array order is semantic and \
         must be identity-relevant"
    );
}

#[test]
fn invalid_input_returns_typed_err_never_silent_fallback() {
    // NaN is not valid JSON — must be a typed CanonicalError, never a
    // fallback canonicalization of a degraded value.
    let err = canonicalize_json_str("{\"x\": NaN}").unwrap_err();
    assert!(
        matches!(err, CanonicalError::Serialization(_)),
        "expected CanonicalError::Serialization, got: {err:?}"
    );

    // Truncated input.
    let err = canonicalize_json_str("{\"x\": ").unwrap_err();
    assert!(
        matches!(err, CanonicalError::Serialization(_)),
        "expected CanonicalError::Serialization, got: {err:?}"
    );

    // Single-quoted (non-JSON) input.
    let err = canonicalize_json_str("{'x': 1}").unwrap_err();
    assert!(
        matches!(err, CanonicalError::Serialization(_)),
        "expected CanonicalError::Serialization, got: {err:?}"
    );

    // `canonicalize_json_str` on valid input must still work after all those
    // errors — proves errors are returned, not a poisoned/global fallback state.
    assert_eq!(canonicalize_json_str("{\"x\":1}").unwrap(), "{\"x\":1}");
}

#[test]
fn serde_json_number_edge_cases_do_not_fall_back_silently() {
    // serde_json cannot represent NaN/Infinity in a Value at all: constructing
    // them from f64 must fail loudly at the boundary we control.
    let v = serde_json::Value::from(f64::NAN);
    // serde_json maps NaN to Null on construction — the canonicalizer must
    // treat that Null as a real Null (identity-visible), not silently drop it.
    let with_null = json!({"maybe": v, "other": 1});
    let with_explicit_null = json!({"maybe": null, "other": 1});
    assert_eq!(
        sha_manifest(&with_null),
        sha_manifest(&with_explicit_null),
        "NaN-coerced Null must canonicalize identically to explicit Null"
    );
    let without_null = json!({"other": 1});
    assert_ne!(
        sha_manifest(&with_null),
        sha_manifest(&without_null),
        "explicit Null field must change the digest — dropping it silently is a fallback"
    );
}
