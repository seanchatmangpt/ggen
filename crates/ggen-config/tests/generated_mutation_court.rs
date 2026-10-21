//! Court consuming the generated-mutation placeholder module.
//! The module under `tests/generated/` is marked @generated-do-not-edit —
//! this court proves the generated pattern is non-vacuous for its one type.

#![allow(clippy::unwrap_used, clippy::expect_used)]

#[path = "generated/generated_mutation.rs"]
mod generated_mutation;

use generated_mutation::{fixture, mutants};
use ggen_config::canonical::json::JsonCanonicalizer;
use ggen_config::canonical::Canonicalizer;

/// Every generated mutant must move the digest — anti-vacuity over the
/// generated mutation surface.
#[test]
fn generated_mutants_all_change_the_digest() {
    let base = {
        let json_val = serde_json::to_value(fixture()).unwrap();
        JsonCanonicalizer::new().hash(json_val).unwrap()
    };

    let ms = mutants();
    assert!(
        ms.len() >= 3,
        "generated mutant list shrank — regeneration dropped fields"
    );

    for (i, m) in ms.iter().enumerate() {
        let mut p = fixture();
        m(&mut p);
        let mutated = serde_json::to_value(&p).unwrap();
        let d = JsonCanonicalizer::new().hash(mutated).unwrap();
        assert_ne!(
            d, base,
            "generated mutant {i} left the digest unchanged — vacuous gate"
        );
    }
}

/// Canonicalization of the type must be deterministic and key-order free
/// (serde struct field order must not leak into identity).
#[test]
fn generated_type_canonicalizes_deterministically() {
    let a = serde_json::to_value(fixture()).unwrap();
    let b = serde_json::to_value(fixture()).unwrap();
    let ha = JsonCanonicalizer::new().hash(a).unwrap();
    let hb = JsonCanonicalizer::new().hash(b).unwrap();
    assert_eq!(
        ha, hb,
        "IDENTITY_VACUOUS: same type instance, different digests"
    );
}
