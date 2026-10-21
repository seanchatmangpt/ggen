//! @generated-do-not-edit
//!
//! Placeholder generated-mutation module (wave-1 lane A10, CanonicalIdentityLattice).
//! Pattern exemplar for the court-compiler seam (L11): when the mutation court
//! compiler lands, this file is regenerated per-type from the mutant catalog.
//! Until then it exercises the pattern by hand for ONE type (`PackIdentity`)
//! so the generated-mutation surface compiles, runs, and is non-vacuous.
//!
//! Regeneration contract: a regenerated file must keep this module's public
//! surface (`mutants()`, `fixture()`) or the consuming court in
//! `generated_mutation_court.rs` fails to compile — compile-failure is the
//! drift signal, by design.

/// The one type the placeholder pattern exercises.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PackIdentity {
    pub id: String,
    pub version: String,
    pub trust_tier: u8,
}

/// Canonical fixture for the type.
#[must_use]
pub fn fixture() -> PackIdentity {
    PackIdentity {
        id: "pack:example".to_string(),
        version: "26.10.20".to_string(),
        trust_tier: 2,
    }
}

/// Exhaustive single-field mutant list: every guarded field of `PackIdentity`.
/// A field missing here is an identity hole; a mutant here that fails to move
/// the digest fails the consuming court (vacuous gate).
#[must_use]
pub fn mutants() -> Vec<fn(&mut PackIdentity)> {
    vec![
        |p| p.id.push('x'),
        |p| p.version = "0.0.1".to_string(),
        |p| p.trust_tier = p.trust_tier.wrapping_add(1),
    ]
}
