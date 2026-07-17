#![cfg(all(test, feature = "cognition"))]

//! Direct tests against the real, `cognition`-feature-gated
//! [`ChatmanEngine::consult_breed`] and [`BreedWitness::into_authority`].
//!
//! Before this module existed, `just test`'s `cargo test --all-features`
//! compiled these two production functions on every run but never called
//! either of them (confirmed by grep: no caller anywhere in `src/` or
//! `tests/`). The only exercised copy of these authority laws was a
//! hand-duplicated reimplementation, `dispatch_agent` in
//! `tests/chatman_engine_acceptance/harness/mod.rs`, whose own doc comment
//! says it exists "without requiring that feature" — i.e. it was built
//! specifically to work around, not exercise, `consult_breed`. A real
//! regression in `consult_breed`'s refusal order, messages, or the
//! `into_authority` law could ship uncaught.
//!
//! These tests drive the three refusal branches that need no external breed
//! fixture (override request, unpermitted breed, empty covering receipts)
//! directly against the production function, plus the always-refuses
//! `into_authority` law. They do not exercise the successful-dispatch path
//! (`dispatch_breed` succeeding), which needs a real `wasm4pm_cognition`
//! breed fixture and covering receipts — out of scope for this gap.

use super::*;
use crate::chatman::abi::ProfileId;

const PROFILE_IRI: &str = "profile:engine-cognition-test";

/// Minimal, law-valid [`EngineProfile`] parameterized only on
/// `breed_permits` — same construction as `engine_test::test_profile`,
/// varying the one field these tests need to control.
fn profile_with_breeds(breed_permits: Vec<String>) -> Result<EngineProfile, Refusal> {
    let profile_id = ProfileId::new(PROFILE_IRI);
    let gates = ProfileGates::new(profile_id.clone(), ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let symbol_table = ProfileSymbolTable::build(profile_id, vec!["<urn:chatman:t0>".to_string()])?;
    Ok(EngineProfile {
        gates,
        symbol_table,
        admission: AdmissionSpec {
            constraint_names: vec!["c0".to_string()],
            required_mask: 0,
            forbidden_mask: 0,
            set_on_admit: 0,
            clear_on_admit: 0,
        },
        breed_permits,
    })
}

fn engine_with_breeds(breed_permits: Vec<String>) -> Result<ChatmanEngine, Refusal> {
    ChatmanEngine::in_memory(profile_with_breeds(breed_permits)?)
}

#[test]
fn consult_breed_refuses_agent_override_before_checking_permits() -> Result<(), Refusal> {
    // Arrange: the breed is NOT in the permit list at all -- proves the
    // override law is checked first, independent of permit state.
    let engine = engine_with_breeds(Vec::new())?;
    let input = wasm4pm_cognition::breeds::BreedInput::default();

    // Act
    let result = engine.consult_breed("mycin", &input, &[], true);

    // Assert
    match result {
        Err(Refusal::AgentOverrideDenied(msg)) => {
            assert!(
                msg.contains("mycin"),
                "message should name the breed: {msg}"
            );
            Ok(())
        }
        other => Err(Refusal::ValidationFailed(format!(
            "wanted AgentOverrideDenied, got {other:?}"
        ))),
    }
}

#[test]
fn consult_breed_refuses_unpermitted_breed() -> Result<(), Refusal> {
    // Arrange: empty permit list, no override requested.
    let engine = engine_with_breeds(Vec::new())?;
    let input = wasm4pm_cognition::breeds::BreedInput::default();

    // Act
    let result = engine.consult_breed("mycin", &input, &[], false);

    // Assert
    match result {
        Err(Refusal::BreedUnpermitted(msg)) => {
            assert!(
                msg.contains("mycin"),
                "message should name the breed: {msg}"
            );
            Ok(())
        }
        other => Err(Refusal::ValidationFailed(format!(
            "wanted BreedUnpermitted, got {other:?}"
        ))),
    }
}

#[test]
fn consult_breed_refuses_permitted_breed_without_covering_receipt() -> Result<(), Refusal> {
    // Arrange: the breed IS permitted, so evaluation reaches the
    // nondeterministic-operator receipt law next.
    let engine = engine_with_breeds(vec!["mycin".to_string()])?;
    let input = wasm4pm_cognition::breeds::BreedInput::default();

    // Act
    let result = engine.consult_breed("mycin", &input, &[], false);

    // Assert
    match result {
        Err(Refusal::NondeterministicOperatorRequiresReceipt(msg)) => {
            assert!(
                msg.contains("mycin"),
                "message should name the breed: {msg}"
            );
            Ok(())
        }
        other => Err(Refusal::ValidationFailed(format!(
            "wanted NondeterministicOperatorRequiresReceipt, got {other:?}"
        ))),
    }
}

#[test]
fn breed_witness_into_authority_always_refuses() -> Result<(), Refusal> {
    // BreedWitness's fields are all `pub`, and this module has the same
    // `use super::*` access to every other engine-module type that
    // `engine_test.rs` already relies on -- constructing one directly here
    // (rather than only via a successful `consult_breed` dispatch) isolates
    // the `into_authority` law from the dispatch path it would otherwise be
    // coupled to.
    let witness = BreedWitness {
        breed: "mycin".to_string(),
        explanation: "test explanation".to_string(),
        selected: None,
    };

    // Act + Assert
    match witness.into_authority() {
        Err(Refusal::WitnessNotAuthority(msg)) => {
            assert!(
                msg.contains("mycin"),
                "message should name the breed: {msg}"
            );
            Ok(())
        }
        other => Err(Refusal::ValidationFailed(format!(
            "wanted WitnessNotAuthority, got {other:?}"
        ))),
    }
}
