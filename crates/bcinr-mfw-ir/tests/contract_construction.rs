//! External-crate-perspective regression test for
//! `SemanticOptimizationContract`'s construction guarantee.
//!
//! `contracts.rs`'s own module doc comment claims the type is
//! "constructible only when the cited FormalLawRef is Proven" — before this
//! fix that was false as written: every field was `pub`, so any downstream
//! crate could build a full `SemanticOptimizationContract` via
//! struct-literal syntax carrying a non-`Proven` law, completely bypassing
//! `new`'s refusal (verified adversarially this session: a scratch
//! integration test doing exactly that compiled and produced a live
//! contract wrapping a `Blocked` law; deleted after confirming the bug,
//! then the fields were made private and the same scratch construction
//! attempt was reconfirmed to fail with `error[E0451]: field ... is
//! private`).
//!
//! This file lives under `tests/` (not `src/contracts.rs`'s own `mod
//! tests`) specifically because integration tests compile as a genuinely
//! separate crate linking against the public API — unlike a unit test
//! inside `contracts.rs` itself (which is a descendant module of the type's
//! defining module and would still see private fields), this is the only
//! way to prove the guarantee holds from a real downstream consumer's
//! perspective.

use bcinr_mfw_ir::{
    ConsequenceHorizonId, Digest, FormalStanding, SemanticOptimizationContract,
    TransformationProfileId, LAW_QLENS_RATIO, LAW_SPECTRUM_ESTIMATOR,
};

#[test]
fn new_is_the_only_construction_path_and_the_public_api_still_works() {
    let law = LAW_QLENS_RATIO;
    assert_eq!(law.standing, FormalStanding::Proven);

    let contract = SemanticOptimizationContract::new(
        law,
        ConsequenceHorizonId(Digest::hash(b"horizon")),
        TransformationProfileId(7),
        vec!["fiber is constant over horizon".to_string()],
    )
    .expect("a Proven law must still construct successfully via the public API");

    // Read access remains available through accessors, even though the
    // fields themselves are now private.
    assert_eq!(contract.law().standing, FormalStanding::Proven);
    assert_eq!(
        contract.consequence_horizon(),
        ConsequenceHorizonId(Digest::hash(b"horizon"))
    );
    assert_eq!(contract.transformation(), TransformationProfileId(7));
    assert_eq!(
        contract.assumptions(),
        &["fiber is constant over horizon".to_string()]
    );
}

#[test]
fn a_non_proven_law_is_still_refused_via_the_public_api() {
    let blocked_law = LAW_SPECTRUM_ESTIMATOR;
    assert_eq!(blocked_law.standing, FormalStanding::Blocked);

    let result = SemanticOptimizationContract::new(
        blocked_law,
        ConsequenceHorizonId(Digest::hash(b"h")),
        TransformationProfileId(1),
        vec![],
    );
    assert!(result.is_err());

    // Note what this test file does *not* (and structurally cannot)
    // contain: a struct-literal construction of `SemanticOptimizationContract`
    // with `blocked_law` bypassing `new` entirely. Before this fix, that
    // compiled and succeeded (the exact adversarial case this regression
    // guards against); after this fix, attempting it is a compile error
    // (`error[E0451]: field ... is private`) rather than something a
    // runtime assertion could even express here.
}
