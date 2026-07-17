//! Property tests over `DialectRouter`'s least-expressive-route (LER) law.
//!
//! The router's own in-module tests (`crates/praxis-graphlaw/src/chatman/router.rs`)
//! already cover `ler_ordering_property_over_all_dialect_pairs` with a
//! hand-rolled double loop. This file exercises the same law through
//! `proptest`, generating arbitrary `(Dialect, Dialect)` pairs and arbitrary
//! `QueryShape`s, so the property is checked over randomized inputs (with
//! shrinking) rather than only the fixed dialect matrix.

use proptest::prelude::*;

use praxis_graphlaw::chatman::abi::{ProfileId, Refusal};
use praxis_graphlaw::chatman::router::{Dialect, DialectRouter, ProfileGates, QueryShape};

/// Arbitrary `Dialect` strategy over the six real variants (checked against
/// `router.rs`'s actual `Dialect::ALL`; no invented variants).
fn any_dialect() -> impl Strategy<Value = Dialect> {
    prop_oneof![
        Just(Dialect::Triple8Pattern),
        Just(Dialect::ShaclCore),
        Just(Dialect::SparqlSelect),
        Just(Dialect::SparqlConstruct),
        Just(Dialect::OwlRl),
        Just(Dialect::N3),
    ]
}

proptest! {
    /// For all dialect pairs (a, b) with a < b, when only {a, b} are enabled
    /// (N3 never in the actuation mask) and the query shape's capability
    /// floor is at most `a`, `decide` never returns anything strictly more
    /// expressive than `a` — the router picks the least-expressive route
    /// subject to the enabled gates, matching `Dialect`'s derived `Ord`.
    #[test]
    fn router_never_exceeds_least_expressive_enabled_dialect(
        a in any_dialect(),
        b in any_dialect(),
        constraint_count in 0u8..8,
    ) {
        prop_assume!(a < b);

        let profile_id = ProfileId::new("proptest-profile");
        let gates = ProfileGates::new(profile_id, a.mask_bit() | b.mask_bit(), 0, 8)
            .expect("a<b enabled mask with empty actuation mask is always lawful");
        let router = DialectRouter::new(gates);

        // Build a shape whose capability floor is exactly `a`.
        let shape = QueryShape {
            constraint_count,
            requires_construct: a >= Dialect::SparqlConstruct,
            requires_owl: a >= Dialect::OwlRl,
            requires_n3_builtins: a == Dialect::N3,
            wants_actuation: false,
        };

        match router.decide(&shape) {
            Ok(decision) => {
                prop_assert!(
                    decision.dialect <= a,
                    "LER violated: router chose {:?} over enabled floor {:?}",
                    decision.dialect,
                    a
                );
            }
            Err(refusal) => {
                // The only lawful refusal here is a hot-budget miss (shape
                // exceeds max_hot_constraints and no qualifying warm dialect
                // is enabled) since `a` is otherwise always capable and
                // enabled.
                prop_assert!(
                    matches!(refusal, Refusal::WarmPathRequired(_)),
                    "unexpected refusal for a capable+enabled floor: {refusal:?}"
                );
            }
        }
    }

    /// `verify_claim` accepts the router's own decision and never accepts a
    /// claim strictly more expressive than what `decide` would choose.
    #[test]
    fn verify_claim_never_accepts_over_expressive_claim(
        enabled_bits in 0u8..64, // 6 dialect bits fit in 0..64
        constraint_count in 0u8..8,
        wants_construct in any::<bool>(),
    ) {
        let profile_id = ProfileId::new("proptest-profile-verify");
        // Never set the N3 bit in actuation (gate law); keep actuation empty.
        let gates = ProfileGates::new(profile_id, enabled_bits, 0, 8)
            .expect("empty actuation mask is always a lawful subset");
        let router = DialectRouter::new(gates);

        let shape = QueryShape {
            constraint_count,
            requires_construct: wants_construct,
            requires_owl: false,
            requires_n3_builtins: false,
            wants_actuation: false,
        };

        if let Ok(computed) = router.decide(&shape) {
            // The router's own decision must always verify against itself.
            prop_assert!(router.verify_claim(&shape, &computed).is_ok());

            // Any strictly-more-expressive claim naming the next dialect
            // (if one exists) is rejected as a LER violation.
            if let Some(next) = Dialect::ALL.iter().find(|d| **d > computed.dialect) {
                let mut inflated = computed.clone();
                inflated.dialect = *next;
                let err = router.verify_claim(&shape, &inflated);
                prop_assert!(
                    matches!(err, Err(Refusal::LeastExpressiveRouteViolation(_))),
                    "expected LeastExpressiveRouteViolation, got {err:?}"
                );
            }
        }
    }
}

// ---- assert_matches! payload guards over the real Refusal API -------------

use chicago_tdd_tools::assert_matches;
use chicago_tdd_tools::prelude::*;

test!(unsupported_dialect_refusal_names_the_offender, {
    // Arrange: nothing enabled at all.
    let profile_id = ProfileId::new("proptest-unsupported");
    let gates = ProfileGates::new(profile_id, 0, 0, 8)?;
    let router = DialectRouter::new(gates);
    let shape = QueryShape {
        constraint_count: 1,
        requires_construct: false,
        requires_owl: false,
        requires_n3_builtins: false,
        wants_actuation: false,
    };

    // Act
    let result = router.decide(&shape);

    // Assert: payload guard on the message naming the profile.
    assert_matches!(
        result,
        Err(Refusal::UnsupportedDialect(ref msg)) if msg.contains("proptest-unsupported")
    );
    Ok::<(), Refusal>(())
});

test!(route_decision_mismatch_refusal_on_hash_drift, {
    // Arrange
    let profile_id = ProfileId::new("proptest-mismatch");
    let gates = ProfileGates::new(profile_id, ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)?;
    let router = DialectRouter::new(gates);
    let shape = QueryShape {
        constraint_count: 1,
        requires_construct: false,
        requires_owl: false,
        requires_n3_builtins: false,
        wants_actuation: false,
    };
    let mut claimed = router.decide(&shape)?;
    claimed.decision_hash = praxis_graphlaw::chatman::abi::Digest::new("0".repeat(64));

    // Act
    let result = router.verify_claim(&shape, &claimed);

    // Assert: RouteDecisionMismatch, not LeastExpressiveRouteViolation, since
    // the dialect itself did not change — only the sealed hash drifted.
    assert_matches!(result, Err(Refusal::RouteDecisionMismatch(_)));
    Ok::<(), Refusal>(())
});

test!(least_expressive_route_violation_names_both_dialects, {
    // Arrange: a shape whose true floor is Triple8Pattern under a router
    // that enables everything.
    let profile_id = ProfileId::new("proptest-ler-violation");
    let all_enabled = ProfileGates::DEFAULT_ENABLED_MASK | Dialect::N3.mask_bit();
    let gates = ProfileGates::new(profile_id, all_enabled, 0, 8)?;
    let router = DialectRouter::new(gates);
    let shape = QueryShape {
        constraint_count: 1,
        requires_construct: false,
        requires_owl: false,
        requires_n3_builtins: false,
        wants_actuation: false,
    };
    let computed = router.decide(&shape)?;
    assert_eq!(computed.dialect, Dialect::Triple8Pattern);

    let mut inflated = computed.clone();
    inflated.dialect = Dialect::N3;
    inflated.route = Dialect::N3.route();

    // Act
    let result = router.verify_claim(&shape, &inflated);

    // Assert: payload guard naming both the claimed and computed dialects.
    assert_matches!(
        result,
        Err(Refusal::LeastExpressiveRouteViolation(ref msg))
            if msg.contains("N3") && msg.contains("Triple8Pattern")
    );
    Ok::<(), Refusal>(())
});
