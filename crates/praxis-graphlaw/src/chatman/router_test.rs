#![cfg(test)]

//! Chicago-style (state-based, real collaborators, Arrange-Act-Assert)
//! in-module tests: full N3 gate matrix, the LER ordering property over
//! all dialect pairs, and decision-hash determinism.

use super::*;

fn profile(enabled: u8, actuation: u8, max_hot: u8) -> ProfileGates {
    match ProfileGates::new(ProfileId::new("test-profile"), enabled, actuation, max_hot) {
        Ok(g) => g,
        Err(refusal) => unreachable!("test profile must validate: {refusal}"),
    }
}

fn shape(
    constraint_count: u8, requires_construct: bool, requires_owl: bool, requires_n3_builtins: bool,
    wants_actuation: bool,
) -> QueryShape {
    QueryShape {
        constraint_count,
        requires_construct,
        requires_owl,
        requires_n3_builtins,
        wants_actuation,
    }
}

fn ok_or_unreachable(result: Result<RouteDecision, Refusal>) -> RouteDecision {
    match result {
        Ok(d) => d,
        Err(refusal) => unreachable!("expected Ok decision: {refusal}"),
    }
}

const ALL_ENABLED: u8 = ProfileGates::DEFAULT_ENABLED_MASK | 1 << 5;

// ---- Dialect order / route / mask laws --------------------------------

#[test]
fn dialect_order_is_ascending_expressive_power() {
    for window in Dialect::ALL.windows(2) {
        assert!(window[0] < window[1], "{:?} !< {:?}", window[0], window[1]);
    }
}

#[test]
fn routes_map_hot_warm_cold() {
    assert_eq!(Dialect::Triple8Pattern.route(), Route::Hot);
    assert_eq!(Dialect::ShaclCore.route(), Route::Warm);
    assert_eq!(Dialect::SparqlSelect.route(), Route::Warm);
    assert_eq!(Dialect::SparqlConstruct.route(), Route::Warm);
    assert_eq!(Dialect::OwlRl.route(), Route::Warm);
    assert_eq!(Dialect::N3.route(), Route::Cold);
}

#[test]
fn mask_bits_are_distinct_powers_of_two() {
    let mut seen: u8 = 0;
    for d in Dialect::ALL {
        let bit = d.mask_bit();
        assert_eq!(bit.count_ones(), 1);
        assert_eq!(seen & bit, 0, "duplicate mask bit for {d:?}");
        seen |= bit;
    }
}

#[test]
fn default_enabled_mask_excludes_n3() {
    assert_eq!(
        ProfileGates::DEFAULT_ENABLED_MASK & Dialect::N3.mask_bit(),
        0
    );
    for d in Dialect::ALL {
        if d != Dialect::N3 {
            assert_ne!(ProfileGates::DEFAULT_ENABLED_MASK & d.mask_bit(), 0);
        }
    }
}

// ---- ProfileGates validation -------------------------------------------

#[test]
fn gates_refuse_n3_in_actuation_mask() {
    let err = ProfileGates::new(ProfileId::new("p"), ALL_ENABLED, Dialect::N3.mask_bit(), 8);
    assert!(
        matches!(err, Err(Refusal::N3ActuationRefused(_))),
        "{err:?}"
    );
}

#[test]
fn gates_refuse_actuation_not_subset_of_enabled() {
    let err = ProfileGates::new(
        ProfileId::new("p"),
        Dialect::Triple8Pattern.mask_bit(),
        Dialect::ShaclCore.mask_bit(),
        8,
    );
    assert!(matches!(err, Err(Refusal::ValidationFailed(_))), "{err:?}");
}

#[test]
fn gates_refuse_hot_budget_over_eight() {
    let err = ProfileGates::new(ProfileId::new("p"), ALL_ENABLED, 0, 9);
    assert!(matches!(err, Err(Refusal::ValidationFailed(_))), "{err:?}");
}

// ---- N3 gate matrix ------------------------------------------------------
// Axes: N3 enabled × requires_n3_builtins × wants_actuation (2×2×2 = 8).

#[test]
fn n3_gate_matrix_full() {
    for n3_on in [false, true] {
        for requires_n3 in [false, true] {
            for wants_actuation in [false, true] {
                let enabled = if n3_on {
                    ALL_ENABLED
                } else {
                    ProfileGates::DEFAULT_ENABLED_MASK
                };
                // Actuation permitted on every non-N3 dialect.
                let router =
                    DialectRouter::new(profile(enabled, ProfileGates::DEFAULT_ENABLED_MASK, 8));
                let s = shape(2, false, false, requires_n3, wants_actuation);
                let result = router.decide(&s);
                match (n3_on, requires_n3, wants_actuation) {
                    (_, false, _) => {
                        let decision = ok_or_unreachable(result);
                        assert_eq!(decision.dialect, Dialect::Triple8Pattern);
                        assert_eq!(decision.route, Route::Hot);
                    }
                    (false, true, _) => assert!(
                        matches!(result, Err(Refusal::N3UnavailableByProfile(_))),
                        "n3 disabled + required must refuse by profile: {result:?}"
                    ),
                    (true, true, true) => assert!(
                        matches!(result, Err(Refusal::N3ActuationRefused(_))),
                        "n3 + actuation must refuse: {result:?}"
                    ),
                    (true, true, false) => {
                        let decision = ok_or_unreachable(result);
                        assert_eq!(decision.dialect, Dialect::N3);
                        assert_eq!(decision.route, Route::Cold);
                    }
                }
            }
        }
    }
}

// ---- Least-expressive-route law ------------------------------------------

#[test]
fn ler_ordering_property_over_all_dialect_pairs() {
    // For every ordered pair (a, b) with a < b: when both are enabled and
    // the shape's floor is ≤ a, decide never returns anything more
    // expressive than a.
    for a in Dialect::ALL {
        for b in Dialect::ALL {
            if a >= b {
                continue;
            }
            let router = DialectRouter::new(profile(a.mask_bit() | b.mask_bit(), 0, 8));
            // Shape whose capability floor is exactly `a`.
            let s = shape(
                1,
                a >= Dialect::SparqlConstruct,
                a >= Dialect::OwlRl,
                a == Dialect::N3,
                false,
            );
            assert!(s.minimum_dialect() <= a, "floor must not exceed a");
            let decision = ok_or_unreachable(router.decide(&s));
            assert!(
                decision.dialect <= a,
                "LER violated: chose {:?} over enabled {:?}",
                decision.dialect,
                a
            );
        }
    }
}

#[test]
fn verify_claim_refuses_more_expressive_claim() {
    let router = DialectRouter::new(profile(ALL_ENABLED, 0, 8));
    let s = shape(1, false, false, false, false);
    let computed = ok_or_unreachable(router.decide(&s));
    assert_eq!(computed.dialect, Dialect::Triple8Pattern);
    let inflated = RouteDecision {
        dialect: Dialect::SparqlSelect,
        route: Route::Warm,
        profile_hash: computed.profile_hash.clone(),
        decision_hash: computed.decision_hash.clone(),
    };
    let err = router.verify_claim(&s, &inflated);
    assert!(
        matches!(err, Err(Refusal::LeastExpressiveRouteViolation(_))),
        "{err:?}"
    );
}

#[test]
fn verify_claim_refuses_hash_drift() {
    let router = DialectRouter::new(profile(ALL_ENABLED, 0, 8));
    let s = shape(1, false, false, false, false);
    let mut claimed = ok_or_unreachable(router.decide(&s));
    claimed.decision_hash = Digest::new("0000".to_string());
    let err = router.verify_claim(&s, &claimed);
    assert!(
        matches!(err, Err(Refusal::RouteDecisionMismatch(_))),
        "{err:?}"
    );
}

#[test]
fn verify_claim_accepts_exact_decision() {
    let router = DialectRouter::new(profile(ALL_ENABLED, 0, 8));
    let s = shape(3, true, false, false, false);
    let claimed = ok_or_unreachable(router.decide(&s));
    assert_eq!(claimed.dialect, Dialect::SparqlConstruct);
    assert!(router.verify_claim(&s, &claimed).is_ok());
}

// ---- Hot budget / refusal coverage ----------------------------------------

#[test]
fn over_budget_falls_to_warm_when_available() {
    let router = DialectRouter::new(profile(ProfileGates::DEFAULT_ENABLED_MASK, 0, 4));
    let s = shape(5, false, false, false, false);
    let decision = ok_or_unreachable(router.decide(&s));
    assert_eq!(decision.dialect, Dialect::ShaclCore);
    assert_eq!(decision.route, Route::Warm);
}

#[test]
fn over_budget_with_no_warm_dialect_is_warm_path_required() {
    let router = DialectRouter::new(profile(Dialect::Triple8Pattern.mask_bit(), 0, 4));
    let s = shape(5, false, false, false, false);
    let err = router.decide(&s);
    assert!(matches!(err, Err(Refusal::WarmPathRequired(_))), "{err:?}");
}

#[test]
fn nothing_enabled_is_unsupported_dialect() {
    let router = DialectRouter::new(profile(0, 0, 8));
    let s = shape(1, false, false, false, false);
    let err = router.decide(&s);
    assert!(
        matches!(err, Err(Refusal::UnsupportedDialect(_))),
        "{err:?}"
    );
}

#[test]
fn actuation_skips_non_permitted_dialects() {
    // Actuation only permitted on ShaclCore; a hot-capable shape wanting
    // actuation must land on ShaclCore, not Triple8Pattern.
    let router = DialectRouter::new(profile(
        ProfileGates::DEFAULT_ENABLED_MASK,
        Dialect::ShaclCore.mask_bit(),
        8,
    ));
    let s = shape(1, false, false, false, true);
    let decision = ok_or_unreachable(router.decide(&s));
    assert_eq!(decision.dialect, Dialect::ShaclCore);
}

// ---- Determinism ------------------------------------------------------------

#[test]
fn decision_hash_is_deterministic() {
    let router = DialectRouter::new(profile(ALL_ENABLED, 0, 8));
    let s = shape(2, true, false, false, false);
    let d1 = ok_or_unreachable(router.decide(&s));
    let d2 = ok_or_unreachable(router.decide(&s));
    assert_eq!(d1, d2);
    assert_eq!(d1.decision_hash, d2.decision_hash);
    assert_eq!(d1.profile_hash, d2.profile_hash);
}

#[test]
fn decision_hash_distinguishes_shapes_and_profiles() {
    let router = DialectRouter::new(profile(ALL_ENABLED, 0, 8));
    let d1 = ok_or_unreachable(router.decide(&shape(2, false, false, false, false)));
    let d2 = ok_or_unreachable(router.decide(&shape(3, false, false, false, false)));
    assert_ne!(d1.decision_hash, d2.decision_hash);

    let other = DialectRouter::new(profile(ALL_ENABLED, 0, 7));
    let d3 = ok_or_unreachable(other.decide(&shape(2, false, false, false, false)));
    assert_ne!(d1.decision_hash, d3.decision_hash);
    assert_ne!(d1.profile_hash, d3.profile_hash);
}

// ---------------------------------------------------------------------------
// N3 controlled execution surface (PROJ-779): cost bound + builtin whitelist.
// ---------------------------------------------------------------------------

/// N3-enabled gates, actuation left empty (N3 may never actuate anyway).
fn n3_enabled_gates() -> ProfileGates {
    profile(ALL_ENABLED, 0, 8)
}

/// N3-disabled gates (the audited default: PROJ-777/778's coverage).
fn n3_disabled_gates() -> ProfileGates {
    profile(ProfileGates::DEFAULT_ENABLED_MASK, 0, 8)
}

fn execution(builtin_whitelist_mask: u8, cost_bound_ticks: u64) -> N3ExecutionProfile {
    N3ExecutionProfile {
        builtin_whitelist_mask,
        cost_bound_ticks: N3Ticks(cost_bound_ticks),
    }
}

fn rule(rule_id: &str, builtins: &[N3Builtin], cost: u64) -> N3Rule {
    N3Rule {
        rule_id: rule_id.to_string(),
        builtins: builtins.to_vec(),
        declared_cost: N3Ticks(cost),
        direct_actuation_builtins: Vec::new(),
    }
}

/// Builds a rule that additionally declares one or more recognized
/// direct-actuation builtins (PROJ-780).
fn rule_requesting_actuation(
    rule_id: &str, builtins: &[N3Builtin], cost: u64, actuation_builtins: &[N3ActuationBuiltin],
) -> N3Rule {
    N3Rule {
        rule_id: rule_id.to_string(),
        builtins: builtins.to_vec(),
        declared_cost: N3Ticks(cost),
        direct_actuation_builtins: actuation_builtins.to_vec(),
    }
}

const LOG_EQUAL_ONLY_MASK: u8 = N3Builtin::LogEqualTo.mask_bit();
const ALL_BUILTINS_MASK: u8 = {
    let mut mask = 0u8;
    let mut i = 0;
    while i < N3Builtin::ALL.len() {
        mask |= N3Builtin::ALL[i].mask_bit();
        i += 1;
    }
    mask
};

// ---- N3Ticks / N3CostBound: unit-level accounting correctness ------------

#[test]
fn n3_cost_bound_exhausts_strictly_over_limit() {
    // Mirrors TickBudget's documented boundary: spending exactly the limit
    // is still within budget; the next tick over exhausts it.
    let mut b = N3CostBound::new(N3Ticks(8));
    for _ in 0..8 {
        assert!(b.consume(N3Ticks(1)), "spending within limit must stay ok");
    }
    assert!(b.is_exhausted(), "used == limit counts as exhausted");
    assert!(
        !b.consume(N3Ticks(1)),
        "the ninth tick must exceed the bound"
    );
}

#[test]
fn n3_cost_bound_consume_saturates_never_wraps() {
    let mut b = N3CostBound::new(N3Ticks(4));
    assert!(!b.consume(N3Ticks(u64::MAX)));
    assert!(!b.consume(N3Ticks(u64::MAX)));
    assert_eq!(b.used, N3Ticks(u64::MAX));
}

#[test]
fn n3_cost_bound_consume_zero_leaves_used_unchanged() {
    // declared_cost == 0 is a legal, real case (a bare graph-pattern rule
    // with no builtins may still declare zero cost); `consume` must be a
    // no-op on `used` while still reporting "within budget".
    let mut b = N3CostBound::new(N3Ticks(5));
    assert!(b.consume(N3Ticks(3)));
    assert_eq!(b.used, N3Ticks(3));
    assert!(
        b.consume(N3Ticks(0)),
        "zero-cost consumption must stay within budget"
    );
    assert_eq!(
        b.used,
        N3Ticks(3),
        "consuming zero ticks must not change `used`"
    );
}

#[test]
fn n3_builtin_mask_bits_are_distinct_powers_of_two() {
    let mut seen: u8 = 0;
    for b in N3Builtin::ALL {
        let bit = b.mask_bit();
        assert_eq!(bit.count_ones(), 1);
        assert_eq!(seen & bit, 0, "duplicate mask bit for {b:?}");
        seen |= bit;
    }
}

// ---- Requirement 1: capability gate (extends, does not duplicate, the ----
// ---- existing n3_gate_matrix_full coverage of DialectRouter::decide) -----

#[test]
fn n3_executor_refuses_without_capability_grant() {
    // The router's N3-disabled default (already covered for `decide` by
    // `n3_gate_matrix_full`); this proves the *new* N3Executor entry point
    // respects the same gate rather than bypassing it.
    let gates = n3_disabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("r1", &[], 1)];
    let err = executor.run(&rules);
    assert!(
        matches!(err, Err(Refusal::N3UnavailableByProfile(_))),
        "{err:?}"
    );
}

#[test]
fn n3_executor_admits_when_capability_granted_and_no_rules_violate_anything() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("r1", &[N3Builtin::LogEqualTo], 1)];
    let receipt = match executor.run(&rules) {
        Ok(r) => r,
        Err(e) => unreachable!("expected admission: {e:?}"),
    };
    assert_eq!(receipt.rules_admitted, vec!["r1".to_string()]);
    assert_eq!(receipt.ticks_used, N3Ticks(1));
}

// ---- Requirement 2: cost bound is really enforced, not declared-and- -----
// ---- ignored: incremental tracking, not a single total pre-check. --------

#[test]
fn n3_executor_refuses_when_a_single_rule_exceeds_the_bound_before_any_other_runs() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 5);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("too-big", &[], 6)];
    let err = executor.run(&rules);
    assert!(
        matches!(err, Err(Refusal::N3CostBoundExceeded(_))),
        "{err:?}"
    );
}

#[test]
fn n3_executor_admits_cumulative_cost_at_exactly_the_bound() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 10);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("a", &[], 4), rule("b", &[], 6)];
    let receipt = match executor.run(&rules) {
        Ok(r) => r,
        Err(e) => unreachable!("spending exactly the bound must be admitted: {e:?}"),
    };
    assert_eq!(receipt.ticks_used, N3Ticks(10));
    assert_eq!(
        receipt.rules_admitted,
        vec!["a".to_string(), "b".to_string()]
    );
}

#[test]
fn n3_executor_admits_zero_declared_cost_rule_without_consuming_budget() {
    // A rule with declared_cost == 0 has no `rule(..., 0)` coverage anywhere
    // else in this suite (PROJ-779). It must pass the builtin whitelist
    // check as normal, then leave the running budget untouched, so a
    // subsequent rule that needs the *entire original* bound still admits.
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 10);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![
        rule("zero-cost", &[N3Builtin::LogEqualTo], 0),
        rule("uses-full-budget", &[], 10),
    ];
    let receipt = match executor.run(&rules) {
        Ok(r) => r,
        Err(e) => unreachable!(
            "zero-cost rule must not consume any budget, leaving room for a rule needing \
             the full original bound: {e:?}"
        ),
    };
    assert_eq!(receipt.ticks_used, N3Ticks(10));
    assert_eq!(
        receipt.rules_admitted,
        vec!["zero-cost".to_string(), "uses-full-budget".to_string()]
    );
}

#[test]
fn n3_executor_refuses_mid_execution_when_a_later_rule_pushes_cumulative_over_bound() {
    // Proof the bound is tracked incrementally, not checked once against a
    // pre-declared total: the same first two rules that succeed alone are
    // refused only once a third, individually-small rule pushes the running
    // total over the bound.
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 10);
    let executor = N3Executor::new(&gates, &exec);
    let prefix = vec![rule("a", &[], 4), rule("b", &[], 4)];
    let prefix_receipt = match executor.run(&prefix) {
        Ok(r) => r,
        Err(e) => unreachable!("prefix alone (cumulative 8) must be admitted: {e:?}"),
    };
    assert_eq!(prefix_receipt.ticks_used, N3Ticks(8));

    let full = vec![rule("a", &[], 4), rule("b", &[], 4), rule("c", &[], 4)];
    let err = executor.run(&full);
    assert!(
        matches!(err, Err(Refusal::N3CostBoundExceeded(_))),
        "cumulative 12 over bound 10 must be refused mid-execution: {err:?}"
    );
}

// ---- Requirement 3: builtin whitelist, both directions --------------------

#[test]
fn n3_executor_refuses_rule_using_non_whitelisted_builtin() {
    let gates = n3_enabled_gates();
    let exec = execution(LOG_EQUAL_ONLY_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("uses-math-sum", &[N3Builtin::MathSum], 1)];
    let err = executor.run(&rules);
    assert!(matches!(err, Err(Refusal::N3BuiltinRefused(_))), "{err:?}");
}

#[test]
fn n3_executor_admits_rule_using_whitelisted_builtin() {
    let gates = n3_enabled_gates();
    let exec = execution(LOG_EQUAL_ONLY_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("uses-log-equalto", &[N3Builtin::LogEqualTo], 1)];
    let receipt = match executor.run(&rules) {
        Ok(r) => r,
        Err(e) => unreachable!("whitelisted builtin must be admitted: {e:?}"),
    };
    assert_eq!(receipt.rules_admitted, vec!["uses-log-equalto".to_string()]);
}

#[test]
fn n3_executor_refuses_non_whitelisted_builtin_before_consuming_its_cost() {
    // A rule whose builtin is refused must not silently spend its declared
    // cost against the bound; refusal short-circuits before any accounting.
    let gates = n3_enabled_gates();
    let exec = execution(LOG_EQUAL_ONLY_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![
        rule("ok-first", &[N3Builtin::LogEqualTo], 3),
        rule("bad-builtin", &[N3Builtin::MathProduct], 3),
    ];
    let err = executor.run(&rules);
    assert!(matches!(err, Err(Refusal::N3BuiltinRefused(_))), "{err:?}");
}

// ---- Zero direct actuation (PROJ-780, PRD §12 "PRD.md:608") ---------------

#[test]
fn n3_executor_refuses_rule_requesting_direct_actuation() {
    // Full whitelist and a generous cost bound: the refusal must come from
    // the dedicated direct-actuation check, not from the ordinary builtin
    // whitelist or the cost bound.
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule_requesting_actuation(
        "dispatches-http",
        &[],
        1,
        &[N3ActuationBuiltin::LogWebOperation],
    )];
    let err = executor.run(&rules);
    assert!(
        matches!(err, Err(Refusal::N3DirectActuationRefused(_))),
        "{err:?}"
    );
}

#[test]
fn n3_executor_direct_actuation_refusal_is_distinguishable_from_builtin_refusal() {
    // Same rule shape as an ordinary non-whitelisted-builtin refusal, except
    // it also declares a direct-actuation builtin: the variant returned must
    // be N3DirectActuationRefused, never N3BuiltinRefused, proving the two
    // refusal paths are genuinely distinct rather than folded together.
    let gates = n3_enabled_gates();
    let exec = execution(LOG_EQUAL_ONLY_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule_requesting_actuation(
        "mixed",
        &[N3Builtin::MathSum],
        1,
        &[N3ActuationBuiltin::LogSemantics],
    )];
    let err = executor.run(&rules);
    assert!(
        matches!(err, Err(Refusal::N3DirectActuationRefused(_))),
        "direct actuation must take priority over the ordinary builtin-whitelist refusal: {err:?}"
    );
}

#[test]
fn n3_executor_refuses_direct_actuation_before_checking_cost_bound() {
    // The rule's declared cost alone would blow the bound (999 > 5), but the
    // returned refusal must be N3DirectActuationRefused, not
    // N3CostBoundExceeded, proving the actuation check runs first and is
    // unconditional -- no execution profile value admits it.
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 5);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule_requesting_actuation(
        "actuates-and-over-cost",
        &[],
        999,
        &[N3ActuationBuiltin::OsProcess],
    )];
    let err = executor.run(&rules);
    assert!(
        matches!(err, Err(Refusal::N3DirectActuationRefused(_))),
        "{err:?}"
    );
}

#[test]
fn n3_executor_refuses_direct_actuation_before_any_later_rule_runs() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![
        rule_requesting_actuation(
            "actuates-first",
            &[],
            1,
            &[N3ActuationBuiltin::LogWebOperation],
        ),
        rule("never-reached", &[], 1),
    ];
    let err = executor.run(&rules);
    assert!(
        matches!(err, Err(Refusal::N3DirectActuationRefused(_))),
        "{err:?}"
    );
}

#[test]
fn n3_executor_refuses_every_recognized_direct_actuation_builtin() {
    // Every N3ActuationBuiltin variant independently triggers the refusal,
    // not just the first one checked in other tests.
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    for actuation_builtin in N3ActuationBuiltin::ALL {
        let rules = vec![rule_requesting_actuation(
            "single-actuation-builtin",
            &[],
            1,
            &[actuation_builtin],
        )];
        let err = executor.run(&rules);
        assert!(
            matches!(err, Err(Refusal::N3DirectActuationRefused(_))),
            "{actuation_builtin:?}: {err:?}"
        );
    }
}

#[test]
fn n3_executor_direct_actuation_refusal_is_deterministic_across_repeated_runs() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 100);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule_requesting_actuation(
        "dispatches-http",
        &[],
        1,
        &[N3ActuationBuiltin::LogWebOperation],
    )];
    for _ in 0..5 {
        let err = executor.run(&rules);
        assert!(
            matches!(err, Err(Refusal::N3DirectActuationRefused(_))),
            "{err:?}"
        );
    }
}

// ---- Determinism: same input + same bound -> same outcome, repeatedly ----

#[test]
fn n3_executor_admission_is_deterministic_across_repeated_runs() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 10);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("a", &[N3Builtin::MathSum], 4), rule("b", &[], 4)];
    let r1 = ok_or_unreachable_n3(executor.run(&rules));
    let r2 = ok_or_unreachable_n3(executor.run(&rules));
    assert_eq!(r1, r2);
    assert_eq!(r1.execution_hash, r2.execution_hash);
}

#[test]
fn n3_executor_refusal_is_deterministic_across_repeated_runs() {
    let gates = n3_enabled_gates();
    let exec = execution(ALL_BUILTINS_MASK, 3);
    let executor = N3Executor::new(&gates, &exec);
    let rules = vec![rule("too-big", &[], 4)];
    for _ in 0..5 {
        let err = executor.run(&rules);
        assert!(
            matches!(err, Err(Refusal::N3CostBoundExceeded(_))),
            "{err:?}"
        );
    }
}

fn ok_or_unreachable_n3(result: Result<N3ExecutionReceipt, Refusal>) -> N3ExecutionReceipt {
    match result {
        Ok(r) => r,
        Err(refusal) => unreachable!("expected Ok execution receipt: {refusal}"),
    }
}
