//! Adversarial verification: does `GroundProblem::find_plan` (the classical
//! BFS `ExactBfsRail` wraps) distinguish a genuine proof of unreachability
//! from merely hitting `PDDL8_MAX_PLAN_DEPTH`?
//!
//! `find_temporal_plan_with_fn_overrides` (same file, `ground/mod.rs`) was
//! explicitly patched during the MFW retrofit to separate these two cases
//! (see its own doc comment: "the old code conflated both into the same
//! unit `Exhausted` variant, which silently claimed 'search exhausted its
//! frontier' even when the loop was cut off mid-progress"). `find_plan` now
//! carries the equivalent fix: it tracks whether any BFS branch was
//! discarded solely for exceeding the depth cap and, if so, reports
//! `Bounded` instead of `Exhausted`.
//!
//! Fixture: a domain with a strictly linear 70-step chain
//! `p0 -> p1 -> ... -> p70`, one action per step, goal `p70`. A plan
//! genuinely exists (exactly 70 actions), but `PDDL8_MAX_PLAN_DEPTH == 64`
//! (see `wasm4pm_compat::pddl::PDDL8_MAX_PLAN_DEPTH`), so every BFS branch
//! gets silently pruned (`if path.len() > PDDL8_MAX_PLAN_DEPTH { continue }`)
//! before it can reach the goal.

use bcinr_mfw_ir::PlannerOutcome;
use bcinr_pddl::ground::GroundProblem;
use bcinr_pddl::parse::{domain_from_pddl, problem_from_pddl};

fn chain_domain_and_problem(n: usize) -> (String, String) {
    let preds: String = (0..=n)
        .map(|i| format!("(p{i})"))
        .collect::<Vec<_>>()
        .join(" ");
    let actions: String = (0..n)
        .map(|i| {
            format!(
                "(:action step{i} :parameters () :precondition (p{i}) :effect (and (p{next} ) (not (p{i}))))",
                i = i,
                next = i + 1
            )
        })
        .collect::<Vec<_>>()
        .join(" ");
    let domain = format!("(define (domain chain) (:predicates {preds}) {actions})");
    let problem = format!("(define (problem chain-p) (:domain chain) (:init (p0)) (:goal (p{n})))");
    (domain, problem)
}

#[test]
fn find_plan_reports_bounded_not_exhausted_for_depth_cutoff() {
    // PDDL8_MAX_PLAN_DEPTH is 64 (wasm4pm_compat::pddl). A 70-step chain has
    // a genuine plan that is strictly longer than the depth bound.
    let (domain_text, problem_text) = chain_domain_and_problem(70);
    let domain = domain_from_pddl(&domain_text).expect("domain must parse");
    let problem = problem_from_pddl(&problem_text).expect("problem must parse");
    let gp = GroundProblem::build(&domain, &problem, None).expect("must ground");

    let outcome = gp.find_plan();

    // `find_plan` must not claim a proof of unreachability (`Exhausted`)
    // when the only reason it didn't find the genuine 70-step plan is that
    // every BFS branch got depth-pruned before reaching the goal. It must
    // report `Bounded` instead, exactly as the sibling
    // `find_temporal_plan_with_fn_overrides` (same file) already does for
    // the identical situation.
    match outcome {
        PlannerOutcome::Bounded(b) => {
            use bcinr_mfw_ir::BoundKind;
            assert_eq!(
                b.kind,
                BoundKind::PlanDepth,
                "must cite the depth bound as the cause"
            );
            assert_eq!(b.limit, 64, "PDDL8_MAX_PLAN_DEPTH is 64");
            assert!(
                b.observed > b.limit,
                "observed depth ({}) must exceed the limit ({}) — that's why it's Bounded",
                b.observed,
                b.limit
            );
        }
        PlannerOutcome::Exhausted(w) => {
            panic!(
                "find_plan claimed Exhausted (a proof of unreachability) for a \
                 depth-bound cutoff — the genuine 70-step plan for this domain \
                 was never explored because every branch was pruned at the 64-step \
                 cap. witness={w:?}"
            );
        }
        other => panic!("unexpected outcome: {other:?}"),
    }
}

/// Sanity check: confirm the 70-step chain genuinely has a reachable goal
/// when the depth bound is not in the way (i.e. this is not a bad fixture
/// that's actually unreachable for some unrelated reason). We can't easily
/// raise PDDL8_MAX_PLAN_DEPTH, so instead we verify a *shorter* chain
/// (well within the depth bound) is solved correctly by the exact same
/// domain-generation logic, establishing the domain shape itself is sound.
#[test]
fn shorter_chain_well_within_depth_bound_is_genuinely_found() {
    let (domain_text, problem_text) = chain_domain_and_problem(5);
    let domain = domain_from_pddl(&domain_text).expect("domain must parse");
    let problem = problem_from_pddl(&problem_text).expect("problem must parse");
    let gp = GroundProblem::build(&domain, &problem, None).expect("must ground");

    let outcome = gp.find_plan();
    match outcome {
        PlannerOutcome::Found(tape) => {
            assert_eq!(tape.ops.len(), 5, "must use exactly the 5-step chain");
        }
        other => panic!("expected Found for a 5-step chain, got {other:?}"),
    }
}

fn chain_domain_with_unreachable_predicate(n: usize) -> String {
    let preds: String = (0..=n)
        .map(|i| format!("(p{i})"))
        .chain(std::iter::once("(pdead)".to_string()))
        .collect::<Vec<_>>()
        .join(" ");
    let actions: String = (0..n)
        .map(|i| {
            format!(
                "(:action step{i} :parameters () :precondition (p{i}) :effect (and (p{next} ) (not (p{i}))))",
                i = i,
                next = i + 1
            )
        })
        .collect::<Vec<_>>()
        .join(" ");
    format!("(define (domain chain) (:predicates {preds}) {actions})")
}

/// Regression guard for the fix itself: a goal that is genuinely
/// unreachable (no action ever produces `pX` for an unused predicate name)
/// and well within the depth bound must still report `Exhausted`, not
/// `Bounded` — the fix must not turn every non-`Found` outcome into
/// `Bounded` regardless of cause.
#[test]
fn genuinely_unreachable_goal_within_depth_bound_still_reports_exhausted() {
    // `pdead` is declared but no action ever asserts it — truly
    // unreachable, and the whole (tiny) state space is well within
    // PDDL8_MAX_PLAN_DEPTH, so this must remain a genuine `Exhausted` proof.
    let domain_text = chain_domain_with_unreachable_predicate(5);
    let problem_text = "(define (problem chain-p) (:domain chain) (:init (p0)) (:goal (pdead)))";
    let domain = domain_from_pddl(&domain_text).expect("domain must parse");
    let problem = problem_from_pddl(problem_text).expect("problem must parse");
    let gp = GroundProblem::build(&domain, &problem, None).expect("must ground");

    let outcome = gp.find_plan();
    match outcome {
        PlannerOutcome::Exhausted(w) => {
            assert!(
                w.frontier_empty,
                "the whole reachable space was actually visited"
            );
        }
        other => panic!(
            "expected a genuine Exhausted proof for an unreachable goal well \
             within the depth bound, got {other:?}"
        ),
    }
}
