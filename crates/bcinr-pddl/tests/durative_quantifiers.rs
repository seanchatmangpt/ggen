//! End-to-end proof that `PddlCondition::Forall` is genuinely evaluated by
//! `GroundTemporalProblem::find_temporal_plan` — real PDDL text, parsed by
//! the real parser, through the real grounder and forward-chaining search,
//! not a unit test calling `eval_condition` directly. This is the one
//! parser-reachable path a `Forall` precondition can currently take in this
//! crate: `:durative-action`'s `:condition` clause is lowered to a real
//! `PddlCondition::Forall` (`lower_da_gd` in `src/parse.rs`), which
//! `GroundTemporalProblem::find_temporal_plan_with_fn_overrides` genuinely
//! consults via `eval_condition` at every scheduling attempt. Before this
//! phase, `eval_condition`'s `Forall` arm was a hardcoded `true` regardless
//! of the body — this test's second case (`some_item_not_ready`) is exactly
//! the adversarial input that stub would have gotten wrong (it would have
//! scheduled `check-all` anyway).
//!
//! `Exists` has no equivalent end-to-end test here: the `pddl` crate's
//! durative-action-condition grammar only supports `forall`, not `exists`
//! (standard PDDL 3.1 `da-GD` — see `src/parse.rs`'s `lower_da_gd`, which has
//! no `Exists` arm because the grammar type has no such variant), and plain
//! `:action` preconditions / `:goal` can't carry a `PddlCondition::Forall`/
//! `Exists` through this crate's flat-`Pddl8Atom` STRIPS representation
//! either. `Exists`'s evaluator is unit-tested directly in
//! `crate::ground::quantifier_tests` instead (see that module's doc comment
//! for the full accounting) — this file only claims what it can prove
//! end-to-end.
use bcinr_pddl::{domain_from_pddl, problem_from_pddl, GroundTemporalProblem};

const DOMAIN: &str = r#"
(define (domain quant-durative)
  (:requirements :durative-actions :typing)
  (:types item)
  (:predicates (ready ?i - item) (all-ready))
  (:durative-action check-all
    :parameters ()
    :duration (= ?duration 1)
    :condition (and (at start (forall (?i - item) (ready ?i))))
    :effect (and (at end (all-ready)))))
"#;

#[test]
fn forall_precondition_admits_when_every_item_is_ready() {
    let domain = domain_from_pddl(DOMAIN).unwrap();
    let problem = problem_from_pddl(
        r#"(define (problem all-ready-p)
             (:domain quant-durative)
             (:objects a b - item)
             (:init (ready a) (ready b))
             (:goal (all-ready)))"#,
    )
    .unwrap();
    let gtp = GroundTemporalProblem::build(&domain, &problem).unwrap();
    let plan = gtp
        .find_temporal_plan()
        .into_result()
        .expect("forall (?i - item) (ready ?i) holds for {a, b}, so check-all should fire");
    assert_eq!(plan.steps.len(), 1);
    assert_eq!(plan.steps[0].action_name, "check-all");
}

#[test]
fn forall_precondition_blocks_when_one_item_is_not_ready() {
    let domain = domain_from_pddl(DOMAIN).unwrap();
    let problem = problem_from_pddl(
        r#"(define (problem one-not-ready-p)
             (:domain quant-durative)
             (:objects a b - item)
             (:init (ready a))
             (:goal (all-ready)))"#,
    )
    .unwrap();
    let gtp = GroundTemporalProblem::build(&domain, &problem).unwrap();
    let outcome = gtp.find_temporal_plan().into_result();
    assert!(
        outcome.is_err(),
        "b is not ready, so forall (?i - item) (ready ?i) must be false and check-all must \
         never fire — the pre-fix stub (Forall => true unconditionally) would have scheduled \
         it anyway and produced Ok(_) here, which is exactly the bug this phase fixed"
    );
}
