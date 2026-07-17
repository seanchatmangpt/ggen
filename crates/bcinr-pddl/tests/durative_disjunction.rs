//! End-to-end proof that `PddlCondition::Or` is genuinely evaluated by
//! `GroundTemporalProblem::find_temporal_plan` — real PDDL text, parsed by
//! the real parser, through the real grounder and forward-chaining search,
//! not a unit test calling `eval_condition` directly.
//!
//! Before this test existed, `capability.rs` rated
//! [`PddlFeature::Disjunction`](bcinr_pddl::PddlFeature) `Exact` on the
//! strength of code inspection alone (`eval_condition`'s `Or` arm being
//! "equally straightforward" to the already-tested `Not` arm) with no
//! passing test anywhere in this crate that ever constructs or evaluates a
//! `PddlCondition::Or` — unlike every sibling `Exact`/`Approximate` entry in
//! that module's doc comment, which cites one. This file closes that gap the
//! same way `durative_quantifiers.rs` closes it for `Forall`: a
//! `:durative-action`'s `:condition` clause can carry a real `(or ...)` term
//! nested inside `(at start ...)` — `TimedGoalDefinition::At`'s inner
//! `GoalDefinition` is the general grammar (`Or` included, requires
//! `:disjunctive-preconditions`, see the `pddl` crate's `GoalDefinition`),
//! lowered by `lower_condition` in `src/parse.rs` into a real
//! `PddlCondition::Or`, which `GroundTemporalProblem::find_temporal_plan`
//! genuinely consults via `eval_condition` at every scheduling attempt. This
//! is the same durative-condition pipeline already proven live for
//! `Not`/`Compare`/`Timed` (`capability_router`'s tests, this crate's
//! `tests/semantic_falsifier.rs`), not a new one.
//!
//! `:action`/`:goal` preconditions can't carry a `PddlCondition::Or` at all
//! in this crate (both are flattened to `Vec<Pddl8Atom>` — see
//! `capability.rs`'s module doc for `ExistentialPreconditions`, which
//! documents the identical structural limit for `Exists`), so, like
//! `UniversalPreconditions`, this file only claims what it can prove
//! end-to-end through the one parser-reachable path that exists.
use bcinr_pddl::{domain_from_pddl, problem_from_pddl, GroundTemporalProblem};

const DOMAIN: &str = r#"
(define (domain disj-durative)
  (:requirements :durative-actions :typing :disjunctive-preconditions)
  (:predicates (ready-a) (ready-b) (done))
  (:durative-action check-either
    :parameters ()
    :duration (= ?duration 1)
    :condition (and (at start (or (ready-a) (ready-b))))
    :effect (and (at end (done)))))
"#;

#[test]
fn or_precondition_admits_when_only_the_second_disjunct_holds() {
    let domain = domain_from_pddl(DOMAIN).unwrap();
    // Adversarial: only `ready-b` holds, not `ready-a`. A broken `Or` arm
    // that (e.g.) checked only the first disjunct, or that degenerated to
    // `And`'s `.all(...)`, would refuse to fire `check-either` here.
    let problem = problem_from_pddl(
        r#"(define (problem only-b-ready-p)
             (:domain disj-durative)
             (:init (ready-b))
             (:goal (done)))"#,
    )
    .unwrap();
    let gtp = GroundTemporalProblem::build(&domain, &problem).unwrap();
    let plan = gtp.find_temporal_plan().into_result().expect(
        "(or (ready-a) (ready-b)) holds because ready-b is true, so check-either \
                 should fire even though ready-a never does",
    );
    assert_eq!(plan.steps.len(), 1);
    assert_eq!(plan.steps[0].action_name, "check-either");
}

#[test]
fn or_precondition_blocks_when_neither_disjunct_holds() {
    let domain = domain_from_pddl(DOMAIN).unwrap();
    let problem = problem_from_pddl(
        r#"(define (problem neither-ready-p)
             (:domain disj-durative)
             (:init )
             (:goal (done)))"#,
    )
    .unwrap();
    let gtp = GroundTemporalProblem::build(&domain, &problem).unwrap();
    let outcome = gtp.find_temporal_plan().into_result();
    assert!(
        outcome.is_err(),
        "neither ready-a nor ready-b holds, so (or (ready-a) (ready-b)) must be false and \
         check-either must never fire — a stub `Or` arm that degenerated to `true` \
         unconditionally (mirroring the pre-fix `Forall` bug this crate already documents) \
         would have scheduled it anyway and produced Ok(_) here"
    );
}
