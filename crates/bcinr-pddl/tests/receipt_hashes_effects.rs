//! Regression tests for FINDING #5: BLAKE3 execution-chain receipts
//! (`execute_tape`, `execute_temporal_plan`, `execute_temporal_plan_instrumented`)
//! previously hashed only op labels/action names/timing — never the actual
//! add/del effect content — so two tapes/plans with identical op
//! labels/names but materially different effects produced byte-identical
//! `plan_root`/`chain_hash`. A consumer verifying the receipt therefore
//! could not detect a mis-grounded or tampered tape/plan.
//!
//! Each test below constructs an adversarial pair that is identical in
//! every field the *old* hash inputs covered (label/index/action_name/args/
//! timing) but differs only in effect content, and asserts the receipts
//! now differ.

use bcinr_pddl::execute::{execute_temporal_plan, execute_temporal_plan_instrumented};
use bcinr_pddl::{domain_from_pddl, execute_tape, problem_from_pddl};
use std::collections::BTreeSet;
use wasm4pm_compat::pddl::{
    Pddl8GroundAction, Pddl8GroundAtom, Pddl8Tape, Pddl8TapeOp, TemporalPlan, TemporalPlanStep,
};

/// Build a single-op tape labeled "pick-up(a)" whose add_effects add
/// `holding(<add_arg>)` — the label/index are held fixed across calls so
/// only the effect content varies.
fn tape_adding(add_arg: &str) -> Pddl8Tape {
    let action = Pddl8GroundAction {
        schema_name: "pick-up".to_string(),
        label: "pick-up(a)".to_string(),
        preconditions: vec![],
        add_effects: vec![Pddl8GroundAtom {
            pred: "holding".to_string(),
            args: vec![add_arg.to_string()],
        }],
        del_effects: vec![],
    };
    Pddl8Tape {
        ops: vec![Pddl8TapeOp {
            index: 0,
            label: "pick-up(a)".to_string(),
            pred_mask: 0,
            action,
        }],
    }
}

/// A tampered/mis-grounded tape (same op label + index as `tape_adding`)
/// must not chain-hash identically to the original when its ground-action
/// effects differ. This is the classical (`execute_tape`) half of
/// FINDING #5.
#[test]
fn execute_tape_receipt_detects_tampered_effects_with_identical_labels() {
    let tape_a = tape_adding("a");
    let tape_b = tape_adding("b");

    // Sanity: the two tapes really are identical in every field the old
    // hash inputs covered.
    assert_eq!(tape_a.ops[0].label, tape_b.ops[0].label);
    assert_eq!(tape_a.ops[0].index, tape_b.ops[0].index);
    assert_ne!(
        tape_a.ops[0].action.add_effects, tape_b.ops[0].action.add_effects,
        "sanity: the two tapes must actually differ in effect content"
    );

    let init: BTreeSet<Pddl8GroundAtom> = BTreeSet::new();
    // Trivial (empty) goal: goal_reached is true regardless of which
    // add_effect fired, isolating the assertion to plan_root/chain_hash
    // rather than the (already-correct) goal-verdict byte.
    let goal: Vec<Pddl8GroundAtom> = vec![];

    let (_log_a, receipt_a, _ocel_a) =
        execute_tape(&tape_a, &init, &goal, "tamper-a", &[]).expect("tape_a executes");
    let (_log_b, receipt_b, _ocel_b) =
        execute_tape(&tape_b, &init, &goal, "tamper-b", &[]).expect("tape_b executes");

    assert!(receipt_a.goal_reached);
    assert!(receipt_b.goal_reached);
    assert_ne!(
        receipt_a.plan_root, receipt_b.plan_root,
        "plan_root must differ when ground-action effects differ, even with identical op labels/index"
    );
    assert_ne!(
        receipt_a.chain_hash, receipt_b.chain_hash,
        "chain_hash must differ when ground-action effects differ, even with identical op labels/index"
    );
}

const TEMPORAL_PROBLEM: &str = r#"(define (problem p)
  (:domain d)
  (:init (ontable a))
  (:goal (and)))"#;

/// Two domains sharing the same action name/params/precondition-predicate
/// but whose `pick-up` schema adds a *different* atom (`holding` vs
/// `grabbed`). A `TemporalPlan` naming "pick-up(a)" is byte-identical
/// between the two calls (same action_name/args/start_time/duration) —
/// only the domain-side effect differs.
fn domain_with_add_effect(add_pred: &str) -> wasm4pm_compat::pddl::Pddl8Domain {
    let text = format!(
        r#"(define (domain d)
  (:requirements :strips)
  (:predicates (holding ?x) (grabbed ?x) (ontable ?x))
  (:action pick-up
    :parameters (?x)
    :precondition (ontable ?x)
    :effect (and ({add_pred} ?x) (not (ontable ?x)))))"#
    );
    domain_from_pddl(&text).expect("domain parses")
}

fn one_step_plan() -> TemporalPlan {
    TemporalPlan {
        steps: vec![TemporalPlanStep {
            start_time: 0.0,
            duration: 1.0,
            action_name: "pick-up(a)".to_string(),
            args: vec!["a".to_string()],
        }],
        makespan: 1.0,
        metric_value: None,
    }
}

/// `execute_temporal_plan` half of FINDING #5: identical plan (same
/// action_name/args/timing) replayed against two domains whose same-named
/// schema has different effects must not produce identical receipts.
#[test]
fn execute_temporal_plan_receipt_detects_domain_effect_swap_with_identical_plan() {
    let domain_a = domain_with_add_effect("holding");
    let domain_b = domain_with_add_effect("grabbed");
    let problem = problem_from_pddl(TEMPORAL_PROBLEM).expect("problem parses");
    let plan = one_step_plan();

    let (receipt_a, _ocel_a) =
        execute_temporal_plan(&plan, &domain_a, &problem, "case-a", &[]).expect("plan admits (a)");
    let (receipt_b, _ocel_b) =
        execute_temporal_plan(&plan, &domain_b, &problem, "case-b", &[]).expect("plan admits (b)");

    // Trivial (:goal (and)) goal: goal_reached is true regardless of which
    // domain fired, isolating the assertion to plan_root/chain_hash.
    assert!(receipt_a.goal_reached);
    assert!(receipt_b.goal_reached);
    assert_ne!(
        receipt_a.plan_root, receipt_b.plan_root,
        "plan_root must differ when the domain schema's effects differ, even with an identical plan"
    );
    assert_ne!(
        receipt_a.chain_hash, receipt_b.chain_hash,
        "chain_hash must differ when the domain schema's effects differ, even with an identical plan"
    );
}

/// Same adversarial pair, exercised through the bench-only instrumented
/// variant (`execute_temporal_plan_instrumented`) — it duplicates
/// `execute_temporal_plan`'s hashing logic and must not regress
/// independently.
#[test]
fn execute_temporal_plan_instrumented_receipt_detects_domain_effect_swap() {
    let domain_a = domain_with_add_effect("holding");
    let domain_b = domain_with_add_effect("grabbed");
    let problem = problem_from_pddl(TEMPORAL_PROBLEM).expect("problem parses");
    let plan = one_step_plan();

    let (receipt_a, _ocel_a, _substage_a) =
        execute_temporal_plan_instrumented(&plan, &domain_a, &problem, "case-a", &[])
            .expect("plan admits (a)");
    let (receipt_b, _ocel_b, _substage_b) =
        execute_temporal_plan_instrumented(&plan, &domain_b, &problem, "case-b", &[])
            .expect("plan admits (b)");

    assert!(receipt_a.goal_reached);
    assert!(receipt_b.goal_reached);
    assert_ne!(
        receipt_a.plan_root, receipt_b.plan_root,
        "instrumented plan_root must differ when the domain schema's effects differ"
    );
    assert_ne!(
        receipt_a.chain_hash, receipt_b.chain_hash,
        "instrumented chain_hash must differ when the domain schema's effects differ"
    );
}
