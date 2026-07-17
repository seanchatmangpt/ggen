//! Vision 2030 end-to-end integration tests.
//!
//! Covers the full LLM first-mile manufacturing loop:
//! domain text → admit → problem text → admit → manufacture_world → receipt → POWL tape.

use bcinr_pddl::powl_bridge::temporal_plan_to_powl_tape;
use bcinr_pddl::{admit_candidate_domain, manufacture_world};

const CLASSICAL_DOMAIN: &str = "(define (domain blocks) (:requirements :strips) (:predicates (on ?x ?y) (ontable ?x) (clear ?x) (holding ?x) (handempty)) (:action pick-up :parameters (?x) :precondition (and (clear ?x) (ontable ?x) (handempty)) :effect (and (holding ?x) (not (clear ?x)) (not (ontable ?x)) (not (handempty)))) (:action put-down :parameters (?x) :precondition (holding ?x) :effect (and (not (holding ?x)) (clear ?x) (ontable ?x) (handempty))) (:action stack :parameters (?x ?y) :precondition (and (holding ?x) (clear ?y)) :effect (and (not (holding ?x)) (not (clear ?y)) (clear ?x) (on ?x ?y) (handempty))) (:action unstack :parameters (?x ?y) :precondition (and (on ?x ?y) (clear ?x) (handempty)) :effect (and (holding ?x) (clear ?y) (not (on ?x ?y)) (not (clear ?x)) (not (handempty)))))";

const CLASSICAL_PROBLEM: &str = "(define (problem blocks-p1) (:domain blocks) (:objects a b) (:init (ontable a) (ontable b) (clear a) (clear b) (handempty)) (:goal (on a b)))";

#[test]
fn test_admit_classical_domain() {
    let admitted = admit_candidate_domain(CLASSICAL_DOMAIN).expect("domain admission failed");
    assert_eq!(admitted.domain31.name, "blocks");
    assert_eq!(admitted.witness.len(), 64);
}

#[test]
fn test_witness_determinism() {
    let a1 = admit_candidate_domain(CLASSICAL_DOMAIN).expect("first admission failed");
    let a2 = admit_candidate_domain(CLASSICAL_DOMAIN).expect("second admission failed");
    assert_eq!(a1.witness, a2.witness);
}

#[test]
fn test_manufacture_world_classical() {
    let receipt = manufacture_world(CLASSICAL_DOMAIN, CLASSICAL_PROBLEM, "test-001", &[]);
    assert!(
        receipt.admitted,
        "world must be admitted; refusal: {:?}",
        receipt.refusal_reason
    );
    assert!(
        !receipt.manufacture_chain.is_empty(),
        "manufacture_chain must not be empty"
    );
    assert!(receipt.plan_receipt.goal_reached, "goal must be reached");
}

#[test]
fn test_manufacture_world_refusal_malformed() {
    let receipt = manufacture_world("not pddl", "also not", "t", &[]);
    assert!(!receipt.admitted, "malformed input must be refused");
    assert!(
        receipt.refusal_reason.is_some(),
        "refusal_reason must be set"
    );
}

#[test]
fn test_manufacture_world_wrong_domain() {
    let receipt = manufacture_world(
        CLASSICAL_DOMAIN,
        "(define (problem p) (:domain wrong) (:objects a) (:init (clear a)) (:goal (clear a)))",
        "t",
        &[],
    );
    assert!(
        !receipt.admitted,
        "mismatched domain reference must be refused"
    );
}

#[test]
fn test_powl_bridge() {
    let receipt = manufacture_world(CLASSICAL_DOMAIN, CLASSICAL_PROBLEM, "test-powl-001", &[]);
    if receipt.admitted {
        let tape = temporal_plan_to_powl_tape(&receipt.plan)
            .expect("this fixture's plan is well within the 64-step tape cap");
        assert!(!tape.is_empty(), "POWL tape must not be empty");
    }
}
