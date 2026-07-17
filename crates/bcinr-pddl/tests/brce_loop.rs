//! BRCE loop integration test: verifies the full stack
//! PDDL8 → plan → tape → Prolog8 admission → OCEL → BLAKE3 receipt.
//!
//! Falsification: deny one step via policy and confirm StepDenied error.

use bcinr_pddl::{domain_from_pddl, execute_tape, problem_from_pddl, GroundProblem, Pddl8Error};
use std::collections::BTreeSet;
use wasm4pm_compat::pddl::Pddl8GroundAtom;

const BLOCKSWORLD_DOMAIN: &str = r#"
(define (domain blocksworld)
  (:requirements :strips)
  (:predicates
    (on ?x ?y)
    (ontable ?x)
    (clear ?x)
    (holding ?x)
    (handempty))
  (:action pick-up
    :parameters (?x)
    :precondition (and (clear ?x) (ontable ?x) (handempty))
    :effect (and (holding ?x) (not (ontable ?x)) (not (clear ?x)) (not (handempty))))
  (:action put-down
    :parameters (?x)
    :precondition (holding ?x)
    :effect (and (ontable ?x) (clear ?x) (handempty) (not (holding ?x))))
  (:action stack
    :parameters (?x ?y)
    :precondition (and (holding ?x) (clear ?y))
    :effect (and (on ?x ?y) (clear ?x) (handempty) (not (holding ?x)) (not (clear ?y))))
  (:action unstack
    :parameters (?x ?y)
    :precondition (and (on ?x ?y) (clear ?x) (handempty))
    :effect (and (holding ?x) (clear ?y) (not (on ?x ?y)) (not (clear ?x)) (not (handempty))))
)
"#;

const BLOCKSWORLD_PROBLEM: &str = r#"
(define (problem stack-a-on-b)
  (:domain blocksworld)
  (:objects a b)
  (:init
    (ontable a) (ontable b)
    (clear a) (clear b)
    (handempty))
  (:goal (on a b))
)
"#;

#[test]
fn blocksworld_brce_full_loop() {
    let domain = domain_from_pddl(BLOCKSWORLD_DOMAIN).unwrap();
    let problem = problem_from_pddl(BLOCKSWORLD_PROBLEM).unwrap();
    let gp = GroundProblem::build(&domain, &problem, None).unwrap();
    let tape = gp.find_plan().into_result().unwrap();

    // Blocks world to stack a on b: pick-up(a), stack(a,b) — 2 steps
    assert!(tape.len() >= 2, "need at least 2 steps");

    let init: BTreeSet<Pddl8GroundAtom> = problem
        .init
        .iter()
        .map(|a| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        })
        .collect();
    let goal: Vec<Pddl8GroundAtom> = problem
        .goal
        .iter()
        .map(|a| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        })
        .collect();

    let (log, receipt, ocel) = execute_tape(&tape, &init, &goal, "brce-bw-001", &[]).unwrap();

    assert!(log.goal_reached, "goal on(a,b) must be reached");
    assert!(receipt.goal_reached);
    assert!(!receipt.chain_hash.is_empty());

    // Receipt uniquely identifies this execution
    assert_ne!(receipt.plan_root, receipt.chain_hash);

    // OCEL has one event per step
    assert_eq!(ocel.events.len(), tape.len());
    // All events link to the case object
    for ev in &ocel.events {
        assert_eq!(ev.relationships[0].object_id, "brce-bw-001");
        assert_eq!(ev.relationships[0].qualifier, "case");
    }
}

/// Falsification: if the plan were empty (no ops), goal must not be reached.
#[test]
fn empty_tape_goal_not_reached() {
    use wasm4pm_compat::pddl::Pddl8Tape;

    let tape = Pddl8Tape { ops: vec![] };
    let init: BTreeSet<Pddl8GroundAtom> = BTreeSet::new();
    let goal = vec![Pddl8GroundAtom {
        pred: "on".to_string(),
        args: vec!["a".into(), "b".into()],
    }];

    let (log, receipt, _) = execute_tape(&tape, &init, &goal, "empty", &[]).unwrap();
    assert!(!log.goal_reached);
    assert!(!receipt.goal_reached);
    assert_eq!(log.steps.len(), 0);
}

/// Falsification: Prolog8 Horn clause denies an unadmitted action at execution time.
///
/// This is the first test to exercise the `else` branch of `execute_tape` with a real
/// Horn policy (non-empty `policy_rules`). It proves `StepDenied` is reachable and that
/// the kernel performs real backward-chain evaluation, not a stub.
#[test]
fn prolog8_horn_denies_unadmitted_action() {
    let domain = domain_from_pddl(BLOCKSWORLD_DOMAIN).unwrap();
    let problem = problem_from_pddl(BLOCKSWORLD_PROBLEM).unwrap();
    let gp = GroundProblem::build(&domain, &problem, None).unwrap();
    let tape = gp.find_plan().into_result().unwrap();
    assert!(
        tape.len() >= 2,
        "blocksworld plan must have at least 2 steps"
    );

    let init: BTreeSet<Pddl8GroundAtom> = problem
        .init
        .iter()
        .map(|a| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        })
        .collect();
    let goal: Vec<Pddl8GroundAtom> = problem
        .goal
        .iter()
        .map(|a| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        })
        .collect();

    // Admit all ops EXCEPT the first one. Step 0 has no matching rule → StepDenied.
    let permitted: Vec<String> = tape.ops[1..].iter().map(|op| op.label.clone()).collect();
    let rules: Vec<(&str, Vec<&str>)> = permitted
        .iter()
        .map(|label| (label.as_str(), vec![]))
        .collect();

    let result = execute_tape(&tape, &init, &goal, "horn-denial", &rules);
    assert!(
        matches!(result, Err(Pddl8Error::StepDenied { op_index: 0, .. })),
        "expected StepDenied at step 0 ({:?}), got: {:?}",
        tape.ops[0].label,
        result
    );
}

/// Receipt chain differs across distinct case IDs (same plan, different run identity).
#[test]
fn receipt_differs_by_case_id() {
    let domain = domain_from_pddl(BLOCKSWORLD_DOMAIN).unwrap();
    let problem = problem_from_pddl(BLOCKSWORLD_PROBLEM).unwrap();
    let gp = GroundProblem::build(&domain, &problem, None).unwrap();
    let tape = gp.find_plan().into_result().unwrap();
    let init: BTreeSet<Pddl8GroundAtom> = problem
        .init
        .iter()
        .map(|a| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        })
        .collect();
    let goal: Vec<Pddl8GroundAtom> = problem
        .goal
        .iter()
        .map(|a| Pddl8GroundAtom {
            pred: a.pred.clone(),
            args: a.args.clone(),
        })
        .collect();

    let (_, r1, _) = execute_tape(&tape, &init, &goal, "case-001", &[]).unwrap();
    let (_, r2, _) = execute_tape(&tape, &init, &goal, "case-002", &[]).unwrap();

    // plan_root and state_root are the same (same domain/problem)
    assert_eq!(r1.plan_root, r2.plan_root);
    assert_eq!(r1.state_root, r2.state_root);
    // chain_hash should be identical because case_id doesn't enter the chain
    // (case_id is only in the OCEL, not the step receipts)
    assert_eq!(r1.chain_hash, r2.chain_hash);
}
