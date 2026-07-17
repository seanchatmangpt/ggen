use bcinr_pddl::ground::lazy::IndexedGroundProblem;
use bcinr_pddl::{domain_from_pddl, problem_from_pddl, GroundProblem};

#[test]
fn test_differential_grounding() {
    let domain_pddl = r#"
        (define (domain logistics)
            (:requirements :typing)
            (:types truck location package city)
            (:predicates
                (at ?obj - object ?loc - location)
                (in ?pkg - package ?veh - truck)
            )
            (:action load
                :parameters (?pkg - package ?veh - truck ?loc - location)
                :precondition (and (at ?pkg ?loc) (at ?veh ?loc))
                :effect (and (not (at ?pkg ?loc)) (in ?pkg ?veh))
            )
        )
    "#;
    let problem_pddl = r#"
        (define (problem log1)
            (:domain logistics)
            (:objects
                t1 t2 - truck
                l1 l2 - location
                p1 p2 - package
            )
            (:init
                (at t1 l1)
                (at t2 l2)
                (at p1 l2)
                (at p2 l1)
            )
            (:goal (and (in p1 t2) (in p2 t1)))
        )
    "#;

    let domain = domain_from_pddl(domain_pddl).unwrap();
    let problem = problem_from_pddl(problem_pddl).unwrap();

    let naive = GroundProblem::build(&domain, &problem, None).unwrap();
    let lazy = IndexedGroundProblem::build(&domain, &problem, None).unwrap();

    let naive_plan = naive.find_plan().into_result().unwrap();
    let lazy_plan = match lazy.find_plan() {
        bcinr_pddl::PlannerOutcome::Found(p) => p,
        _ => panic!("Expected Found"),
    };

    assert_eq!(
        naive_plan.ops.len(),
        lazy_plan.ops.len(),
        "Plans should have same length"
    );
}

// ---------------------------------------------------------------------------
// Regression: IndexedGroundProblem::find_plan must not conflate a
// depth-bound cutoff with a genuine proof of unreachability. See
// tests/depth_bound_exhaustion_conflation.rs for the sibling naive-grounder
// regression this mirrors (same fixture-generation approach, duplicated
// locally rather than shared, consistent with this crate's existing
// per-file test-fixture convention).
// ---------------------------------------------------------------------------

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
fn indexed_find_plan_reports_bounded_not_exhausted_for_depth_cutoff() {
    // PDDL8_MAX_PLAN_DEPTH is 64. A 70-step chain has a genuine plan
    // strictly longer than the depth bound, so every BFS branch is
    // depth-pruned before it can reach the goal. Before this fix,
    // IndexedGroundProblem::find_plan discarded depth-cut branches with no
    // flag set and unconditionally returned Exhausted{frontier_empty: true}
    // once the queue emptied -- a false proof of unreachability.
    let (domain_text, problem_text) = chain_domain_and_problem(70);
    let domain = domain_from_pddl(&domain_text).expect("domain must parse");
    let problem = problem_from_pddl(&problem_text).expect("problem must parse");
    let gp = IndexedGroundProblem::build(&domain, &problem, None).expect("must ground");

    match gp.find_plan() {
        bcinr_pddl::PlannerOutcome::Bounded(b) => {
            use bcinr_mfw_ir::BoundKind;
            assert_eq!(
                b.kind,
                BoundKind::PlanDepth,
                "must cite the depth bound as the cause"
            );
            assert_eq!(b.limit, 64, "PDDL8_MAX_PLAN_DEPTH is 64");
            assert!(
                b.observed > b.limit,
                "observed depth ({}) must exceed the limit ({}) -- that's why it's Bounded",
                b.observed,
                b.limit
            );
        }
        bcinr_pddl::PlannerOutcome::Exhausted(w) => {
            panic!(
                "IndexedGroundProblem::find_plan claimed Exhausted (a proof of \
                 unreachability) for a depth-bound cutoff -- the genuine 70-step \
                 plan for this domain was never explored because every branch was \
                 pruned at the 64-step cap. witness={w:?}"
            );
        }
        other => panic!("unexpected outcome: {other:?}"),
    }
}

#[test]
fn indexed_genuinely_unreachable_goal_within_depth_bound_still_reports_exhausted() {
    // The fix must not turn every non-Found outcome into Bounded regardless
    // of cause: a goal that is genuinely unreachable and well within the
    // depth bound must still report a real Exhausted proof.
    let domain_text = "(define (domain chain) (:predicates (p0) (p1) (pdead)) \
         (:action step0 :parameters () :precondition (p0) \
          :effect (and (p1) (not (p0)))))";
    let problem_text = "(define (problem chain-p) (:domain chain) (:init (p0)) (:goal (pdead)))";
    let domain = domain_from_pddl(domain_text).expect("domain must parse");
    let problem = problem_from_pddl(problem_text).expect("problem must parse");
    let gp = IndexedGroundProblem::build(&domain, &problem, None).expect("must ground");

    match gp.find_plan() {
        bcinr_pddl::PlannerOutcome::Exhausted(w) => {
            assert!(
                w.frontier_empty,
                "the whole reachable space was actually visited"
            );
        }
        other => panic!(
            "expected a genuine Exhausted proof for an unreachable goal well within \
             the depth bound, got {other:?}"
        ),
    }
}
