//! Regression test: the STRIPS8 grounder must restrict bindings to
//! type-compatible objects instead of cross-producting every object against
//! every parameter slot regardless of declared `:types`.

use bcinr_pddl::{domain_from_pddl, problem_from_pddl, GroundProblem};

const DOMAIN: &str = r#"
(define (domain typed-logistics)
  (:requirements :strips :typing)
  (:types package truck location)
  (:predicates
    (at ?x ?y)
    (in ?x ?y))
  (:action load-truck
    :parameters (?pkg - package ?truck - truck ?loc - location)
    :precondition (and (at ?pkg ?loc) (at ?truck ?loc))
    :effect (and (in ?pkg ?truck) (not (at ?pkg ?loc))))
  (:action drive-truck
    :parameters (?truck - truck ?from - location ?to - location)
    :precondition (at ?truck ?from)
    :effect (and (at ?truck ?to) (not (at ?truck ?from))))
  (:action unload-truck
    :parameters (?pkg - package ?truck - truck ?loc - location)
    :precondition (and (in ?pkg ?truck) (at ?truck ?loc))
    :effect (and (at ?pkg ?loc) (not (in ?pkg ?truck))))
)
"#;

const PROBLEM: &str = r#"
(define (problem typed-get-pkgs)
  (:domain typed-logistics)
  (:objects
    pkg1 pkg2 - package
    truck1 truck2 - truck
    loc_a loc_b - location)
  (:init
    (at pkg1 loc_a) (at pkg2 loc_a)
    (at truck1 loc_a) (at truck2 loc_a))
  (:goal (and (at pkg1 loc_b) (at pkg2 loc_b)))
)
"#;

#[test]
fn typed_grounding_restricts_to_type_compatible_bindings() {
    let domain = domain_from_pddl(DOMAIN).expect("domain parse");
    let problem = problem_from_pddl(PROBLEM).expect("problem parse");

    // Sanity: the parser actually captured object types — if this is empty,
    // the test below would pass vacuously (falling back to untyped behavior).
    assert_eq!(problem.object_types.len(), 6, "expected 6 typed objects");

    let ground = GroundProblem::build(&domain, &problem, None).expect("grounding");

    // 2 packages x 2 trucks x 2 locations = 8 per 3-typed-param schema.
    // load-truck: 8, unload-truck: 8, drive-truck (truck x location x location): 2*2*2=8.
    // Total = 24, versus an untyped cross-product of 6^3 * 3 = 648.
    assert_eq!(
        ground.actions.len(),
        24,
        "type-restricted grounding should produce exactly 24 ground actions, got {}",
        ground.actions.len()
    );

    // No ground action may bind a truck object where a package was required,
    // or vice versa — spot-check via the synthesized label's argument list.
    for action in &ground.actions {
        if action.schema_name == "load-truck" || action.schema_name == "unload-truck" {
            let args: Vec<&str> = action
                .label
                .split('(')
                .nth(1)
                .unwrap()
                .trim_end_matches(')')
                .split(',')
                .collect();
            assert!(
                args[0].starts_with("pkg"),
                "first arg of {} must be a package, got {}",
                action.schema_name,
                args[0]
            );
            assert!(
                args[1].starts_with("truck"),
                "second arg of {} must be a truck, got {}",
                action.schema_name,
                args[1]
            );
            assert!(
                args[2].starts_with("loc"),
                "third arg of {} must be a location, got {}",
                action.schema_name,
                args[2]
            );
        }
    }

    // A plan should still be findable — type filtering must not break search.
    let plan = ground.find_plan().into_result().expect("plan found");
    assert!(!plan.is_empty());
}
